# =============================================================================
# TECHNICAL VALIDATION WORKFLOW
#
# Purpose:
#   This script performs technical validation of the Dengue–Rainfall dataset.
#   It is intended for data quality assurance, structural validation, and
#   reproducibility of dataset generation, not for inferential or predictive
#   analysis.
#
# Validation domains:
#   1. File loading and schema verification
#   2. Required column checks
#   3. Data type harmonization
#   4. Missingness assessment
#   5. Duplicate record detection
#   6. Temporal ordering and continuity checks
#   7. Value-domain checks (non-negative counts/rainfall)
#   8. Geographic key consistency
#   9. Cross-scale aggregation checks
#   10. Output reproducibility reports
#
# Notes:
#   - This script avoids correlation analysis, regression, lag modeling,
#     significance testing, and interpretation-heavy outputs.
#   - All outputs are descriptive quality-control artifacts only.
#
# =============================================================================

# =============================================================================
# 0. PACKAGE SETUP
# =============================================================================
needed <- c("readxl", "dplyr", "tidyr", "stringr", "purrr", "writexl")

for (pkg in needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(writexl)

# =============================================================================
# 1. USER INPUT
# =============================================================================
PATH <- "C:/Users/User/Downloads/Dengue-Rainfall_Dataset.xlsx"

# Output directory
out_dir <- "technical_validation_outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 2. HELPER FUNCTIONS
# =============================================================================
section_header <- function(title) {
  cat("\n", strrep("=", 78), "\n", title, "\n", strrep("=", 78), "\n", sep = "")
}

safe_n_distinct <- function(x) {
  dplyr::n_distinct(x, na.rm = TRUE)
}

safe_min <- function(x) {
  if (all(is.na(x))) return(NA)
  min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) return(NA)
  max(x, na.rm = TRUE)
}

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA)
  mean(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  if (all(is.na(x))) return(NA)
  sd(x, na.rm = TRUE)
}

missing_summary <- function(df, dataset_name) {
  data.frame(
    Dataset = dataset_name,
    Variable = names(df),
    Missing_n = sapply(df, function(x) sum(is.na(x))),
    Missing_pct = round(sapply(df, function(x) mean(is.na(x)) * 100), 2),
    stringsAsFactors = FALSE
  )
}

duplicate_summary <- function(df, keys, dataset_name) {
  dup_df <- df %>%
    count(across(all_of(keys)), name = "n_records") %>%
    filter(n_records > 1)
  
  data.frame(
    Dataset = dataset_name,
    Key = paste(keys, collapse = " + "),
    Duplicate_key_groups = nrow(dup_df),
    Duplicate_rows_excess = ifelse(nrow(dup_df) == 0, 0, sum(dup_df$n_records - 1)),
    stringsAsFactors = FALSE
  )
}

continuity_check <- function(df, group_vars, year_var, week_var, dataset_name) {
  year_sym <- rlang::sym(year_var)
  week_sym <- rlang::sym(week_var)
  
  out <- df %>%
    mutate(
      .YEAR = as.integer(!!year_sym),
      .WEEK = as.integer(!!week_sym),
      .YW = .YEAR * 100 + .WEEK
    ) %>%
    arrange(across(all_of(group_vars)), .YEAR, .WEEK) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n_rows = n(),
      min_year = safe_min(.YEAR),
      max_year = safe_max(.YEAR),
      min_week = safe_min(.WEEK),
      max_week = safe_max(.WEEK),
      duplicate_year_week = n() - n_distinct(.YW),
      missing_year = sum(is.na(.YEAR)),
      missing_week = sum(is.na(.WEEK)),
      .groups = "drop"
    ) %>%
    mutate(Dataset = dataset_name, .before = 1)
  
  out
}

range_check_numeric <- function(df, dataset_name, var_names, min_allowed = 0) {
  bind_rows(lapply(var_names, function(v) {
    x <- df[[v]]
    data.frame(
      Dataset = dataset_name,
      Variable = v,
      Min = safe_min(x),
      Max = safe_max(x),
      Mean = round(safe_mean(x), 3),
      SD = round(safe_sd(x), 3),
      Negative_values = sum(!is.na(x) & x < min_allowed),
      Missing_n = sum(is.na(x)),
      stringsAsFactors = FALSE
    )
  }))
}

schema_check <- function(df, dataset_name) {
  data.frame(
    Dataset = dataset_name,
    Variable = names(df),
    Class = sapply(df, function(x) paste(class(x), collapse = ", ")),
    stringsAsFactors = FALSE
  )
}

required_column_check <- function(df, dataset_name, required_cols) {
  data.frame(
    Dataset = dataset_name,
    Required_Column = required_cols,
    Present = required_cols %in% names(df),
    stringsAsFactors = FALSE
  )
}

week_range_check <- function(df, dataset_name, week_var) {
  wk <- suppressWarnings(as.integer(df[[week_var]]))
  data.frame(
    Dataset = dataset_name,
    Week_variable = week_var,
    Min_week = safe_min(wk),
    Max_week = safe_max(wk),
    Out_of_range_n = sum(!is.na(wk) & !(wk %in% 1:53)),
    Missing_n = sum(is.na(wk)),
    stringsAsFactors = FALSE
  )
}

year_range_check <- function(df, dataset_name, year_var) {
  yr <- suppressWarnings(as.integer(df[[year_var]]))
  data.frame(
    Dataset = dataset_name,
    Year_variable = year_var,
    Min_year = safe_min(yr),
    Max_year = safe_max(yr),
    Missing_n = sum(is.na(yr)),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# 3. LOAD DATA
# =============================================================================
section_header("1. LOADING DATA")

df_qc  <- read_excel(PATH, sheet = "QC Data")
df_reg <- read_excel(PATH, sheet = "Regional Data")
df_cty <- read_excel(PATH, sheet = "Country Data")

cat("QC Data loaded:       ", nrow(df_qc),  "rows x", ncol(df_qc),  "cols\n")
cat("Regional Data loaded: ", nrow(df_reg), "rows x", ncol(df_reg), "cols\n")
cat("Country Data loaded:  ", nrow(df_cty), "rows x", ncol(df_cty), "cols\n")

# =============================================================================
# 4. REQUIRED COLUMN DEFINITIONS
# =============================================================================
section_header("2. REQUIRED COLUMN VALIDATION")

required_qc  <- c("YR", "WN", "DC_QC", "RF_NASA")
required_reg <- c("REGION", "YR", "WN", "DC_DOH", "RF_HDX")
required_cty <- c("COUNTRY", "YR", "WN", "DC_OPENDENGUE", "RF_NASA")

req_check_qc  <- required_column_check(df_qc,  "QC Data", required_qc)
req_check_reg <- required_column_check(df_reg, "Regional Data", required_reg)
req_check_cty <- required_column_check(df_cty, "Country Data", required_cty)

print(req_check_qc)
print(req_check_reg)
print(req_check_cty)

if (!all(req_check_qc$Present, req_check_reg$Present, req_check_cty$Present)) {
  stop("One or more required columns are missing. Validation cannot proceed.")
}

# =============================================================================
# 5. DATA TYPE HARMONIZATION
# =============================================================================
section_header("3. DATA TYPE HARMONIZATION")

df_qc <- df_qc %>%
  mutate(
    YR = as.integer(YR),
    WN = as.integer(WN),
    DC_QC = as.numeric(DC_QC),
    RF_NASA = as.numeric(RF_NASA)
  )

df_reg <- df_reg %>%
  mutate(
    REGION = as.character(REGION),
    YR = as.integer(YR),
    WN = as.integer(WN),
    DC_DOH = as.numeric(DC_DOH),
    RF_HDX = as.numeric(RF_HDX)
  )

df_cty <- df_cty %>%
  mutate(
    COUNTRY = as.character(COUNTRY),
    YR = as.integer(YR),
    WN = as.integer(WN),
    DC_OPENDENGUE = as.numeric(DC_OPENDENGUE),
    RF_NASA = as.numeric(RF_NASA)
  )

schema_qc  <- schema_check(df_qc,  "QC Data")
schema_reg <- schema_check(df_reg, "Regional Data")
schema_cty <- schema_check(df_cty, "Country Data")

print(schema_qc)
print(schema_reg)
print(schema_cty)

# =============================================================================
# 6. BASIC STRUCTURAL SUMMARIES
# =============================================================================
section_header("4. BASIC STRUCTURAL SUMMARIES")

dataset_overview <- bind_rows(
  data.frame(
    Dataset = "QC Data",
    Rows = nrow(df_qc),
    Columns = ncol(df_qc),
    Unique_years = safe_n_distinct(df_qc$YR),
    Unique_weeks = safe_n_distinct(df_qc$WN),
    Unique_geographies = 1
  ),
  data.frame(
    Dataset = "Regional Data",
    Rows = nrow(df_reg),
    Columns = ncol(df_reg),
    Unique_years = safe_n_distinct(df_reg$YR),
    Unique_weeks = safe_n_distinct(df_reg$WN),
    Unique_geographies = safe_n_distinct(df_reg$REGION)
  ),
  data.frame(
    Dataset = "Country Data",
    Rows = nrow(df_cty),
    Columns = ncol(df_cty),
    Unique_years = safe_n_distinct(df_cty$YR),
    Unique_weeks = safe_n_distinct(df_cty$WN),
    Unique_geographies = safe_n_distinct(df_cty$COUNTRY)
  )
)

print(dataset_overview)

# =============================================================================
# 7. MISSINGNESS ASSESSMENT
# =============================================================================
section_header("5. MISSINGNESS ASSESSMENT")

missing_qc  <- missing_summary(df_qc,  "QC Data")
missing_reg <- missing_summary(df_reg, "Regional Data")
missing_cty <- missing_summary(df_cty, "Country Data")

missing_all <- bind_rows(missing_qc, missing_reg, missing_cty)
print(missing_all)

# =============================================================================
# 8. DUPLICATE KEY CHECKS
# =============================================================================
section_header("6. DUPLICATE RECORD CHECKS")

dup_qc  <- duplicate_summary(df_qc,  c("YR", "WN"), "QC Data")
dup_reg <- duplicate_summary(df_reg, c("REGION", "YR", "WN"), "Regional Data")
dup_cty <- duplicate_summary(df_cty, c("COUNTRY", "YR", "WN"), "Country Data")

duplicate_results <- bind_rows(dup_qc, dup_reg, dup_cty)
print(duplicate_results)

# =============================================================================
# 9. TEMPORAL RANGE AND WEEK RANGE CHECKS
# =============================================================================
section_header("7. TEMPORAL RANGE CHECKS")

year_qc  <- year_range_check(df_qc,  "QC Data", "YR")
year_reg <- year_range_check(df_reg, "Regional Data", "YR")
year_cty <- year_range_check(df_cty, "Country Data", "YR")

week_qc  <- week_range_check(df_qc,  "QC Data", "WN")
week_reg <- week_range_check(df_reg, "Regional Data", "WN")
week_cty <- week_range_check(df_cty, "Country Data", "WN")

print(bind_rows(year_qc, year_reg, year_cty))
print(bind_rows(week_qc, week_reg, week_cty))

# =============================================================================
# 10. TEMPORAL CONTINUITY BY GEOGRAPHIC UNIT
# =============================================================================
section_header("8. TEMPORAL CONTINUITY BY GEOGRAPHIC UNIT")

continuity_qc <- continuity_check(df_qc, character(0), "YR", "WN", "QC Data")
continuity_reg <- continuity_check(df_reg, "REGION", "YR", "WN", "Regional Data")
continuity_cty <- continuity_check(df_cty, "COUNTRY", "YR", "WN", "Country Data")

print(continuity_qc)
print(continuity_reg)
print(continuity_cty)

# =============================================================================
# 11. VALUE-DOMAIN VALIDATION
# =============================================================================
section_header("9. VALUE-DOMAIN VALIDATION")

range_qc <- range_check_numeric(
  df_qc,
  "QC Data",
  c("DC_QC", "RF_NASA"),
  min_allowed = 0
)

range_reg <- range_check_numeric(
  df_reg,
  "Regional Data",
  c("DC_DOH", "RF_HDX"),
  min_allowed = 0
)

range_cty <- range_check_numeric(
  df_cty,
  "Country Data",
  c("DC_OPENDENGUE", "RF_NASA"),
  min_allowed = 0
)

value_domain_results <- bind_rows(range_qc, range_reg, range_cty)
print(value_domain_results)

# =============================================================================
# 12. GEOGRAPHIC IDENTIFIER CHECKS
# =============================================================================
section_header("10. GEOGRAPHIC IDENTIFIER CHECKS")

geo_reg <- df_reg %>%
  summarise(
    Dataset = "Regional Data",
    Missing_REGION = sum(is.na(REGION) | str_trim(REGION) == ""),
    Unique_REGION = n_distinct(REGION, na.rm = TRUE)
  )

geo_cty <- df_cty %>%
  summarise(
    Dataset = "Country Data",
    Missing_COUNTRY = sum(is.na(COUNTRY) | str_trim(COUNTRY) == ""),
    Unique_COUNTRY = n_distinct(COUNTRY, na.rm = TRUE)
  )

print(geo_reg)
print(geo_cty)

region_name_table <- df_reg %>%
  distinct(REGION) %>%
  arrange(REGION)

country_name_table <- df_cty %>%
  distinct(COUNTRY) %>%
  arrange(COUNTRY)

cat("\nRegional identifiers:\n")
print(region_name_table)

cat("\nCountry identifiers:\n")
print(country_name_table)

# =============================================================================
# 13. CROSS-SCALE AGGREGATION CHECKS
# =============================================================================
section_header("11. CROSS-SCALE AGGREGATION CHECKS")

# 13A. Regional weekly totals by year
regional_yearly <- df_reg %>%
  group_by(YR) %>%
  summarise(
    Regions_present = n_distinct(REGION),
    Regional_case_total = sum(DC_DOH, na.rm = TRUE),
    Regional_rainfall_mean = mean(RF_HDX, na.rm = TRUE),
    .groups = "drop"
  )

# 13B. Country weekly totals by year
country_yearly <- df_cty %>%
  group_by(YR) %>%
  summarise(
    Countries_present = n_distinct(COUNTRY),
    Country_case_total = sum(DC_OPENDENGUE, na.rm = TRUE),
    Country_rainfall_mean = mean(RF_NASA, na.rm = TRUE),
    .groups = "drop"
  )

# 13C. QC yearly totals
qc_yearly <- df_qc %>%
  group_by(YR) %>%
  summarise(
    QC_case_total = sum(DC_QC, na.rm = TRUE),
    QC_rainfall_mean = mean(RF_NASA, na.rm = TRUE),
    .groups = "drop"
  )

print(qc_yearly)
print(regional_yearly)
print(country_yearly)

# =============================================================================
# 14. WEEKLY RECORD COUNT CONSISTENCY
# =============================================================================
section_header("12. WEEKLY RECORD COUNT CONSISTENCY")

weekly_count_qc <- df_qc %>%
  count(YR, name = "n_weeks_qc")

weekly_count_reg <- df_reg %>%
  count(REGION, YR, name = "n_weeks_region")

weekly_count_cty <- df_cty %>%
  count(COUNTRY, YR, name = "n_weeks_country")

cat("QC weekly counts by year:\n")
print(weekly_count_qc)

cat("\nRegional weekly counts by geography-year:\n")
print(weekly_count_reg)

cat("\nCountry weekly counts by geography-year:\n")
print(weekly_count_cty)

# =============================================================================
# 15. NEUTRAL DATA QUALITY FLAGS
# =============================================================================
section_header("13. DATA QUALITY FLAGS")

quality_flags <- bind_rows(
  data.frame(
    Dataset = "QC Data",
    Check = "Missing required columns",
    Flag = ifelse(any(!req_check_qc$Present), "YES", "NO")
  ),
  data.frame(
    Dataset = "Regional Data",
    Check = "Missing required columns",
    Flag = ifelse(any(!req_check_reg$Present), "YES", "NO")
  ),
  data.frame(
    Dataset = "Country Data",
    Check = "Missing required columns",
    Flag = ifelse(any(!req_check_cty$Present), "YES", "NO")
  ),
  data.frame(
    Dataset = "QC Data",
    Check = "Duplicate key groups",
    Flag = ifelse(dup_qc$Duplicate_key_groups > 0, "YES", "NO")
  ),
  data.frame(
    Dataset = "Regional Data",
    Check = "Duplicate key groups",
    Flag = ifelse(dup_reg$Duplicate_key_groups > 0, "YES", "NO")
  ),
  data.frame(
    Dataset = "Country Data",
    Check = "Duplicate key groups",
    Flag = ifelse(dup_cty$Duplicate_key_groups > 0, "YES", "NO")
  ),
  data.frame(
    Dataset = "QC Data",
    Check = "Negative values in validated numeric fields",
    Flag = ifelse(any(range_qc$Negative_values > 0), "YES", "NO")
  ),
  data.frame(
    Dataset = "Regional Data",
    Check = "Negative values in validated numeric fields",
    Flag = ifelse(any(range_reg$Negative_values > 0), "YES", "NO")
  ),
  data.frame(
    Dataset = "Country Data",
    Check = "Negative values in validated numeric fields",
    Flag = ifelse(any(range_cty$Negative_values > 0), "YES", "NO")
  ),
  data.frame(
    Dataset = "QC Data",
    Check = "Out-of-range week values",
    Flag = ifelse(week_qc$Out_of_range_n > 0, "YES", "NO")
  ),
  data.frame(
    Dataset = "Regional Data",
    Check = "Out-of-range week values",
    Flag = ifelse(week_reg$Out_of_range_n > 0, "YES", "NO")
  ),
  data.frame(
    Dataset = "Country Data",
    Check = "Out-of-range week values",
    Flag = ifelse(week_cty$Out_of_range_n > 0, "YES", "NO")
  )
)

print(quality_flags)

# =============================================================================
# 16. EXPORT VALIDATION OUTPUTS
# =============================================================================
section_header("14. EXPORTING VALIDATION OUTPUTS")

write.csv(dataset_overview,      file.path(out_dir, "01_dataset_overview.csv"), row.names = FALSE)
write.csv(bind_rows(schema_qc, schema_reg, schema_cty),
          file.path(out_dir, "02_schema_check.csv"), row.names = FALSE)
write.csv(bind_rows(req_check_qc, req_check_reg, req_check_cty),
          file.path(out_dir, "03_required_column_check.csv"), row.names = FALSE)
write.csv(missing_all,           file.path(out_dir, "04_missingness_summary.csv"), row.names = FALSE)
write.csv(duplicate_results,     file.path(out_dir, "05_duplicate_summary.csv"), row.names = FALSE)
write.csv(bind_rows(year_qc, year_reg, year_cty),
          file.path(out_dir, "06_year_range_check.csv"), row.names = FALSE)
write.csv(bind_rows(week_qc, week_reg, week_cty),
          file.path(out_dir, "07_week_range_check.csv"), row.names = FALSE)
write.csv(continuity_qc,         file.path(out_dir, "08_temporal_continuity_qc.csv"), row.names = FALSE)
write.csv(continuity_reg,        file.path(out_dir, "09_temporal_continuity_regional.csv"), row.names = FALSE)
write.csv(continuity_cty,        file.path(out_dir, "10_temporal_continuity_country.csv"), row.names = FALSE)
write.csv(value_domain_results,  file.path(out_dir, "11_value_domain_validation.csv"), row.names = FALSE)
write.csv(region_name_table,     file.path(out_dir, "12_region_identifiers.csv"), row.names = FALSE)
write.csv(country_name_table,    file.path(out_dir, "13_country_identifiers.csv"), row.names = FALSE)
write.csv(qc_yearly,             file.path(out_dir, "14_qc_yearly_summary.csv"), row.names = FALSE)
write.csv(regional_yearly,       file.path(out_dir, "15_regional_yearly_summary.csv"), row.names = FALSE)
write.csv(country_yearly,        file.path(out_dir, "16_country_yearly_summary.csv"), row.names = FALSE)
write.csv(weekly_count_qc,       file.path(out_dir, "17_qc_weekly_counts.csv"), row.names = FALSE)
write.csv(weekly_count_reg,      file.path(out_dir, "18_regional_weekly_counts.csv"), row.names = FALSE)
write.csv(weekly_count_cty,      file.path(out_dir, "19_country_weekly_counts.csv"), row.names = FALSE)
write.csv(quality_flags,         file.path(out_dir, "20_quality_flags.csv"), row.names = FALSE)

# Optional: single Excel workbook with all validation outputs
validation_workbook <- list(
  dataset_overview = dataset_overview,
  schema_check = bind_rows(schema_qc, schema_reg, schema_cty),
  required_column_check = bind_rows(req_check_qc, req_check_reg, req_check_cty),
  missingness_summary = missing_all,
  duplicate_summary = duplicate_results,
  year_range_check = bind_rows(year_qc, year_reg, year_cty),
  week_range_check = bind_rows(week_qc, week_reg, week_cty),
  temporal_continuity_qc = continuity_qc,
  temporal_continuity_regional = continuity_reg,
  temporal_continuity_country = continuity_cty,
  value_domain_validation = value_domain_results,
  region_identifiers = region_name_table,
  country_identifiers = country_name_table,
  qc_yearly_summary = qc_yearly,
  regional_yearly_summary = regional_yearly,
  country_yearly_summary = country_yearly,
  qc_weekly_counts = weekly_count_qc,
  regional_weekly_counts = weekly_count_reg,
  country_weekly_counts = weekly_count_cty,
  quality_flags = quality_flags
)

write_xlsx(validation_workbook, path = file.path(out_dir, "technical_validation_report.xlsx"))

cat("Validation outputs saved to folder:", out_dir, "\n")

# =============================================================================
# 17. REPRODUCIBILITY LOG
# =============================================================================
section_header("15. REPRODUCIBILITY LOG")

cat("Input file:", PATH, "\n")
cat("Output folder:", out_dir, "\n")
cat("Run timestamp:", as.character(Sys.time()), "\n")

# Save session info to text file
sink(file.path(out_dir, "21_session_info.txt"))
sessionInfo()
sink()

cat("\nSession info saved.\n")
cat("Technical validation workflow completed successfully.\n")