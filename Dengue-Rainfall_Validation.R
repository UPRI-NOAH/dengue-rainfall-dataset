# =============================================================================
# TECHNICAL VALIDATION WORKFLOW
# Weekly Dengue Incidence Linked to Satellite Rainfall Dataset
#
# PURPOSE
#   Complete 10-stage technical validation of the three dataset scales
#   contained in Dengue-Rainfall_Dataset.xlsx:
#     1) QC Data      — Quezon City city-level,  weekly, 2010-2025
#     2) Regional     — 17 Philippine regions,   weekly, 2016-2025
#     3) Country      — 8 dengue-endemic nations, weekly, 2016-2025
#
# WORKBOOK STRUCTURE
#   Each data sheet has a one-row informational banner followed by headers:
#     Row 1 : Banner / info string  (skipped via skip = 1 in read_excel)
#     Row 2 : Column headers
#     Row 3+: Data
#
# VALIDATION STAGES
#   Stage  1 — Dataset structure and row reconciliation
#   Stage  2 — Cell-level completeness
#   Stage  3 — Structural year coverage
#   Stage  4 — Structural week completeness
#   Stage  5 — Duplicate record validation
#   Stage  6 — Value-domain validation
#   Stage  7 — Schema and type verification
#   Stage  8 — Temporal validity and epidemiological plausibility
#   Stage  9 — Cross-scale descriptive summaries
#   Stage 10 — Data quality flag verification
#
# FLAG COLUMNS (already present in workbook; verified in Stage 10)
#   FLAG_COVID           QC Data       1 = YR in {2020, 2021}
#   FLAG_SINGLE_CELL_RF  QC + Country  1 = single IMERG centroid cell
#   FLAG_PLAUSIBILITY    QC + Regional 1 = week-over-week change > +-500%,
#                                      excluding cross-multi-year comparisons (year-gap
#                                      guard: YR - YR_Lag <= 1)
#   FLAG_DEKADAL_APPROX  Regional      1 = RF_HDX from dekadal disaggregation
#   FLAG_TERMINAL_GAP    Country       1 = documented terminal truncation
#
# OUTPUTS
#   CSV files for every validation stage
#   Master Excel workbook: technical_validation_report.xlsx
#   Run log (run_log.txt) and session info (session_info.txt)
#
# REQUIRES: R >= 4.1.0
# UPDATE PATH BELOW before running
# =============================================================================


# =============================================================================
# 0. PACKAGE SETUP
# =============================================================================

needed <- c("readxl", "dplyr", "tidyr", "stringr", "purrr", "writexl", "tibble")
for (pkg in needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(writexl)
  library(tibble)
})

options(
  stringsAsFactors       = FALSE,
  scipen                 = 999,
  tibble.print_max       = Inf,
  tibble.width           = Inf,
  dplyr.summarise.inform = FALSE
)


# =============================================================================
# 1. USER INPUT
# =============================================================================

PATH    <- "C:/Users/User/Downloads/Dengue-Rainfall_Dataset.xlsx"
out_dir <- "technical_validation_outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# 2. UTILITY FUNCTIONS
# NOTE: All if/else constructs are either on a single line or use explicit
#       braces { } on both branches to prevent "unexpected else" parse errors.
# =============================================================================

section_header <- function(title) {
  cat("\n", strrep("=", 100), "\n", title, "\n", strrep("=", 100), "\n", sep = "")
}

print_full <- function(x, title = NULL) {
  if (!is.null(title)) { cat("\n", title, "\n", sep = "") }
  if (inherits(x, c("tbl_df", "tbl", "data.frame"))) {
    print(x, n = Inf, width = Inf)
  } else {
    print(x)
  }
}

safe_n_distinct <- function(x) { dplyr::n_distinct(x, na.rm = TRUE) }

safe_min <- function(x) {
  if (!length(x) || all(is.na(x))) { NA } else { min(x, na.rm = TRUE) }
}

safe_max <- function(x) {
  if (!length(x) || all(is.na(x))) { NA } else { max(x, na.rm = TRUE) }
}

safe_mean <- function(x) {
  if (!length(x) || all(is.na(x))) { NA_real_ } else { mean(x, na.rm = TRUE) }
}

safe_sd <- function(x) {
  if (!length(x) || all(is.na(x))) { NA_real_ } else { sd(x, na.rm = TRUE) }
}

safe_sum <- function(x) {
  if (!length(x) || all(is.na(x))) { NA_real_ } else { sum(x, na.rm = TRUE) }
}

safe_pct <- function(num, den, digits = 4) {
  if (is.na(den) || den == 0) { NA_real_ } else { round((num / den) * 100, digits) }
}

trim_blank_to_na <- function(x) {
  x <- str_trim(as.character(x))
  x[x == ""] <- NA_character_
  x
}

collapse_years <- function(x) {
  x <- sort(unique(x[!is.na(x)]))
  if (length(x) == 0) { "None" } else { paste(x, collapse = ", ") }
}

collapse_weeks <- function(x) {
  x <- sort(unique(x[!is.na(x)]))
  if (length(x) == 0) { "None" } else { paste(x, collapse = ", ") }
}

iso_weeks_in_year <- function(year) {
  year <- suppressWarnings(as.integer(year))
  if (is.na(year)) {
    NA_integer_
  } else {
    as.integer(strftime(as.Date(sprintf("%d-12-28", year)), format = "%V"))
  }
}

write_csv_safe <- function(df, file) {
  utils::write.csv(df, file = file, row.names = FALSE, na = "")
}


# =============================================================================
# 3. VALIDATION HELPER FUNCTIONS
# =============================================================================

# ── Required column check ────────────────────────────────────────────────────
required_column_check <- function(df, dataset_name, required_cols) {
  tibble(
    Dataset         = dataset_name,
    Required_Column = required_cols,
    Present         = required_cols %in% names(df)
  )
}

# ── Schema (column classes) ──────────────────────────────────────────────────
schema_check <- function(df, dataset_name) {
  tibble(
    Dataset  = dataset_name,
    Variable = names(df),
    Class    = vapply(df, function(x) paste(class(x), collapse = ", "), character(1))
  )
}

# ── Cell-level missingness ───────────────────────────────────────────────────
cell_missingness <- function(df, dataset_name, check_cols) {
  cols      <- intersect(check_cols, names(df))
  na_counts <- sapply(df[cols], function(x) sum(is.na(x)))
  total_na  <- sum(na_counts)
  # Build the "variables with missing" string using a local variable
  # to avoid splitting if/else across the tibble() argument boundary
  vars_missing <- if (total_na == 0) { "None" } else {
    paste(names(na_counts)[na_counts > 0], collapse = ", ")
  }
  tibble(
    Dataset                       = dataset_name,
    Variables_Checked             = paste(cols, collapse = ", "),
    Variables_with_Missing_Values = vars_missing,
    Total_Missing_Entries         = total_na,
    Missing_Pct                   = safe_pct(total_na, nrow(df) * length(cols)),
    Interpretation                = "Cell-level NA in observed rows only; Stages 3-4 cover structural gaps"
  )
}

# ── Duplicate detection ──────────────────────────────────────────────────────
duplicate_summary <- function(df, keys, dataset_name) {
  dup_df <- df %>%
    count(across(all_of(keys)), name = "n_records") %>%
    filter(n_records > 1)
  n_dup  <- nrow(dup_df)
  excess <- if (n_dup == 0) { 0L } else { sum(dup_df$n_records - 1L) }
  tibble(
    Dataset               = dataset_name,
    Key_Structure         = paste(keys, collapse = " + "),
    Duplicate_Key_Groups  = n_dup,
    Excess_Duplicate_Rows = excess
  )
}

duplicate_detail <- function(df, keys, dataset_name) {
  df %>%
    count(across(all_of(keys)), name = "n_records") %>%
    filter(n_records > 1) %>%
    mutate(Dataset = dataset_name, .before = 1)
}

# ── Value-domain check ───────────────────────────────────────────────────────
value_domain_check <- function(df, dataset_name, var_names) {
  bind_rows(lapply(var_names, function(v) {
    x <- df[[v]]
    tibble(
      Dataset         = dataset_name,
      Variable        = v,
      Min             = safe_min(x),
      Max             = safe_max(x),
      Negative_Values = sum(!is.na(x) & x < 0),
      Missing_n       = sum(is.na(x))
    )
  }))
}

# ── FLAG binary check ────────────────────────────────────────────────────────
check_binary_flags <- function(df, dataset_name, flag_cols) {
  bind_rows(lapply(flag_cols, function(col) {
    x <- df[[col]]
    tibble(
      Dataset           = dataset_name,
      Flag_Column       = col,
      Min               = safe_min(x),
      Max               = safe_max(x),
      Non_Binary_Values = sum(!is.na(x) & !(x %in% c(0L, 1L))),
      NA_Values         = sum(is.na(x))
    )
  }))
}

# ── Row reconciliation ───────────────────────────────────────────────────────
row_reconciliation <- function(df, dataset_name, geo_var = NULL) {
  geo_val <- if (is.null(geo_var)) { "ALL" } else { trim_blank_to_na(df[[geo_var]]) }
  tmp <- df %>%
    mutate(Geography = geo_val) %>%
    filter(!is.na(Geography), !is.na(YR), !is.na(WN))
  obs_gy       <- tmp %>% distinct(Geography, YR)
  expected_iso <- obs_gy %>%
    rowwise() %>%
    mutate(w = iso_weeks_in_year(YR)) %>%
    ungroup() %>%
    summarise(total = sum(w, na.rm = TRUE)) %>%
    pull(total)
  tibble(
    Dataset           = dataset_name,
    Observed_Rows     = nrow(df),
    Unique_Key_Rows   = nrow(tmp %>% distinct(Geography, YR, WN)),
    Expected_ISO_Rows = expected_iso,
    Structural_Gap    = expected_iso - nrow(tmp %>% distinct(Geography, YR, WN))
  )
}

# ── Structural year coverage ─────────────────────────────────────────────────
structural_year_coverage <- function(df, dataset_name, geo_var = NULL) {
  geo_val <- if (is.null(geo_var)) { rep("ALL", nrow(df)) } else { trim_blank_to_na(df[[geo_var]]) }
  tmp <- tibble(Geography = geo_val, YR = as.integer(df$YR)) %>%
    filter(!is.na(Geography), !is.na(YR)) %>%
    distinct()
  if (nrow(tmp) == 0) {
    return(list(summary = tibble(), within_span = tibble(), vs_universe = tibble()))
  }
  global_yrs <- sort(unique(tmp$YR))
  geo_obs <- tmp %>%
    group_by(Geography) %>%
    summarise(Observed = list(sort(unique(YR))), .groups = "drop")
  summary_tbl <- geo_obs %>%
    mutate(
      Dataset               = dataset_name,
      Min_Year              = sapply(Observed, min),
      Max_Year              = sapply(Observed, max),
      Observed_Years_n      = sapply(Observed, length),
      Span_Years_n          = Max_Year - Min_Year + 1L,
      Missing_Within_Span_n = Span_Years_n - Observed_Years_n,
      Missing_Within_Span   = sapply(Observed, function(o) {
        collapse_years(setdiff(seq(min(o), max(o)), o))
      }),
      Missing_vs_Universe_n = sapply(Observed, function(o) {
        length(setdiff(global_yrs, o))
      }),
      Missing_vs_Universe   = sapply(Observed, function(o) {
        collapse_years(setdiff(global_yrs, o))
      }),
      Balanced_vs_Universe  = sapply(Observed, function(o) {
        if (length(setdiff(global_yrs, o)) == 0) { "YES" } else { "NO" }
      })
    ) %>%
    select(
      Dataset, Geography, Min_Year, Max_Year,
      Observed_Years_n, Span_Years_n,
      Missing_Within_Span_n, Missing_Within_Span,
      Missing_vs_Universe_n, Missing_vs_Universe,
      Balanced_vs_Universe
    ) %>%
    arrange(Dataset, Geography)
  within_span <- geo_obs %>%
    rowwise() %>%
    mutate(My = list(setdiff(seq(min(Observed), max(Observed)), Observed))) %>%
    ungroup() %>%
    filter(lengths(My) > 0) %>%
    select(Geography, My) %>%
    unnest(My) %>%
    rename(Missing_Year = My) %>%
    mutate(
      Dataset          = dataset_name,
      Missingness_Type = "Within geography-specific span",
      .before          = 1
    ) %>%
    arrange(Dataset, Geography, Missing_Year)
  vs_universe <- geo_obs %>%
    rowwise() %>%
    mutate(My = list(setdiff(global_yrs, Observed))) %>%
    ungroup() %>%
    filter(lengths(My) > 0) %>%
    select(Geography, My) %>%
    unnest(My) %>%
    rename(Missing_Year = My) %>%
    mutate(
      Dataset          = dataset_name,
      Missingness_Type = "Relative to dataset-wide year universe",
      .before          = 1
    ) %>%
    arrange(Dataset, Geography, Missing_Year)
  list(summary = summary_tbl, within_span = within_span, vs_universe = vs_universe)
}

# ── Structural week completeness ─────────────────────────────────────────────
structural_week_completeness <- function(df, dataset_name, geo_var = NULL) {
  geo_val <- if (is.null(geo_var)) { rep("ALL", nrow(df)) } else { trim_blank_to_na(df[[geo_var]]) }
  obs <- tibble(
    Geography = geo_val,
    YR        = as.integer(df$YR),
    WN        = as.integer(df$WN)
  ) %>%
    filter(!is.na(Geography), !is.na(YR), !is.na(WN)) %>%
    distinct()
  obs_gy <- obs %>% distinct(Geography, YR)
  if (nrow(obs_gy) == 0) {
    return(list(summary = tibble(), missing_weeks = tibble()))
  }
  expected_grid <- obs_gy %>%
    rowwise() %>%
    mutate(Exp_Wks = iso_weeks_in_year(YR), WN = list(seq_len(Exp_Wks))) %>%
    ungroup() %>%
    unnest(WN)
  missing_wks <- expected_grid %>%
    anti_join(obs, by = c("Geography", "YR", "WN")) %>%
    mutate(Dataset = dataset_name, .before = 1) %>%
    rename(Missing_Week = WN) %>%
    arrange(Dataset, Geography, YR, Missing_Week)
  week_summ <- expected_grid %>%
    count(Geography, YR, name = "Expected_Weeks") %>%
    left_join(
      obs %>% count(Geography, YR, name = "Observed_Weeks"),
      by = c("Geography", "YR")
    ) %>%
    mutate(
      Observed_Weeks  = coalesce(Observed_Weeks, 0L),
      Missing_Weeks_n = Expected_Weeks - Observed_Weeks
    ) %>%
    rowwise() %>%
    mutate(
      Missing_Weeks = {
        # Local variables inside {} are safe — avoids "unexpected else" outside {}
        cur_geo <- Geography
        cur_yr  <- YR
        cur_n   <- Missing_Weeks_n
        if (cur_n == 0) {
          "None"
        } else {
          wks <- missing_wks %>%
            filter(Geography == cur_geo, YR == cur_yr) %>%
            pull(Missing_Week)
          collapse_weeks(wks)
        }
      },
      Complete = if (Missing_Weeks_n == 0) { "YES" } else { "NO" }
    ) %>%
    ungroup() %>%
    mutate(Dataset = dataset_name, .before = 1) %>%
    arrange(Dataset, Geography, YR)
  list(summary = week_summ, missing_weeks = missing_wks)
}

# ── Temporal validity helpers ────────────────────────────────────────────────
year_range_check <- function(df, dataset_name) {
  tibble(
    Dataset        = dataset_name,
    Year_Range     = paste0(safe_min(df$YR), "\u2013", safe_max(df$YR)),
    Min_Year       = safe_min(df$YR),
    Max_Year       = safe_max(df$YR),
    NA_Year_Values = sum(is.na(df$YR))
  )
}

week_range_check <- function(df, dataset_name) {
  tibble(
    Dataset            = dataset_name,
    Week_Range         = paste0(safe_min(df$WN), "\u2013", safe_max(df$WN)),
    Out_of_Range_Weeks = sum(!is.na(df$WN) & !(df$WN %in% 1:53))
  )
}

invalid_week_for_year <- function(df, dataset_name) {
  tmp <- tibble(
    YR        = as.integer(df$YR),
    WN        = as.integer(df$WN),
    Max_Valid = vapply(as.integer(df$YR), iso_weeks_in_year, integer(1))
  ) %>%
    mutate(Invalid = !is.na(WN) & !is.na(Max_Valid) & WN > Max_Valid)
  tibble(
    Dataset               = dataset_name,
    Invalid_Week_for_Year = sum(tmp$Invalid, na.rm = TRUE)
  )
}

# ── Epidemiological plausibility ─────────────────────────────────────────────
plausibility_check <- function(df, dataset_name, case_var,
                               geo_var = NULL, threshold_pct = 500) {
  # YEAR-GAP GUARD (rev. per reviewer recommendation):
  # Week-over-week comparisons are restricted to observations separated by
  # at most one calendar year (YR - YR_Lag <= 1).  This prevents structural
  # multi-year gaps — such as the 2019-2021 COVID-19 absence in the Regional
  # dataset — from generating spurious high-percentage flags.  Consecutive-
  # year comparisons (WN52 -> WN1) are retained because they reflect genuine
  # seasonal dynamics and are correctly documented as year-boundary flags.
  #
  # Effect: REGION XI 2022 WK1 (YR - YR_Lag = 3) is excluded; 6 Regional
  # rows are flagged instead of the 7 produced without the guard.
  geo_val <- if (is.null(geo_var)) { rep("ALL", nrow(df)) } else { trim_blank_to_na(df[[geo_var]]) }
  tmp <- df %>%
    mutate(
      Geography = geo_val,
      Cases     = as.numeric(.data[[case_var]])
    ) %>%
    arrange(Geography, YR, WN) %>%
    group_by(Geography) %>%
    mutate(
      Cases_Lag  = lag(Cases),
      YR_Lag     = lag(YR),            # year of the lagged observation
      WoW_Pct    = ifelse(
        !is.na(Cases_Lag) & Cases_Lag != 0 &
          !is.na(YR_Lag)  & (YR - YR_Lag <= 1),   # year-gap guard
        (Cases - Cases_Lag) / Cases_Lag * 100,
        NA_real_
      ),
      Plaus_Flag = !is.na(WoW_Pct) & abs(WoW_Pct) > threshold_pct
    ) %>%
    ungroup()
  flagged <- tmp %>%
    filter(Plaus_Flag) %>%
    select(Geography, YR, WN, Cases_Lag, Cases, WoW_Pct) %>%
    mutate(Dataset = dataset_name, Threshold_Pct = threshold_pct, .before = 1)
  summary_tbl <- tibble(
    Dataset        = dataset_name,
    Case_Variable  = case_var,
    Threshold_Pct  = threshold_pct,
    Rows_Evaluated = sum(!is.na(tmp$WoW_Pct)),
    Flagged_Rows   = sum(tmp$Plaus_Flag, na.rm = TRUE),
    Flag_Rate_Pct  = safe_pct(
      sum(tmp$Plaus_Flag, na.rm = TRUE),
      sum(!is.na(tmp$WoW_Pct))
    )
  )
  list(summary = summary_tbl, detail = flagged)
}

# ── Cross-scale descriptive summaries ────────────────────────────────────────
cross_scale_summary <- function(df_qc, df_reg, df_cty) {
  qc_ann <- df_qc %>%
    group_by(YR) %>%
    summarise(
      Dataset          = "QC Data",
      Weeks_Observed   = n(),
      Cases_Total      = safe_sum(DC_QC),
      Cases_Mean_wk    = round(safe_mean(DC_QC),   1),
      Cases_SD_wk      = round(safe_sd(DC_QC),     1),
      Rainfall_Mean_mm = round(safe_mean(RF_NASA),  3),
      Rainfall_SD_mm   = round(safe_sd(RF_NASA),    3),
      .groups = "drop"
    ) %>%
    select(Dataset, YR, Weeks_Observed, Cases_Total,
           Cases_Mean_wk, Cases_SD_wk, Rainfall_Mean_mm, Rainfall_SD_mm)
  
  reg_ann <- df_reg %>%
    group_by(YR) %>%
    summarise(
      Dataset          = "Regional Data",
      Regions_Present  = safe_n_distinct(REGION),
      Weeks_Observed   = n(),
      Cases_Total      = safe_sum(DC_DOH),
      Cases_Mean_wk    = round(safe_mean(DC_DOH),  1),
      Cases_SD_wk      = round(safe_sd(DC_DOH),    1),
      Rainfall_Mean_mm = round(safe_mean(RF_HDX),  3),
      Rainfall_SD_mm   = round(safe_sd(RF_HDX),    3),
      .groups = "drop"
    ) %>%
    select(Dataset, YR, Regions_Present, Weeks_Observed, Cases_Total,
           Cases_Mean_wk, Cases_SD_wk, Rainfall_Mean_mm, Rainfall_SD_mm)
  
  cty_ann <- df_cty %>%
    group_by(YR) %>%
    summarise(
      Dataset           = "Country Data",
      Countries_Present = safe_n_distinct(COUNTRY),
      Weeks_Observed    = n(),
      Cases_Total       = safe_sum(DC_OPENDENGUE),
      Cases_Mean_wk     = round(safe_mean(DC_OPENDENGUE), 1),
      Cases_SD_wk       = round(safe_sd(DC_OPENDENGUE),   1),
      Rainfall_Mean_mm  = round(safe_mean(RF_NASA),        3),
      Rainfall_SD_mm    = round(safe_sd(RF_NASA),          3),
      .groups = "drop"
    ) %>%
    select(Dataset, YR, Countries_Present, Weeks_Observed, Cases_Total,
           Cases_Mean_wk, Cases_SD_wk, Rainfall_Mean_mm, Rainfall_SD_mm)
  
  qc_seas <- df_qc %>%
    filter(WN <= 52) %>%
    group_by(WN) %>%
    summarise(
      Dataset          = "QC Data",
      Cases_Mean       = round(safe_mean(DC_QC),   2),
      Rainfall_Mean_mm = round(safe_mean(RF_NASA), 3),
      .groups = "drop"
    )
  
  reg_seas <- df_reg %>%
    filter(WN <= 52) %>%
    group_by(WN) %>%
    summarise(
      Dataset          = "Regional Data",
      Cases_Mean       = round(safe_mean(DC_DOH), 2),
      Rainfall_Mean_mm = round(safe_mean(RF_HDX), 3),
      .groups = "drop"
    )
  
  cty_seas <- df_cty %>%
    filter(WN <= 52) %>%
    group_by(WN) %>%
    summarise(
      Dataset          = "Country Data",
      Cases_Mean       = round(safe_mean(DC_OPENDENGUE), 2),
      Rainfall_Mean_mm = round(safe_mean(RF_NASA),       3),
      .groups = "drop"
    )
  
  list(
    qc_annual    = qc_ann,  reg_annual    = reg_ann,  cty_annual    = cty_ann,
    qc_seasonal  = qc_seas, reg_seasonal  = reg_seas, cty_seasonal  = cty_seas
  )
}

# ── Quality flag summary table ────────────────────────────────────────────────
build_quality_flag_table <- function(
    req_qc,      req_reg,      req_cty,
    miss_qc,     miss_reg,     miss_cty,
    dup_qc,      dup_reg,      dup_cty,
    wk_qc,       wk_reg,       wk_cty,
    inv_qc,      inv_reg,      inv_cty,
    vd_qc,       vd_reg,       vd_cty,
    yearcov_qc,  yearcov_reg,  yearcov_cty,
    weekcomp_qc, weekcomp_reg, weekcomp_cty,
    plaus_qc,    plaus_reg
) {
  # f() is on one line — safe
  f <- function(cond) { if (isTRUE(cond)) { "YES" } else { "NO" } }
  
  checks <- c(
    "Missing required columns",
    "Cell-level NA in required fields",
    "Duplicate composite keys",
    "Out-of-range week values (< 1 or > 53)",
    "Invalid week for calendar year",
    "Negative values in numeric fields",
    "Missing years within geography span",
    "Unbalanced year coverage vs. universe",
    "Missing weeks within observed geography-year strata",
    "Plausibility: week-over-week case change > 500%"
  )
  
  qc_flags <- c(
    f(any(!req_qc$Present)),
    f(miss_qc$Total_Missing_Entries > 0),
    f(dup_qc$Duplicate_Key_Groups > 0),
    f(wk_qc$Out_of_Range_Weeks > 0),
    f(inv_qc$Invalid_Week_for_Year > 0),
    f(any(vd_qc$Negative_Values > 0)),
    f(nrow(yearcov_qc$within_span) > 0),
    f(any(yearcov_qc$summary$Balanced_vs_Universe == "NO")),
    f(nrow(weekcomp_qc$missing_weeks) > 0),
    f(plaus_qc$summary$Flagged_Rows > 0)
  )
  
  reg_flags <- c(
    f(any(!req_reg$Present)),
    f(miss_reg$Total_Missing_Entries > 0),
    f(dup_reg$Duplicate_Key_Groups > 0),
    f(wk_reg$Out_of_Range_Weeks > 0),
    f(inv_reg$Invalid_Week_for_Year > 0),
    f(any(vd_reg$Negative_Values > 0)),
    f(nrow(yearcov_reg$within_span) > 0),
    f(any(yearcov_reg$summary$Balanced_vs_Universe == "NO")),
    f(nrow(weekcomp_reg$missing_weeks) > 0),
    f(plaus_reg$summary$Flagged_Rows > 0)
  )
  
  cty_flags <- c(
    f(any(!req_cty$Present)),
    f(miss_cty$Total_Missing_Entries > 0),
    f(dup_cty$Duplicate_Key_Groups > 0),
    f(wk_cty$Out_of_Range_Weeks > 0),
    f(inv_cty$Invalid_Week_for_Year > 0),
    f(any(vd_cty$Negative_Values > 0)),
    f(nrow(yearcov_cty$within_span) > 0),
    f(any(yearcov_cty$summary$Balanced_vs_Universe == "NO")),
    f(nrow(weekcomp_cty$missing_weeks) > 0),
    "N/A"
  )
  
  tibble(
    Check         = checks,
    QC_Data       = qc_flags,
    Regional_Data = reg_flags,
    Country_Data  = cty_flags
  )
}

# ── FLAG column verification ──────────────────────────────────────────────────
verify_flags <- function(df_qc, df_reg, df_cty, plaus_qc_detail, plaus_reg_detail) {
  
  # QC: FLAG_COVID — expected = 1 when YR in {2020, 2021}
  expected_covid <- as.integer(df_qc$YR %in% c(2020L, 2021L))
  covid_match    <- identical(expected_covid, as.integer(df_qc$FLAG_COVID))
  
  # QC: FLAG_SINGLE_CELL_RF — expected = 1 for all rows
  qc_scrf_match  <- all(df_qc$FLAG_SINGLE_CELL_RF == 1L, na.rm = TRUE)
  
  # QC: FLAG_PLAUSIBILITY — build expected from plausibility detail
  exp_plaus_qc <- rep(0L, nrow(df_qc))
  if (nrow(plaus_qc_detail) > 0) {
    for (i in seq_len(nrow(plaus_qc_detail))) {
      idx <- which(
        df_qc$YR == plaus_qc_detail$YR[i] &
          df_qc$WN == plaus_qc_detail$WN[i]
      )
      if (length(idx) > 0) { exp_plaus_qc[idx] <- 1L }
    }
  }
  qc_plaus_match <- identical(exp_plaus_qc, as.integer(df_qc$FLAG_PLAUSIBILITY))
  
  # Regional: FLAG_DEKADAL_APPROX — expected = 1 for all rows
  reg_dek_match  <- all(df_reg$FLAG_DEKADAL_APPROX == 1L, na.rm = TRUE)
  
  # Regional: FLAG_PLAUSIBILITY
  # Expected: 6 rows (year-gap guard active; REGION XI 2022 WK1 = 0).
  # The distributed workbook has been updated: REGION XI 2022 WK1
  # FLAG_PLAUSIBILITY was corrected from 1 to 0 to match the year-gap
  # guard. Running this script on the distributed dataset will produce
  # Match = TRUE for all FLAG columns.
  exp_plaus_reg <- rep(0L, nrow(df_reg))
  if (nrow(plaus_reg_detail) > 0) {
    for (i in seq_len(nrow(plaus_reg_detail))) {
      idx <- which(
        df_reg$REGION == plaus_reg_detail$Geography[i] &
          df_reg$YR     == plaus_reg_detail$YR[i] &
          df_reg$WN     == plaus_reg_detail$WN[i]
      )
      if (length(idx) > 0) { exp_plaus_reg[idx] <- 1L }
    }
  }
  reg_plaus_match <- identical(exp_plaus_reg, as.integer(df_reg$FLAG_PLAUSIBILITY))
  
  # Country: FLAG_SINGLE_CELL_RF — expected = 1 for all rows
  cty_scrf_match <- all(df_cty$FLAG_SINGLE_CELL_RF == 1L, na.rm = TRUE)
  
  # Country: FLAG_TERMINAL_GAP — verify binary (0/1), document flagged rows
  # NOTE: This flag marks rows that ARE present in the dataset at the
  # terminal boundary (e.g. Philippines 2023 WK51-52 carry the flag).
  # Structurally absent weeks (e.g. Philippines 2023 WK46-50, which have
  # no rows) are documented in Stage 4 but cannot be flagged.
  cty_tgap_binary <- all(df_cty$FLAG_TERMINAL_GAP %in% c(0L, 1L), na.rm = TRUE)
  cty_tgap_rows   <- df_cty %>%
    filter(FLAG_TERMINAL_GAP == 1L) %>%
    select(COUNTRY, YR, WN, FLAG_TERMINAL_GAP)
  
  summary_tbl <- tibble(
    Dataset      = c("QC Data", "QC Data", "QC Data",
                     "Regional Data", "Regional Data",
                     "Country Data", "Country Data"),
    Flag_Column  = c("FLAG_COVID", "FLAG_SINGLE_CELL_RF", "FLAG_PLAUSIBILITY",
                     "FLAG_DEKADAL_APPROX", "FLAG_PLAUSIBILITY",
                     "FLAG_SINGLE_CELL_RF", "FLAG_TERMINAL_GAP"),
    Observed_Sum = c(
      sum(df_qc$FLAG_COVID,          na.rm = TRUE),
      sum(df_qc$FLAG_SINGLE_CELL_RF, na.rm = TRUE),
      sum(df_qc$FLAG_PLAUSIBILITY,   na.rm = TRUE),
      sum(df_reg$FLAG_DEKADAL_APPROX,na.rm = TRUE),
      sum(df_reg$FLAG_PLAUSIBILITY,  na.rm = TRUE),
      sum(df_cty$FLAG_SINGLE_CELL_RF,na.rm = TRUE),
      sum(df_cty$FLAG_TERMINAL_GAP,  na.rm = TRUE)
    ),
    Expected_Sum = c(
      sum(expected_covid),
      nrow(df_qc),
      sum(exp_plaus_qc),
      nrow(df_reg),
      sum(exp_plaus_reg),
      nrow(df_cty),
      NA_integer_
    ),
    Match = c(
      covid_match, qc_scrf_match, qc_plaus_match,
      reg_dek_match, reg_plaus_match,
      cty_scrf_match, cty_tgap_binary
    )
  )
  
  list(summary = summary_tbl, terminal_gap_rows = cty_tgap_rows)
}


# =============================================================================
# 4. INPUT FILE CHECK
# =============================================================================

section_header("INPUT FILE CHECK")

if (!file.exists(PATH)) {
  stop("Input file not found. Update PATH at the top of this script.")
}
cat("Input file found:\n  ", normalizePath(PATH), "\n")


# =============================================================================
# 5. LOAD DATA
#    skip = 1 drops the one-row banner so row 2 becomes the header row.
# =============================================================================

section_header("LOADING DATA")

required_sheets <- c("QC Data", "Regional Data", "Country Data")
available       <- readxl::excel_sheets(PATH)
missing_sheets  <- setdiff(required_sheets, available)

if (length(missing_sheets) > 0) {
  stop("Missing required sheet(s): ", paste(missing_sheets, collapse = ", "))
}

cat("Sheets in workbook:", paste(available, collapse = ", "), "\n")

df_qc  <- read_excel(PATH, sheet = "QC Data",       skip = 1)
df_reg <- read_excel(PATH, sheet = "Regional Data",  skip = 1)
df_cty <- read_excel(PATH, sheet = "Country Data",   skip = 1)

# Drop fully-empty trailing columns (artifact of banner row export)
df_qc  <- df_qc  %>% select(where(~ !all(is.na(.))))
df_reg <- df_reg %>% select(where(~ !all(is.na(.))))
df_cty <- df_cty %>% select(where(~ !all(is.na(.))))

total_rows <- nrow(df_qc) + nrow(df_reg) + nrow(df_cty)
cat(sprintf(
  "\nRows loaded:  QC = %d | Regional = %d | Country = %d | Total = %d\n",
  nrow(df_qc), nrow(df_reg), nrow(df_cty), total_rows
))


# =============================================================================
# 6. COLUMN DEFINITIONS AND TYPE COERCION
# =============================================================================

section_header("SCHEMA PREPARATION AND TYPE COERCION")

# Core analysis columns
core_qc  <- c("YR", "WN", "DC_QC",          "RF_NASA")
core_reg <- c("REGION",  "YR", "WN", "DC_DOH",     "RF_HDX")
core_cty <- c("COUNTRY", "YR", "WN", "DC_OPENDENGUE", "RF_NASA")

# FLAG columns (pre-existing in workbook; verified in Stage 10)
flag_qc  <- c("FLAG_COVID", "FLAG_SINGLE_CELL_RF", "FLAG_PLAUSIBILITY")
flag_reg <- c("FLAG_DEKADAL_APPROX", "FLAG_PLAUSIBILITY")
flag_cty <- c("FLAG_SINGLE_CELL_RF", "FLAG_TERMINAL_GAP")

# All required columns (core + flag)
required_qc  <- c(core_qc,  flag_qc)
required_reg <- c(core_reg, flag_reg)
required_cty <- c(core_cty, flag_cty)

req_qc  <- required_column_check(df_qc,  "QC Data",       required_qc)
req_reg <- required_column_check(df_reg, "Regional Data", required_reg)
req_cty <- required_column_check(df_cty, "Country Data",  required_cty)

required_check_all <- bind_rows(req_qc, req_reg, req_cty)
print_full(required_check_all, "Required column presence check:")

if (!all(required_check_all$Present)) {
  print(required_check_all %>% filter(!Present))
  stop("Missing required columns. Validation cannot proceed.")
}

# Log WN type before coercion (type consistency check)
wn_before <- tibble(
  Dataset         = c("QC Data", "Regional Data", "Country Data"),
  WN_Class_Before = c(
    paste(class(df_qc$WN),  collapse = ", "),
    paste(class(df_reg$WN), collapse = ", "),
    paste(class(df_cty$WN), collapse = ", ")
  )
)
print_full(wn_before, "WN column class BEFORE coercion:")

# Coerce to correct types
df_qc <- df_qc %>%
  mutate(
    YR                  = as.integer(YR),
    WN                  = as.integer(WN),
    DC_QC               = as.integer(DC_QC),
    RF_NASA             = as.numeric(RF_NASA),
    FLAG_COVID          = as.integer(FLAG_COVID),
    FLAG_SINGLE_CELL_RF = as.integer(FLAG_SINGLE_CELL_RF),
    FLAG_PLAUSIBILITY   = as.integer(FLAG_PLAUSIBILITY)
  )

df_reg <- df_reg %>%
  mutate(
    REGION              = trim_blank_to_na(REGION),
    YR                  = as.integer(YR),
    WN                  = as.integer(WN),
    DC_DOH              = as.integer(DC_DOH),
    RF_HDX              = as.numeric(RF_HDX),
    FLAG_DEKADAL_APPROX = as.integer(FLAG_DEKADAL_APPROX),
    FLAG_PLAUSIBILITY   = as.integer(FLAG_PLAUSIBILITY)
  )

df_cty <- df_cty %>%
  mutate(
    COUNTRY             = trim_blank_to_na(COUNTRY),
    YR                  = as.integer(YR),
    WN                  = as.integer(WN),
    DC_OPENDENGUE       = as.integer(DC_OPENDENGUE),
    RF_NASA             = as.numeric(RF_NASA),
    FLAG_SINGLE_CELL_RF = as.integer(FLAG_SINGLE_CELL_RF),
    FLAG_TERMINAL_GAP   = as.integer(FLAG_TERMINAL_GAP)
  )

wn_after <- tibble(
  Dataset        = c("QC Data", "Regional Data", "Country Data"),
  WN_Class_After = c(
    paste(class(df_qc$WN),  collapse = ", "),
    paste(class(df_reg$WN), collapse = ", "),
    paste(class(df_cty$WN), collapse = ", ")
  )
)
print_full(wn_after, "WN column class AFTER coercion (all should be integer):")

schema_all <- bind_rows(
  schema_check(df_qc,  "QC Data"),
  schema_check(df_reg, "Regional Data"),
  schema_check(df_cty, "Country Data")
)
print_full(schema_all, "Full schema after coercion:")


# =============================================================================
# STAGE 1 — DATASET STRUCTURE AND ROW RECONCILIATION
# =============================================================================

section_header("STAGE 1 — DATASET STRUCTURE AND ROW RECONCILIATION")

recon_qc  <- row_reconciliation(df_qc,  "QC Data",       geo_var = NULL)
recon_reg <- row_reconciliation(df_reg, "Regional Data", geo_var = "REGION")
recon_cty <- row_reconciliation(df_cty, "Country Data",  geo_var = "COUNTRY")

table_structure <- bind_rows(
  tibble(
    Dataset = "QC Data", Rows = nrow(df_qc), Cols = ncol(df_qc),
    Years = paste0(safe_min(df_qc$YR), "\u2013", safe_max(df_qc$YR)),
    Geo_Units    = 1L,
    Unique_Years = safe_n_distinct(df_qc$YR),
    Unique_Weeks = safe_n_distinct(df_qc$WN)
  ),
  tibble(
    Dataset = "Regional Data", Rows = nrow(df_reg), Cols = ncol(df_reg),
    Years = paste0(safe_min(df_reg$YR), "\u2013", safe_max(df_reg$YR)),
    Geo_Units    = safe_n_distinct(df_reg$REGION),
    Unique_Years = safe_n_distinct(df_reg$YR),
    Unique_Weeks = safe_n_distinct(df_reg$WN)
  ),
  tibble(
    Dataset = "Country Data", Rows = nrow(df_cty), Cols = ncol(df_cty),
    Years = paste0(safe_min(df_cty$YR), "\u2013", safe_max(df_cty$YR)),
    Geo_Units    = safe_n_distinct(df_cty$COUNTRY),
    Unique_Years = safe_n_distinct(df_cty$YR),
    Unique_Weeks = safe_n_distinct(df_cty$WN)
  )
) %>%
  left_join(bind_rows(recon_qc, recon_reg, recon_cty), by = "Dataset")

print_full(table_structure, "Dataset structure and row reconciliation:")
cat(sprintf("\nTotal records across all components: %d\n", total_rows))

geo_regions   <- df_reg %>% distinct(REGION)  %>% arrange(REGION)
geo_countries <- df_cty %>% distinct(COUNTRY) %>% arrange(COUNTRY)
print_full(geo_regions,   "Regions present:")
print_full(geo_countries, "Countries present:")


# =============================================================================
# STAGE 2 — CELL-LEVEL COMPLETENESS
# =============================================================================

section_header("STAGE 2 — CELL-LEVEL COMPLETENESS")
cat(
  "\nNOTE: Zero NA values does NOT mean the panel is structurally complete.\n",
  "      Structural gaps (absent strata) are assessed in Stages 3 and 4.\n",
  sep = ""
)

miss_qc  <- cell_missingness(df_qc,  "QC Data",       required_qc)
miss_reg <- cell_missingness(df_reg, "Regional Data", required_reg)
miss_cty <- cell_missingness(df_cty, "Country Data",  required_cty)

table_cell_miss <- bind_rows(miss_qc, miss_reg, miss_cty)
print_full(table_cell_miss, "Cell-level missingness summary:")


# =============================================================================
# STAGE 3 — STRUCTURAL YEAR COVERAGE
# =============================================================================

section_header("STAGE 3 — STRUCTURAL YEAR COVERAGE")

yearcov_qc  <- structural_year_coverage(df_qc,  "QC Data",       geo_var = NULL)
yearcov_reg <- structural_year_coverage(df_reg, "Regional Data", geo_var = "REGION")
yearcov_cty <- structural_year_coverage(df_cty, "Country Data",  geo_var = "COUNTRY")

table_year_cov <- bind_rows(
  yearcov_qc$summary, yearcov_reg$summary, yearcov_cty$summary
)
year_gaps_span <- bind_rows(
  yearcov_qc$within_span, yearcov_reg$within_span, yearcov_cty$within_span
)
year_gaps_univ <- bind_rows(
  yearcov_qc$vs_universe, yearcov_reg$vs_universe, yearcov_cty$vs_universe
)

print_full(table_year_cov,  "Structural year coverage summary:")
print_full(year_gaps_span,  "Missing years within geography-specific span:")
print_full(year_gaps_univ,  "Missing years vs. dataset-wide year universe:")


# =============================================================================
# STAGE 4 — STRUCTURAL WEEK COMPLETENESS
# =============================================================================

section_header("STAGE 4 — STRUCTURAL WEEK COMPLETENESS")

weekcomp_qc  <- structural_week_completeness(df_qc,  "QC Data",       geo_var = NULL)
weekcomp_reg <- structural_week_completeness(df_reg, "Regional Data", geo_var = "REGION")
weekcomp_cty <- structural_week_completeness(df_cty, "Country Data",  geo_var = "COUNTRY")

week_summ_all      <- bind_rows(
  weekcomp_qc$summary, weekcomp_reg$summary, weekcomp_cty$summary
)
table_week_incompl <- week_summ_all %>% filter(Complete == "NO")
missing_wks_detail <- bind_rows(
  weekcomp_qc$missing_weeks, weekcomp_reg$missing_weeks, weekcomp_cty$missing_weeks
)

print_full(table_week_incompl, "Incomplete geography-year combinations (missing weeks):")
print_full(missing_wks_detail, "Missing week detail:")
cat("\nCombinations NOT listed above are complete at the ISO week level.\n")


# =============================================================================
# STAGE 5 — DUPLICATE RECORD VALIDATION
# =============================================================================

section_header("STAGE 5 — DUPLICATE RECORD VALIDATION")

dup_qc  <- duplicate_summary(df_qc,  c("YR", "WN"),            "QC Data")
dup_reg <- duplicate_summary(df_reg, c("REGION", "YR", "WN"),  "Regional Data")
dup_cty <- duplicate_summary(df_cty, c("COUNTRY", "YR", "WN"), "Country Data")

table_dups <- bind_rows(dup_qc, dup_reg, dup_cty)
print_full(table_dups, "Duplicate record summary:")

dup_detail <- bind_rows(
  duplicate_detail(df_qc,  c("YR", "WN"),            "QC Data"),
  duplicate_detail(df_reg, c("REGION", "YR", "WN"),  "Regional Data"),
  duplicate_detail(df_cty, c("COUNTRY", "YR", "WN"), "Country Data")
)

# Use braces on both if and else branches — prevents "unexpected else" error
if (nrow(dup_detail) > 0) {
  print_full(dup_detail, "Duplicate key detail:")
} else {
  cat("No duplicate records detected in any dataset.\n")
}


# =============================================================================
# STAGE 6 — VALUE-DOMAIN VALIDATION
# =============================================================================

section_header("STAGE 6 — VALUE-DOMAIN VALIDATION")

vd_qc  <- value_domain_check(df_qc,  "QC Data",       c("DC_QC",        "RF_NASA"))
vd_reg <- value_domain_check(df_reg, "Regional Data", c("DC_DOH",        "RF_HDX"))
vd_cty <- value_domain_check(df_cty, "Country Data",  c("DC_OPENDENGUE", "RF_NASA"))

table_vd <- bind_rows(vd_qc, vd_reg, vd_cty)
print_full(table_vd, "Value-domain check (core numeric columns):")

flag_domain <- bind_rows(
  check_binary_flags(df_qc,  "QC Data",       flag_qc),
  check_binary_flags(df_reg, "Regional Data", flag_reg),
  check_binary_flags(df_cty, "Country Data",  flag_cty)
)
print_full(flag_domain, "FLAG column binary-domain check (must be 0 or 1):")


# =============================================================================
# STAGE 7 — SCHEMA AND TYPE VERIFICATION
# =============================================================================

section_header("STAGE 7 — SCHEMA AND TYPE VERIFICATION")

print_full(required_check_all, "Required columns present:")
print_full(schema_all,         "Variable classes after coercion:")
print_full(wn_before,          "WN source type (pre-coercion):")
print_full(wn_after,           "WN type post-coercion (integer in all sheets):")


# =============================================================================
# STAGE 8 — TEMPORAL VALIDITY AND EPIDEMIOLOGICAL PLAUSIBILITY
# =============================================================================

section_header("STAGE 8 — TEMPORAL VALIDITY AND EPIDEMIOLOGICAL PLAUSIBILITY")

yr_qc   <- year_range_check(df_qc,  "QC Data")
yr_reg  <- year_range_check(df_reg, "Regional Data")
yr_cty  <- year_range_check(df_cty, "Country Data")

wk_qc   <- week_range_check(df_qc,  "QC Data")
wk_reg  <- week_range_check(df_reg, "Regional Data")
wk_cty  <- week_range_check(df_cty, "Country Data")

inv_qc  <- invalid_week_for_year(df_qc,  "QC Data")
inv_reg <- invalid_week_for_year(df_reg, "Regional Data")
inv_cty <- invalid_week_for_year(df_cty, "Country Data")

table_temporal <- bind_rows(yr_qc, yr_reg, yr_cty) %>%
  left_join(bind_rows(wk_qc,  wk_reg,  wk_cty),  by = "Dataset") %>%
  left_join(bind_rows(inv_qc, inv_reg, inv_cty),  by = "Dataset")
print_full(table_temporal, "Temporal validity summary:")

cat(
  "\nPlausibility check: week-over-week dengue case change > +-500%\n",
  "  Year-gap guard: YR - YR_Lag <= 1 (excludes multi-year COVID-gap lags)\n",
  "  QC Data: 0 flags | Regional: 6 flags (year-boundary/small-count; advisory)\n",
  "  REGION XI 2022 WK1 FLAG_PLAUSIBILITY corrected to 0 in workbook.\n"
)
plaus_qc  <- plausibility_check(df_qc,  "QC Data",       "DC_QC",  geo_var = NULL)
plaus_reg <- plausibility_check(df_reg, "Regional Data", "DC_DOH", geo_var = "REGION")

table_plaus <- bind_rows(plaus_qc$summary, plaus_reg$summary)
print_full(table_plaus, "Plausibility summary:")

if (nrow(plaus_qc$detail) > 0) {
  print_full(plaus_qc$detail, "QC Data — plausibility-flagged rows:")
}
if (nrow(plaus_reg$detail) > 0) {
  print_full(plaus_reg$detail, "Regional Data — plausibility-flagged rows:")
}


# =============================================================================
# STAGE 9 — CROSS-SCALE DESCRIPTIVE SUMMARIES
# =============================================================================

section_header("STAGE 9 — CROSS-SCALE DESCRIPTIVE SUMMARIES")

cs <- cross_scale_summary(df_qc, df_reg, df_cty)

print_full(cs$qc_annual,    "QC Data — annual summary:")
print_full(cs$reg_annual,   "Regional Data — annual summary:")
print_full(cs$cty_annual,   "Country Data — annual summary:")
print_full(cs$qc_seasonal,  "QC seasonal profile (mean by ISO week 1-52):")
print_full(cs$reg_seasonal, "Regional seasonal profile:")
print_full(cs$cty_seasonal, "Country seasonal profile:")


# =============================================================================
# STAGE 10 — DATA QUALITY FLAG VERIFICATION
# =============================================================================

section_header("STAGE 10 — DATA QUALITY FLAG VERIFICATION")

table_quality_flags <- build_quality_flag_table(
  req_qc,      req_reg,      req_cty,
  miss_qc,     miss_reg,     miss_cty,
  dup_qc,      dup_reg,      dup_cty,
  wk_qc,       wk_reg,       wk_cty,
  inv_qc,      inv_reg,      inv_cty,
  vd_qc,       vd_reg,       vd_cty,
  yearcov_qc,  yearcov_reg,  yearcov_cty,
  weekcomp_qc, weekcomp_reg, weekcomp_cty,
  plaus_qc,    plaus_reg
)
print_full(table_quality_flags, "Structural quality flag summary table:")

cat(
  "\nVerifying pre-existing FLAG columns against expected values...\n",
  "  Expected: Match = TRUE for all checks (workbook corrected).\n"
)
flag_verif <- verify_flags(
  df_qc, df_reg, df_cty,
  plaus_qc$detail, plaus_reg$detail
)
print_full(flag_verif$summary,          "FLAG column verification (Match=TRUE means values agree):")
print_full(flag_verif$terminal_gap_rows,"FLAG_TERMINAL_GAP flagged rows:")

flag_dist <- bind_rows(
  tibble(
    Dataset              = "QC Data",
    FLAG_COVID           = sum(df_qc$FLAG_COVID,          na.rm = TRUE),
    FLAG_SINGLE_CELL_RF  = sum(df_qc$FLAG_SINGLE_CELL_RF, na.rm = TRUE),
    FLAG_PLAUSIBILITY    = sum(df_qc$FLAG_PLAUSIBILITY,   na.rm = TRUE)
  ),
  tibble(
    Dataset              = "Regional Data",
    FLAG_DEKADAL_APPROX  = sum(df_reg$FLAG_DEKADAL_APPROX, na.rm = TRUE),
    FLAG_PLAUSIBILITY    = sum(df_reg$FLAG_PLAUSIBILITY,   na.rm = TRUE)
  ),
  tibble(
    Dataset              = "Country Data",
    FLAG_SINGLE_CELL_RF  = sum(df_cty$FLAG_SINGLE_CELL_RF, na.rm = TRUE),
    FLAG_TERMINAL_GAP    = sum(df_cty$FLAG_TERMINAL_GAP,   na.rm = TRUE)
  )
)
print_full(flag_dist, "FLAG column distribution summary:")


# =============================================================================
# INTERPRETIVE GUIDE
# =============================================================================

section_header("INTERPRETIVE GUIDE")
cat(
  "1. Cell-level completeness (Stage 2): counts NA in existing rows only.\n",
  "   Zero NAs does not confirm a structurally complete panel.\n\n",
  "2. Structural completeness (Stages 3-4): identifies entirely absent strata.\n",
  "   These reflect source data gaps, not processing errors.\n\n",
  "3. Duplicate keys, invalid ISO weeks, and negative numeric values are\n",
  "   treated as potential technical errors.\n\n",
  "4. FLAG verification (Stage 10): Match=TRUE means pre-existing flag values\n",
  "   agree with independently computed expected values.\n\n",
  "5. RF_HDX (CHIRPS) and RF_NASA (IMERG) are from different products.\n",
  "   Do not combine them in direct numerical comparisons without harmonisation.\n\n",
  "6. FLAG_PLAUSIBILITY is advisory. Investigate flagged rows before treating\n",
  "   them as errors.\n",
  sep = ""
)


# =============================================================================
# EXPORT ALL OUTPUTS
# =============================================================================

section_header("EXPORTING OUTPUTS")

write_csv_safe(table_structure,              file.path(out_dir, "stage1_dataset_structure.csv"))
write_csv_safe(table_cell_miss,              file.path(out_dir, "stage2_cell_missingness.csv"))
write_csv_safe(table_year_cov,               file.path(out_dir, "stage3_year_coverage.csv"))
write_csv_safe(table_week_incompl,           file.path(out_dir, "stage4_week_incomplete.csv"))
write_csv_safe(table_dups,                   file.path(out_dir, "stage5_duplicates.csv"))
write_csv_safe(table_vd,                     file.path(out_dir, "stage6_value_domain.csv"))
write_csv_safe(flag_domain,                  file.path(out_dir, "stage6_flag_binary_check.csv"))
write_csv_safe(required_check_all,           file.path(out_dir, "stage7_required_columns.csv"))
write_csv_safe(schema_all,                   file.path(out_dir, "stage7_schema_classes.csv"))
write_csv_safe(wn_before,                    file.path(out_dir, "stage7_wn_type_before.csv"))
write_csv_safe(wn_after,                     file.path(out_dir, "stage7_wn_type_after.csv"))
write_csv_safe(table_temporal,               file.path(out_dir, "stage8_temporal_validity.csv"))
write_csv_safe(table_plaus,                  file.path(out_dir, "stage8_plausibility_summary.csv"))
write_csv_safe(plaus_qc$detail,              file.path(out_dir, "stage8_plausibility_qc_detail.csv"))
write_csv_safe(plaus_reg$detail,             file.path(out_dir, "stage8_plausibility_reg_detail.csv"))
write_csv_safe(cs$qc_annual,                file.path(out_dir, "stage9_qc_annual.csv"))
write_csv_safe(cs$reg_annual,               file.path(out_dir, "stage9_regional_annual.csv"))
write_csv_safe(cs$cty_annual,               file.path(out_dir, "stage9_country_annual.csv"))
write_csv_safe(cs$qc_seasonal,              file.path(out_dir, "stage9_qc_seasonal.csv"))
write_csv_safe(cs$reg_seasonal,             file.path(out_dir, "stage9_reg_seasonal.csv"))
write_csv_safe(cs$cty_seasonal,             file.path(out_dir, "stage9_cty_seasonal.csv"))
write_csv_safe(table_quality_flags,          file.path(out_dir, "stage10_quality_flag_table.csv"))
write_csv_safe(flag_verif$summary,           file.path(out_dir, "stage10_flag_verification.csv"))
write_csv_safe(flag_verif$terminal_gap_rows, file.path(out_dir, "stage10_terminal_gap_rows.csv"))
write_csv_safe(flag_dist,                    file.path(out_dir, "stage10_flag_distribution.csv"))
write_csv_safe(year_gaps_span,               file.path(out_dir, "support_year_gaps_within_span.csv"))
write_csv_safe(year_gaps_univ,               file.path(out_dir, "support_year_gaps_vs_universe.csv"))
write_csv_safe(week_summ_all,                file.path(out_dir, "support_week_completeness_full.csv"))
write_csv_safe(missing_wks_detail,           file.path(out_dir, "support_missing_weeks_detail.csv"))
write_csv_safe(dup_detail,                   file.path(out_dir, "support_duplicate_detail.csv"))
write_csv_safe(geo_regions,                  file.path(out_dir, "support_region_identifiers.csv"))
write_csv_safe(geo_countries,                file.path(out_dir, "support_country_identifiers.csv"))

validation_workbook <- list(
  "Stage1_Structure"           = table_structure,
  "Stage2_CellMissingness"     = table_cell_miss,
  "Stage3_YearCoverage"        = table_year_cov,
  "Stage4_WeekIncomplete"      = table_week_incompl,
  "Stage5_Duplicates"          = table_dups,
  "Stage6_ValueDomain"         = table_vd,
  "Stage6_FlagBinaryCheck"     = flag_domain,
  "Stage7_RequiredColumns"     = required_check_all,
  "Stage7_Schema"              = schema_all,
  "Stage7_WN_TypeBefore"       = wn_before,
  "Stage7_WN_TypeAfter"        = wn_after,
  "Stage8_TemporalValidity"    = table_temporal,
  "Stage8_PlausibilitySummary" = table_plaus,
  "Stage8_PlausibilityQC"      = plaus_qc$detail,
  "Stage8_PlausibilityReg"     = plaus_reg$detail,
  "Stage9_QC_Annual"           = cs$qc_annual,
  "Stage9_Regional_Annual"     = cs$reg_annual,
  "Stage9_Country_Annual"      = cs$cty_annual,
  "Stage9_QC_Seasonal"         = cs$qc_seasonal,
  "Stage9_Reg_Seasonal"        = cs$reg_seasonal,
  "Stage9_Cty_Seasonal"        = cs$cty_seasonal,
  "Stage10_QualityFlagTable"   = table_quality_flags,
  "Stage10_FlagVerification"   = flag_verif$summary,
  "Stage10_TerminalGapRows"    = flag_verif$terminal_gap_rows,
  "Stage10_FlagDistribution"   = flag_dist,
  "Support_YearGaps_Span"      = year_gaps_span,
  "Support_YearGaps_Universe"  = year_gaps_univ,
  "Support_WeekFullPanel"      = week_summ_all,
  "Support_MissingWeeks"       = missing_wks_detail,
  "Support_DuplicateDetail"    = dup_detail,
  "Support_RegionIDs"          = geo_regions,
  "Support_CountryIDs"         = geo_countries
)

write_xlsx(
  validation_workbook,
  path = file.path(out_dir, "technical_validation_report.xlsx")
)

cat("\nOutputs saved to: ", normalizePath(out_dir), "\n")
cat("  CSV files:       one per validation stage + supporting detail\n")
cat("  Master workbook: technical_validation_report.xlsx\n")


# =============================================================================
# REPRODUCIBILITY LOG
# =============================================================================

section_header("REPRODUCIBILITY LOG")

run_ts <- as.character(Sys.time())
cat("Input:     ", normalizePath(PATH),       "\n")
cat("Output:    ", normalizePath(out_dir),    "\n")
cat("Timestamp: ", run_ts,                    "\n")
cat("R version: ", R.version$version.string, "\n")

writeLines(
  c(
    "Technical Validation Run Log",
    "==============================",
    paste0("Script:    Dengue-Rainfall_Validation.R v2.0"),
    paste0("Input:     ", normalizePath(PATH)),
    paste0("Output:    ", normalizePath(out_dir)),
    paste0("Timestamp: ", run_ts),
    paste0("R:         ", R.version$version.string)
  ),
  con = file.path(out_dir, "run_log.txt")
)

sink(file.path(out_dir, "session_info.txt"))
cat("Timestamp:", run_ts, "\n\n")
sessionInfo()
sink()

cat("Run log:      ", file.path(out_dir, "run_log.txt"),      "\n")
cat("Session info: ", file.path(out_dir, "session_info.txt"), "\n")
cat("\nValidation workflow completed successfully.\n")
