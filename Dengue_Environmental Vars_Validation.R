# =============================================================================
# TECHNICAL VALIDATION WORKFLOW
# Weekly dengue incidence linked to meteorological variables at city, regional, 
# and multi-setting scales
# =============================================================================
#
# PURPOSE
#   Validate the complete environmental-variable workbook and quantify agreement
#   among environmental sources that measure the same construct.
#
# DATA SHEETS
#   1) QC Data
#   2) Regional Data
#   3) Multi-Setting Data
#
# ENVIRONMENTAL SOURCES PRESENT IN THE WORKBOOK
#   NASA GPM IMERG, ERA5-Land, PAGASA, CHIRPS/HDX, NASA MERRA-2
#
# IMPORTANT INTERPRETIVE RULES
#   1) Technical validation uses all rows in the workbook.
#   2) Primary analytic source comparisons exclude 2020, 2021, and 2025.
#   3) QC primary analytic years are 2013-2019 and 2022-2024.
#   4) RF_PAGASA and other PAGASA variables are reference observations, not
#      automatically ground truth. The workbook does not identify the station.
#   5) Cross-source agreement does not establish that either product is correct.
#   6) Cross-scale comparisons test representativeness, not interchangeable
#      measurement, because spatial support differs.
#   7) Structural completeness follows the Dataset Summary weekly-alignment note:
#      52 blocks per year with rare 53rd blocks folded into week 52. The Data
#      Dictionary separately labels WN as ISO week. Stage 1 reports this metadata
#      inconsistency rather than silently reconciling it.
#
# VALIDATION STAGES
#   Stage  1  Workbook, schema, dictionary, and environmental inventory
#   Stage  2  Row reconciliation, year coverage, and 52-week completeness
#   Stage  3  Environmental-variable coverage and missingness
#   Stage  4  Duplicate keys and temporal validity
#   Stage  5  Numeric and physical-domain checks
#   Stage  6  Internal ordering and derived-variable consistency checks
#   Stage  7  Data-quality flag verification
#   Stage  8  Dengue week-to-week plausibility checks
#   Stage  9  QC cross-source validation against PAGASA reference series
#   Stage 10  Multi-setting cross-source concordance
#   Stage 11  Year-specific stability of cross-source agreement
#   Stage 12  Leave-one-year-out cross-source calibration validation
#   Stage 13  Cross-scale representativeness checks within the Philippines
#   Stage 14  Lagged dengue-environment signal screen for all variables
#   Stage 15  Source-robust lag summary and final validation manifest
#
# RUN FROM COMMAND LINE
#   Rscript Dengue-Environmental_Validation_Script_v3.0.R \
#     "C:/path/Dengue_Environmental Vars_Dataset.xlsx" \
#     "C:/path/environmental_validation_outputs"
#
# REQUIRES
#   R >= 4.1.0
# =============================================================================


# =============================================================================
# 0. PACKAGE SETUP
# =============================================================================

if (getRversion() < "4.1.0") {
  stop("R 4.1.0 or newer is required.")
}

needed <- c("readxl", "dplyr", "tidyr", "purrr", "tibble", "writexl")
missing_pkgs <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(writexl)
})

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  tibble.print_max = Inf,
  tibble.width = Inf
)

set.seed(12345)


# =============================================================================
# 1. USER INPUT
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

PATH <- if (length(args) >= 1) {
  args[[1]]
} else {
  "Dengue_Environmental Vars_Dataset.xlsx"
}

out_dir <- if (length(args) >= 2) {
  args[[2]]
} else {
  "environmental_validation_outputs"
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

PRIMARY_EXCLUDE_YEARS <- c(2020L, 2021L, 2025L)
QC_PRIMARY_YEARS <- c(2013:2019, 2022:2024)
LAG_WEEKS <- 0:8
PLAUSIBILITY_THRESHOLD_PCT <- 500


# =============================================================================
# 2. UTILITY FUNCTIONS
# =============================================================================

section_header <- function(title) {
  cat("\n", strrep("=", 96), "\n", title, "\n", strrep("=", 96), "\n", sep = "")
}

print_full <- function(x, title = NULL) {
  if (!is.null(title)) {
    cat("\n", title, "\n", sep = "")
  }
  if (inherits(x, c("tbl_df", "tbl", "data.frame"))) {
    print(x, n = Inf, width = Inf)
  } else {
    print(x)
  }
}

safe_n_distinct <- function(x) {
  dplyr::n_distinct(x, na.rm = TRUE)
}

safe_mean <- function(x) {
  if (!length(x) || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  if (!length(x) || sum(!is.na(x)) < 2) NA_real_ else stats::sd(x, na.rm = TRUE)
}

safe_min <- function(x) {
  if (!length(x) || all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (!length(x) || all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

safe_sum <- function(x) {
  if (!length(x) || all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

safe_pct <- function(num, den, digits = 3) {
  if (length(den) != 1 || is.na(den) || den == 0) {
    NA_real_
  } else {
    round(100 * num / den, digits)
  }
}

safe_cor <- function(x, y, method = "pearson") {
  ok <- stats::complete.cases(x, y)
  x <- as.numeric(x[ok])
  y <- as.numeric(y[ok])
  if (length(x) < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(NA_real_)
  }
  suppressWarnings(stats::cor(x, y, method = method))
}

safe_cor_p <- function(x, y, method = "pearson") {
  ok <- stats::complete.cases(x, y)
  x <- as.numeric(x[ok])
  y <- as.numeric(y[ok])
  if (length(x) < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(NA_real_)
  }
  out <- tryCatch(
    suppressWarnings(stats::cor.test(x, y, method = method, exact = FALSE)$p.value),
    error = function(e) NA_real_
  )
  out
}

trim_blank_to_na <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

clean_col_names <- function(x) {
  x <- trimws(as.character(x))

  # Remove invisible spacing characters sometimes retained from Excel headers.
  x <- gsub("[\u00A0\u200B\u200C\u200D\uFEFF]", "", x, perl = TRUE)

  # Normalize Unicode dash variants to the ordinary ASCII hyphen.
  x <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2212]", "-", x, perl = TRUE)

  # Canonicalize ERA5-Land if an import/name-repair step has represented the
  # separator as a period, underscore, space, or hyphen.
  x <- gsub("ERA5[._ -]?Land", "ERA5-Land", x, perl = TRUE)

  # Some workbook versions use the shorter suffix "_ERA5" rather than
  # "_ERA5-Land". Normalize those headers to the canonical names used
  # throughout this script. This covers, for example:
  # Temp_ERA5 -> Temp_ERA5-Land
  # RH_ERA5 -> RH_ERA5-Land
  # Pressure_ERA5 -> Pressure_ERA5-Land
  # SH_ERA5 -> SH_ERA5-Land
  # RF_ERA5 -> RF_ERA5-Land
  x <- sub("_ERA5$", "_ERA5-Land", x, perl = TRUE)

  x
}

format_yw <- function(year, week) {
  ifelse(is.na(year) | is.na(week), NA_character_, sprintf("%04d-W%02d", year, week))
}

write_csv_safe <- function(df, filename) {
  utils::write.csv(df, file = file.path(out_dir, filename), row.names = FALSE, na = "")
}

collapse_ints <- function(x) {
  x <- sort(unique(as.integer(x[!is.na(x)])))
  if (length(x) == 0) "None" else paste(x, collapse = ", ")
}

find_header_row <- function(path, sheet, anchors, max_scan = 8L) {
  raw <- suppressMessages(
    readxl::read_excel(path, sheet = sheet, col_names = FALSE, n_max = max_scan)
  )
  for (i in seq_len(nrow(raw))) {
    vals <- clean_col_names(unlist(raw[i, ], use.names = FALSE))
    if (all(anchors %in% vals)) {
      return(i)
    }
  }
  NA_integer_
}

load_sheet <- function(path, sheet, anchors) {
  header_row <- find_header_row(path, sheet, anchors)
  if (is.na(header_row)) {
    stop(
      "Could not find the header row in sheet '", sheet,
      "'. Expected anchors: ", paste(anchors, collapse = ", ")
    )
  }
  out <- suppressMessages(
    readxl::read_excel(
      path,
      sheet = sheet,
      skip = header_row - 1,
      .name_repair = "minimal"
    )
  )
  names(out) <- clean_col_names(names(out))

  # Keep every explicitly named workbook column, even when all values are NA.
  # The previous select(where(~ !all(is.na(.)))) could remove a legitimate
  # header before required_column_check(), causing a false "missing column" error.
  # Remove only truly blank Excel columns auto-named by readxl as ...1, ...2, etc.
  auto_blank <- grepl("^\\.\\.\\.[0-9]+$", names(out)) &
    vapply(out, function(x) all(is.na(x)), logical(1))
  if (any(auto_blank)) {
    out <- out[, !auto_blank, drop = FALSE]
  }

  attr(out, "header_row_used") <- header_row
  out
}

required_column_check <- function(df, dataset, required_cols) {
  tibble(
    Dataset = dataset,
    Required_Column = required_cols,
    Present = required_cols %in% names(df)
  )
}

schema_check <- function(df, dataset) {
  tibble(
    Dataset = dataset,
    Variable = names(df),
    Class = vapply(df, function(x) paste(class(x), collapse = ", "), character(1))
  )
}

row_reconciliation_52 <- function(df, dataset, geo_var = NULL) {
  geo <- if (is.null(geo_var)) rep("ALL", nrow(df)) else trim_blank_to_na(df[[geo_var]])
  x <- tibble(
    Geography = geo,
    YR = as.integer(df$YR),
    WN = as.integer(df$WN)
  ) %>%
    filter(!is.na(Geography), !is.na(YR), !is.na(WN))

  unique_keys <- x %>% distinct(Geography, YR, WN)
  observed_strata <- unique_keys %>% distinct(Geography, YR)
  expected <- nrow(observed_strata) * 52L

  tibble(
    Dataset = dataset,
    Observed_Rows = nrow(df),
    Unique_Key_Rows = nrow(unique_keys),
    Observed_Geography_Years = nrow(observed_strata),
    Expected_Rows_at_52_Weeks = expected,
    Missing_Week_Rows = expected - nrow(unique_keys)
  )
}

year_coverage <- function(df, dataset, geo_var = NULL) {
  geo <- if (is.null(geo_var)) rep("ALL", nrow(df)) else trim_blank_to_na(df[[geo_var]])
  x <- tibble(Geography = geo, YR = as.integer(df$YR)) %>%
    filter(!is.na(Geography), !is.na(YR)) %>%
    distinct()

  if (nrow(x) == 0) {
    return(tibble(
      Dataset = character(), Geography = character(), Min_Year = integer(),
      Max_Year = integer(), Observed_Years_n = integer(), Missing_Within_Span = character()
    ))
  }

  x %>%
    group_by(Geography) %>%
    summarise(
      Min_Year = min(YR),
      Max_Year = max(YR),
      Observed_Years_n = n_distinct(YR),
      Missing_Within_Span = collapse_ints(setdiff(seq(min(YR), max(YR)), unique(YR))),
      .groups = "drop"
    ) %>%
    mutate(Dataset = dataset, .before = 1)
}

week_completeness_52 <- function(df, dataset, geo_var = NULL) {
  geo <- if (is.null(geo_var)) rep("ALL", nrow(df)) else trim_blank_to_na(df[[geo_var]])
  obs <- tibble(
    Geography = geo,
    YR = as.integer(df$YR),
    WN = as.integer(df$WN)
  ) %>%
    filter(!is.na(Geography), !is.na(YR), !is.na(WN)) %>%
    distinct()

  strata <- obs %>% distinct(Geography, YR)
  if (nrow(strata) == 0) {
    return(list(summary = tibble(), missing = tibble()))
  }

  expected <- strata %>%
    tidyr::crossing(WN = 1:52)

  missing <- expected %>%
    anti_join(obs, by = c("Geography", "YR", "WN")) %>%
    mutate(Dataset = dataset, .before = 1) %>%
    rename(Missing_Week = WN) %>%
    arrange(Geography, YR, Missing_Week)

  summary <- expected %>%
    count(Geography, YR, name = "Expected_Weeks") %>%
    left_join(obs %>% count(Geography, YR, name = "Observed_Weeks"), by = c("Geography", "YR")) %>%
    mutate(
      Observed_Weeks = dplyr::coalesce(Observed_Weeks, 0L),
      Missing_Weeks_n = Expected_Weeks - Observed_Weeks,
      Complete_52_Weeks = Missing_Weeks_n == 0L,
      Dataset = dataset,
      .before = 1
    ) %>%
    arrange(Geography, YR)

  list(summary = summary, missing = missing)
}

duplicate_summary <- function(df, dataset, keys) {
  dups <- df %>%
    count(across(all_of(keys)), name = "n_records") %>%
    filter(n_records > 1)

  tibble(
    Dataset = dataset,
    Key = paste(keys, collapse = " + "),
    Duplicate_Key_Groups = nrow(dups),
    Excess_Duplicate_Rows = if (nrow(dups) == 0) 0L else sum(dups$n_records - 1L)
  )
}

duplicate_detail <- function(df, dataset, keys) {
  df %>%
    count(across(all_of(keys)), name = "n_records") %>%
    filter(n_records > 1) %>%
    mutate(Dataset = dataset, .before = 1)
}

env_coverage <- function(df, dataset, env_vars) {
  bind_rows(lapply(env_vars, function(v) {
    x <- df[[v]]
    observed <- which(!is.na(x))
    first_idx <- if (length(observed) == 0) NA_integer_ else observed[[1]]
    last_idx <- if (length(observed) == 0) NA_integer_ else observed[[length(observed)]]

    tibble(
      Dataset = dataset,
      Variable = v,
      Rows = nrow(df),
      Observed_n = sum(!is.na(x)),
      Missing_n = sum(is.na(x)),
      Coverage_Pct = safe_pct(sum(!is.na(x)), nrow(df)),
      First_Observed = if (is.na(first_idx)) NA_character_ else format_yw(df$YR[first_idx], df$WN[first_idx]),
      Last_Observed = if (is.na(last_idx)) NA_character_ else format_yw(df$YR[last_idx], df$WN[last_idx])
    )
  }))
}

range_check <- function(df, dataset, variable, lower = -Inf, upper = Inf, allow_na = TRUE) {
  x <- as.numeric(df[[variable]])
  tibble(
    Dataset = dataset,
    Variable = variable,
    Lower_Bound = lower,
    Upper_Bound = upper,
    Min_Observed = safe_min(x),
    Max_Observed = safe_max(x),
    Missing_n = sum(is.na(x)),
    Below_Bound_n = sum(!is.na(x) & x < lower),
    Above_Bound_n = sum(!is.na(x) & x > upper),
    Pass = (sum(!is.na(x) & x < lower) == 0L) &&
      (sum(!is.na(x) & x > upper) == 0L) &&
      (allow_na || sum(is.na(x)) == 0L)
  )
}

binary_flag_check <- function(df, dataset, flag_var, allow_na = FALSE) {
  x <- df[[flag_var]]
  bad <- sum(!is.na(x) & !(x %in% c(0, 1)))
  na_n <- sum(is.na(x))
  tibble(
    Dataset = dataset,
    Flag = flag_var,
    Non_Binary_n = bad,
    Missing_n = na_n,
    Pass = bad == 0L && (allow_na || na_n == 0L)
  )
}

ordering_check <- function(df, dataset, lower_var, middle_var, upper_var, check_name) {
  ok <- stats::complete.cases(df[[lower_var]], df[[middle_var]], df[[upper_var]])
  lower <- as.numeric(df[[lower_var]][ok])
  middle <- as.numeric(df[[middle_var]][ok])
  upper <- as.numeric(df[[upper_var]][ok])
  violations <- sum(!(lower <= middle & middle <= upper))

  tibble(
    Dataset = dataset,
    Check = check_name,
    Rows_Evaluated = length(lower),
    Violations_n = violations,
    Pass = violations == 0L
  )
}

actual_vs_saturation_check <- function(df, dataset, actual_var, sat_var, check_name) {
  ok <- stats::complete.cases(df[[actual_var]], df[[sat_var]])
  actual <- as.numeric(df[[actual_var]][ok])
  sat <- as.numeric(df[[sat_var]][ok])
  violations <- sum(actual > sat)
  tibble(
    Dataset = dataset,
    Check = check_name,
    Rows_Evaluated = length(actual),
    Violations_n = violations,
    Pass = violations == 0L
  )
}

approx_derived_consistency <- function(df, dataset, temp_var, rh_var, pressure_var,
                                       sat_var, act_var, sh_var, source_label) {
  temp <- as.numeric(df[[temp_var]])
  rh <- as.numeric(df[[rh_var]])
  pressure <- as.numeric(df[[pressure_var]])

  es_approx <- 6.112 * exp((17.67 * temp) / (temp + 243.5))
  ea_approx <- es_approx * rh / 100
  sh_approx <- 1000 * 0.622 * ea_approx / (pressure - 0.378 * ea_approx)

  compare_one <- function(stored, approx, variable) {
    ok <- stats::complete.cases(stored, approx)
    stored <- as.numeric(stored[ok])
    approx <- as.numeric(approx[ok])
    n <- length(stored)
    tibble(
      Dataset = dataset,
      Source = source_label,
      Variable = variable,
      N = n,
      Pearson_r = if (n >= 3) safe_cor(stored, approx, "pearson") else NA_real_,
      Mean_Difference_Stored_minus_Approx = if (n == 0) NA_real_ else mean(stored - approx),
      MAE = if (n == 0) NA_real_ else mean(abs(stored - approx)),
      RMSE = if (n == 0) NA_real_ else sqrt(mean((stored - approx)^2)),
      Interpretation = paste0(
        "Approximation uses weekly mean inputs. Stored values were derived from daily inputs before weekly averaging, ",
        "so exact equality is not expected."
      )
    )
  }

  bind_rows(
    compare_one(df[[sat_var]], es_approx, sat_var),
    compare_one(df[[act_var]], ea_approx, act_var),
    compare_one(df[[sh_var]], sh_approx, sh_var)
  )
}

plausibility_check <- function(df, dataset, case_var, geo_var = NULL,
                               threshold_pct = PLAUSIBILITY_THRESHOLD_PCT) {
  geo <- if (is.null(geo_var)) rep("ALL", nrow(df)) else trim_blank_to_na(df[[geo_var]])
  x <- df %>%
    mutate(
      .geo = geo,
      .cases = as.numeric(.data[[case_var]])
    ) %>%
    arrange(.geo, YR, WN) %>%
    group_by(.geo) %>%
    mutate(
      .cases_lag = lag(.cases),
      .yr_lag = lag(YR),
      .wow_pct = ifelse(
        !is.na(.cases_lag) & .cases_lag != 0 & !is.na(.yr_lag) & (YR - .yr_lag <= 1),
        100 * (.cases - .cases_lag) / .cases_lag,
        NA_real_
      ),
      .flag = !is.na(.wow_pct) & abs(.wow_pct) > threshold_pct
    ) %>%
    ungroup()

  detail <- x %>%
    filter(.flag) %>%
    transmute(
      Dataset = dataset,
      Geography = .geo,
      YR, WN,
      Previous_Cases = .cases_lag,
      Cases = .cases,
      WoW_Pct = .wow_pct,
      Threshold_Pct = threshold_pct
    )

  summary <- tibble(
    Dataset = dataset,
    Case_Variable = case_var,
    Rows_Evaluated = sum(!is.na(x$.wow_pct)),
    Flagged_Rows = sum(x$.flag, na.rm = TRUE),
    Flag_Rate_Pct = safe_pct(sum(x$.flag, na.rm = TRUE), sum(!is.na(x$.wow_pct))),
    Threshold_Pct = threshold_pct
  )

  list(summary = summary, detail = detail)
}

pair_metrics <- function(x, y, pair_id, series_a, series_b, unit,
                         scope, group_label, note = "") {
  ok <- stats::complete.cases(x, y)
  x <- as.numeric(x[ok])
  y <- as.numeric(y[ok])
  n <- length(x)

  empty <- tibble(
    Pair_ID = pair_id,
    Scope = scope,
    Group = group_label,
    Series_A = series_a,
    Series_B = series_b,
    Unit = unit,
    N = n,
    Mean_A = NA_real_, Mean_B = NA_real_,
    Pearson_r = NA_real_, Pearson_p = NA_real_, Spearman_rho = NA_real_,
    Bias_A_minus_B = NA_real_, Relative_Bias_Pct = NA_real_,
    MAE = NA_real_, RMSE = NA_real_, CCC = NA_real_, NSE = NA_real_, KGE = NA_real_,
    BA_Lower_95 = NA_real_, BA_Upper_95 = NA_real_,
    Regression_Intercept_B_on_A = NA_real_, Regression_Slope_B_on_A = NA_real_,
    Regression_R2 = NA_real_, Note = note
  )

  if (n < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(empty)
  }

  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- stats::var(x)
  var_y <- stats::var(y)
  cov_xy <- stats::cov(x, y)

  pearson_r <- safe_cor(x, y, "pearson")
  pearson_p <- safe_cor_p(x, y, "pearson")
  spearman_rho <- safe_cor(x, y, "spearman")

  diff <- x - y
  bias <- mean(diff)
  mae <- mean(abs(diff))
  rmse <- sqrt(mean(diff^2))

  ccc_den <- var_x + var_y + (mean_x - mean_y)^2
  ccc <- if (is.na(ccc_den) || ccc_den == 0) NA_real_ else (2 * cov_xy) / ccc_den

  nse_den <- sum((y - mean_y)^2)
  nse <- if (nse_den == 0) NA_real_ else 1 - sum((x - y)^2) / nse_den

  alpha <- if (stats::sd(y) == 0) NA_real_ else stats::sd(x) / stats::sd(y)
  beta <- if (mean_y == 0) NA_real_ else mean_x / mean_y
  kge <- if (any(is.na(c(pearson_r, alpha, beta)))) {
    NA_real_
  } else {
    1 - sqrt((pearson_r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
  }

  diff_sd <- stats::sd(diff)
  ba_lo <- bias - 1.96 * diff_sd
  ba_hi <- bias + 1.96 * diff_sd

  model <- tryCatch(stats::lm(y ~ x), error = function(e) NULL)
  if (is.null(model)) {
    intercept <- slope <- r2 <- NA_real_
  } else {
    cf <- stats::coef(model)
    intercept <- unname(cf[[1]])
    slope <- unname(cf[[2]])
    r2 <- summary(model)$r.squared
  }

  tibble(
    Pair_ID = pair_id,
    Scope = scope,
    Group = group_label,
    Series_A = series_a,
    Series_B = series_b,
    Unit = unit,
    N = n,
    Mean_A = mean_x,
    Mean_B = mean_y,
    Pearson_r = pearson_r,
    Pearson_p = pearson_p,
    Spearman_rho = spearman_rho,
    Bias_A_minus_B = bias,
    Relative_Bias_Pct = if (mean_y == 0) NA_real_ else 100 * bias / mean_y,
    MAE = mae,
    RMSE = rmse,
    CCC = ccc,
    NSE = nse,
    KGE = kge,
    BA_Lower_95 = ba_lo,
    BA_Upper_95 = ba_hi,
    Regression_Intercept_B_on_A = intercept,
    Regression_Slope_B_on_A = slope,
    Regression_R2 = r2,
    Note = note
  )
}

apply_pair_filter <- function(df, rule) {
  if (is.na(rule) || rule == "none") {
    return(df)
  }
  if (rule == "merra2_sh_strict") {
    return(df %>% filter(!is.na(FLAG_MERRA2_SH_LOW_COVERAGE), FLAG_MERRA2_SH_LOW_COVERAGE == 0))
  }
  stop("Unknown pair filter rule: ", rule)
}

run_pair_metrics <- function(df, pair_defs, scope_label, group_col = NULL,
                             pooled_label = "ALL") {
  out <- list()
  z <- 0L

  for (i in seq_len(nrow(pair_defs))) {
    p <- pair_defs[i, ]
    d <- apply_pair_filter(df, p$Filter_Rule)

    if (is.null(group_col)) {
      z <- z + 1L
      out[[z]] <- pair_metrics(
        d[[p$Series_A]], d[[p$Series_B]], p$Pair_ID, p$Series_A, p$Series_B,
        p$Unit, scope_label, pooled_label, p$Note
      )
    } else {
      groups <- sort(unique(d[[group_col]][!is.na(d[[group_col]])]))
      for (g in groups) {
        dg <- d[d[[group_col]] == g, , drop = FALSE]
        z <- z + 1L
        out[[z]] <- pair_metrics(
          dg[[p$Series_A]], dg[[p$Series_B]], p$Pair_ID, p$Series_A, p$Series_B,
          p$Unit, scope_label, as.character(g), p$Note
        )
      }
    }
  }

  bind_rows(out)
}

run_pair_metrics_by_year <- function(df, pair_defs, scope_label, group_col = NULL,
                                     min_pairs = 10L) {
  out <- list()
  z <- 0L

  for (i in seq_len(nrow(pair_defs))) {
    p <- pair_defs[i, ]
    d <- apply_pair_filter(df, p$Filter_Rule)
    groups <- if (is.null(group_col)) "ALL" else sort(unique(d[[group_col]][!is.na(d[[group_col]])]))

    for (g in groups) {
      dg <- if (is.null(group_col)) d else d[d[[group_col]] == g, , drop = FALSE]
      years <- sort(unique(dg$YR[!is.na(dg$YR)]))
      for (yr in years) {
        dy <- dg[dg$YR == yr, , drop = FALSE]
        ok <- stats::complete.cases(dy[[p$Series_A]], dy[[p$Series_B]])
        n_ok <- sum(ok)

        if (n_ok < min_pairs) {
          z <- z + 1L
          out[[z]] <- tibble(
            Pair_ID = p$Pair_ID, Scope = scope_label, Group = as.character(g), YR = yr,
            Series_A = p$Series_A, Series_B = p$Series_B, N = n_ok,
            Pearson_r = NA_real_, Spearman_rho = NA_real_, Bias_A_minus_B = NA_real_,
            MAE = NA_real_, RMSE = NA_real_, CCC = NA_real_, Stable_n = FALSE
          )
        } else {
          m <- pair_metrics(
            dy[[p$Series_A]], dy[[p$Series_B]], p$Pair_ID, p$Series_A, p$Series_B,
            p$Unit, scope_label, as.character(g), p$Note
          )
          z <- z + 1L
          out[[z]] <- m %>%
            transmute(
              Pair_ID, Scope, Group, YR = yr, Series_A, Series_B, N,
              Pearson_r, Spearman_rho, Bias_A_minus_B, MAE, RMSE, CCC,
              Stable_n = TRUE
            )
        }
      }
    }
  }

  bind_rows(out)
}

loyo_calibration_cv <- function(df, pair_defs, scope_label, group_col = NULL,
                                min_train = 20L, min_test = 5L) {
  out <- list()
  z <- 0L

  for (i in seq_len(nrow(pair_defs))) {
    p <- pair_defs[i, ]
    d <- apply_pair_filter(df, p$Filter_Rule)
    groups <- if (is.null(group_col)) "ALL" else sort(unique(d[[group_col]][!is.na(d[[group_col]])]))

    for (g in groups) {
      dg <- if (is.null(group_col)) d else d[d[[group_col]] == g, , drop = FALSE]
      years <- sort(unique(dg$YR[!is.na(dg$YR)]))

      for (holdout in years) {
        train <- dg[dg$YR != holdout, , drop = FALSE]
        test <- dg[dg$YR == holdout, , drop = FALSE]

        train_ok <- stats::complete.cases(train[[p$Series_A]], train[[p$Series_B]])
        test_ok <- stats::complete.cases(test[[p$Series_A]], test[[p$Series_B]])
        train <- train[train_ok, , drop = FALSE]
        test <- test[test_ok, , drop = FALSE]

        if (nrow(train) < min_train || nrow(test) < min_test ||
            stats::sd(train[[p$Series_A]]) == 0 || stats::sd(train[[p$Series_B]]) == 0) {
          z <- z + 1L
          out[[z]] <- tibble(
            Pair_ID = p$Pair_ID,
            Scope = scope_label,
            Group = as.character(g),
            Holdout_Year = holdout,
            Train_n = nrow(train),
            Test_n = nrow(test),
            Train_Intercept = NA_real_,
            Train_Slope = NA_real_,
            Test_Bias_Pred_minus_B = NA_real_,
            Test_MAE = NA_real_,
            Test_RMSE = NA_real_,
            Test_CV_R2 = NA_real_,
            Test_Pearson_r = NA_real_,
            Valid_Holdout = FALSE
          )
          next
        }

        train_a <- as.numeric(train[[p$Series_A]])
        train_b <- as.numeric(train[[p$Series_B]])
        test_a <- as.numeric(test[[p$Series_A]])
        test_b <- as.numeric(test[[p$Series_B]])

        fit <- stats::lm(train_b ~ train_a)
        cf <- stats::coef(fit)
        pred <- unname(cf[[1]]) + unname(cf[[2]]) * test_a
        err <- pred - test_b
        sst <- sum((test_b - mean(test_b))^2)
        cv_r2 <- if (sst == 0) NA_real_ else 1 - sum(err^2) / sst

        z <- z + 1L
        out[[z]] <- tibble(
          Pair_ID = p$Pair_ID,
          Scope = scope_label,
          Group = as.character(g),
          Holdout_Year = holdout,
          Train_n = nrow(train),
          Test_n = nrow(test),
          Train_Intercept = unname(cf[[1]]),
          Train_Slope = unname(cf[[2]]),
          Test_Bias_Pred_minus_B = mean(err),
          Test_MAE = mean(abs(err)),
          Test_RMSE = sqrt(mean(err^2)),
          Test_CV_R2 = cv_r2,
          Test_Pearson_r = safe_cor(pred, test_b, "pearson"),
          Valid_Holdout = TRUE
        )
      }
    }
  }

  bind_rows(out)
}

summarise_loyo <- function(cv) {
  if (nrow(cv) == 0) {
    return(tibble())
  }
  cv %>%
    filter(Valid_Holdout) %>%
    group_by(Pair_ID, Scope, Group) %>%
    summarise(
      Holdout_Years_n = n(),
      Total_Test_n = sum(Test_n),
      Weighted_RMSE = sqrt(sum((Test_RMSE^2) * Test_n) / sum(Test_n)),
      Weighted_MAE = sum(Test_MAE * Test_n) / sum(Test_n),
      Weighted_Abs_Bias = sum(abs(Test_Bias_Pred_minus_B) * Test_n) / sum(Test_n),
      Median_CV_R2 = stats::median(Test_CV_R2, na.rm = TRUE),
      Min_CV_R2 = safe_min(Test_CV_R2),
      Max_CV_R2 = safe_max(Test_CV_R2),
      .groups = "drop"
    )
}

cross_scale_metric <- function(df_a, df_b, var_a, var_b, pair_id, unit,
                               scope, label_a, label_b, note) {
  a <- df_a %>% transmute(YR, WN, A = .data[[var_a]])
  b <- df_b %>% transmute(YR, WN, B = .data[[var_b]])
  joined <- inner_join(a, b, by = c("YR", "WN"))
  pair_metrics(
    joined$A, joined$B, pair_id, label_a, label_b, unit,
    scope, "PHILIPPINES", note
  )
}

lagged_environment_association <- function(df, dataset, case_var, env_vars,
                                           lags = 0:8, geo_var = NULL,
                                           quality_note = "") {
  out <- list()
  z <- 0L
  geos <- if (is.null(geo_var)) "ALL" else sort(unique(df[[geo_var]][!is.na(df[[geo_var]])]))

  for (g in geos) {
    dg <- if (is.null(geo_var)) df else df[df[[geo_var]] == g, , drop = FALSE]
    for (v in env_vars) {
      for (k in lags) {
        dx <- dg %>%
          arrange(YR, WN) %>%
          group_by(YR) %>%
          mutate(.env_lag = dplyr::lag(as.numeric(.data[[v]]), n = k)) %>%
          ungroup()

        ok <- stats::complete.cases(dx[[case_var]], dx$.env_lag)
        cases <- as.numeric(dx[[case_var]][ok])
        envlag <- as.numeric(dx$.env_lag[ok])

        z <- z + 1L
        out[[z]] <- tibble(
          Dataset = dataset,
          Geography = as.character(g),
          Variable = v,
          Lag_Weeks = k,
          N = length(cases),
          Pearson_r = if (length(cases) >= 3) safe_cor(cases, envlag, "pearson") else NA_real_,
          Spearman_rho = if (length(cases) >= 3) safe_cor(cases, envlag, "spearman") else NA_real_,
          Note = quality_note
        )
      }
    }
  }

  bind_rows(out)
}

best_lag_summary <- function(lag_table) {
  lag_table %>%
    group_by(Dataset, Geography, Variable) %>%
    filter(!is.na(Spearman_rho)) %>%
    arrange(desc(abs(Spearman_rho)), Lag_Weeks) %>%
    slice(1) %>%
    ungroup() %>%
    rename(
      Best_Lag_Weeks = Lag_Weeks,
      Best_Pearson_r = Pearson_r,
      Best_Spearman_rho = Spearman_rho
    )
}


# =============================================================================
# 3. INPUT FILE AND WORKBOOK CHECK
# =============================================================================

section_header("INPUT FILE AND WORKBOOK CHECK")

if (!file.exists(PATH)) {
  stop("Input workbook not found: ", PATH)
}

available_sheets <- readxl::excel_sheets(PATH)
required_sheets <- c("Dataset Summary", "Data Dictionary", "QC Data", "Regional Data", "Multi-Setting Data")
missing_sheets <- setdiff(required_sheets, available_sheets)

if (length(missing_sheets) > 0) {
  stop("Missing required sheet(s): ", paste(missing_sheets, collapse = ", "))
}

cat("Input workbook: ", normalizePath(PATH), "\n", sep = "")
cat("Sheets: ", paste(available_sheets, collapse = ", "), "\n", sep = "")


# =============================================================================
# 4. LOAD DATA AND DICTIONARY
# =============================================================================

section_header("LOADING DATA")

df_qc <- load_sheet(PATH, "QC Data", c("YR", "WN"))
df_reg <- load_sheet(PATH, "Regional Data", c("YR", "WN"))
df_ms <- load_sheet(PATH, "Multi-Setting Data", c("YR", "WN"))
dict <- load_sheet(PATH, "Data Dictionary", c("CODE", "Variable Name"))
summary_raw <- suppressMessages(readxl::read_excel(PATH, sheet = "Dataset Summary", col_names = FALSE))

hdr_qc <- attr(df_qc, "header_row_used")
hdr_reg <- attr(df_reg, "header_row_used")
hdr_ms <- attr(df_ms, "header_row_used")
hdr_dict <- attr(dict, "header_row_used")

required_dict_cols <- c("CODE", "Variable Name", "Description", "Source Sheet", "Data Source", "Unit of Measurement")
if (!all(required_dict_cols %in% names(dict))) {
  stop("Data Dictionary is missing required metadata columns.")
}

dict <- dict %>%
  rename(
    Code = CODE,
    Variable_Name = `Variable Name`,
    Source_Sheet = `Source Sheet`,
    Data_Source = `Data Source`,
    Unit = `Unit of Measurement`
  )

cat(sprintf(
  "Rows loaded: QC = %d | Regional = %d | Multi-Setting = %d | Total = %d\n",
  nrow(df_qc), nrow(df_reg), nrow(df_ms), nrow(df_qc) + nrow(df_reg) + nrow(df_ms)
))
cat(sprintf(
  "Header rows used: QC = %d | Regional = %d | Multi-Setting = %d | Dictionary = %d\n",
  hdr_qc, hdr_reg, hdr_ms, hdr_dict
))


# =============================================================================
# 5. COLUMN DEFINITIONS AND TYPE COERCION
# =============================================================================

case_qc <- "DC_QC"
case_reg <- "DC_DOH"
case_ms <- "DC_OPENDENGUE"

env_qc <- c(
  "RF_NASA", "RF_PAGASA",
  "Temp_ERA5-Land", "TempMax_ERA5-Land", "TempMin_ERA5-Land",
  "RH_ERA5-Land", "RHMax_ERA5-Land", "RHMin_ERA5-Land",
  "Pressure_ERA5-Land", "VaporPressureSat_ERA5-Land", "VaporPressureAct_ERA5-Land", "SH_ERA5-Land",
  "Temp_PAGASA", "RH_PAGASA",
  "VaporPressureSat_PAGASA", "VaporPressureAct_PAGASA", "SH_PAGASA"
)

env_reg <- c("RF_HDX", "Temp_ERA5-Land", "SH_ERA5-Land")

env_ms <- c(
  "RF_NASA", "RF_ERA5-Land", "Temp_ERA5-Land", "SH_ERA5-Land",
  "Temp_MERRA2", "TempMax_MERRA2", "TempMin_MERRA2", "SH_MERRA2"
)

flag_qc <- c("FLAG_COVID", "FLAG_SINGLE_CELL_RF", "FLAG_PLAUSIBILITY", "FLAG_PRESSURE_SH_GAP")
flag_reg <- c("FLAG_DEKADAL_APPROX", "FLAG_PLAUSIBILITY")
flag_ms <- c("FLAG_SINGLE_CELL_RF", "FLAG_TERMINAL_GAP", "FLAG_MERRA2_SH_LOW_COVERAGE")

required_qc <- c("YR", "WN", case_qc, env_qc, flag_qc)
required_reg <- c("REGION", "YR", "WN", case_reg, env_reg, flag_reg)
required_ms <- c("SETTING", "YR", "WN", case_ms, env_ms, flag_ms)

required_check <- bind_rows(
  required_column_check(df_qc, "QC Data", required_qc),
  required_column_check(df_reg, "Regional Data", required_reg),
  required_column_check(df_ms, "Multi-Setting Data", required_ms)
)

if (!all(required_check$Present)) {
  print_full(required_check %>% filter(!Present), "Missing required columns:")
  cat("\nActual QC Data columns:\n", paste(names(df_qc), collapse = " | "), "\n", sep = "")
  cat("\nActual Regional Data columns:\n", paste(names(df_reg), collapse = " | "), "\n", sep = "")
  cat("\nActual Multi-Setting Data columns:\n", paste(names(df_ms), collapse = " | "), "\n", sep = "")
  stop("Required columns are missing. Validation stopped before type coercion.")
}

numeric_qc <- c("YR", "WN", case_qc, env_qc, flag_qc)
numeric_reg <- c("YR", "WN", case_reg, env_reg, flag_reg)
numeric_ms <- c("YR", "WN", case_ms, env_ms, flag_ms)

schema_before <- bind_rows(
  schema_check(df_qc, "QC Data"),
  schema_check(df_reg, "Regional Data"),
  schema_check(df_ms, "Multi-Setting Data")
)

df_qc <- df_qc %>%
  mutate(across(all_of(numeric_qc), as.numeric)) %>%
  mutate(YR = as.integer(YR), WN = as.integer(WN), DC_QC = as.integer(DC_QC),
         across(all_of(flag_qc), as.integer))

df_reg <- df_reg %>%
  mutate(REGION = trim_blank_to_na(REGION)) %>%
  mutate(across(all_of(numeric_reg), as.numeric)) %>%
  mutate(YR = as.integer(YR), WN = as.integer(WN), DC_DOH = as.integer(DC_DOH),
         across(all_of(flag_reg), as.integer))

df_ms <- df_ms %>%
  mutate(SETTING = trim_blank_to_na(SETTING)) %>%
  mutate(across(all_of(numeric_ms), as.numeric)) %>%
  mutate(YR = as.integer(YR), WN = as.integer(WN), DC_OPENDENGUE = as.integer(DC_OPENDENGUE),
         across(all_of(flag_ms), as.integer))

schema_after <- bind_rows(
  schema_check(df_qc, "QC Data"),
  schema_check(df_reg, "Regional Data"),
  schema_check(df_ms, "Multi-Setting Data")
)

qc_primary <- df_qc %>% filter(YR %in% QC_PRIMARY_YEARS)
reg_primary <- df_reg %>% filter(!(YR %in% PRIMARY_EXCLUDE_YEARS))
ms_primary <- df_ms %>% filter(!(YR %in% PRIMARY_EXCLUDE_YEARS))

# For lagged SH_MERRA2 analyses, retain only weeks with at least 4 valid source days.
ms_primary_lag <- ms_primary
ms_primary_lag$SH_MERRA2[
  is.na(ms_primary_lag$FLAG_MERRA2_SH_LOW_COVERAGE) |
    ms_primary_lag$FLAG_MERRA2_SH_LOW_COVERAGE != 0
] <- NA_real_


# =============================================================================
# STAGE 1. WORKBOOK, SCHEMA, DICTIONARY, AND ENVIRONMENTAL INVENTORY
# =============================================================================

section_header("STAGE 1. WORKBOOK, SCHEMA, DICTIONARY, AND ENVIRONMENTAL INVENTORY")

expected_env_map <- bind_rows(
  tibble(Dataset = "QC Data", Variable = env_qc),
  tibble(Dataset = "Regional Data", Variable = env_reg),
  tibble(Dataset = "Multi-Setting Data", Variable = env_ms)
)

env_inventory <- expected_env_map %>%
  left_join(
    dict %>% select(Code, Variable_Name, Description, Source_Sheet, Data_Source, Unit),
    by = c("Variable" = "Code")
  ) %>%
  mutate(
    Dictionary_Metadata_Present = !is.na(Variable_Name),
    Workbook_Column_Present = case_when(
      Dataset == "QC Data" ~ Variable %in% names(df_qc),
      Dataset == "Regional Data" ~ Variable %in% names(df_reg),
      Dataset == "Multi-Setting Data" ~ Variable %in% names(df_ms),
      TRUE ~ FALSE
    )
  )

observed_env_like <- bind_rows(
  tibble(Dataset = "QC Data", Variable = names(df_qc)),
  tibble(Dataset = "Regional Data", Variable = names(df_reg)),
  tibble(Dataset = "Multi-Setting Data", Variable = names(df_ms))
) %>%
  filter(grepl("^(RF_|Temp|RH|Pressure|VaporPressure|SH_)", Variable))

unexpected_env_like <- observed_env_like %>%
  anti_join(expected_env_map, by = c("Dataset", "Variable"))

missing_expected_env <- expected_env_map %>%
  anti_join(observed_env_like, by = c("Dataset", "Variable"))

all_workbook_cols <- sort(unique(c(names(df_qc), names(df_reg), names(df_ms))))
dictionary_codes <- sort(unique(dict$Code[!is.na(dict$Code)]))

workbook_cols_missing_dictionary <- tibble(
  Workbook_Column = setdiff(all_workbook_cols, dictionary_codes)
)

dictionary_codes_missing_workbook <- tibble(
  Dictionary_Code = setdiff(dictionary_codes, all_workbook_cols)
)

summary_rows <- apply(summary_raw, 1, function(r) {
  vals <- as.character(r)
  vals <- vals[!is.na(vals) & trimws(vals) != ""]
  paste(vals, collapse = " | " )
})
summary_week_note <- summary_rows[grepl(
  "Weekly alignment|continuous 7-day blocks|53rd block",
  summary_rows,
  ignore.case = TRUE
)]
summary_week_note <- if (length(summary_week_note) == 0) NA_character_ else paste(summary_week_note, collapse = " || " )

dictionary_wn_note <- dict$Description[dict$Code == "WN"]
dictionary_wn_note <- if (length(dictionary_wn_note) == 0) NA_character_ else paste(dictionary_wn_note, collapse = " || " )

week_metadata_conflict <-
  !is.na(dictionary_wn_note) && !is.na(summary_week_note) &&
  grepl("ISO", dictionary_wn_note, ignore.case = TRUE) &&
  grepl("continuous 7-day blocks|folded into WN 52|53rd block", summary_week_note, ignore.case = TRUE)

week_metadata_audit <- tibble(
  Item = "WN weekly alignment",
  Data_Dictionary = dictionary_wn_note,
  Dataset_Summary = summary_week_note,
  Metadata_Conflict = week_metadata_conflict,
  Action = if (week_metadata_conflict) {
    "Reconcile the Data Dictionary and Dataset Summary before publication. Structural completeness in this script follows the Dataset Summary 52-block convention."
  } else {
    "No direct wording conflict detected."
  }
)

print_full(env_inventory, "Environmental variable inventory:")
print_full(unexpected_env_like, "Environmental-looking columns not in the expected inventory:")
print_full(missing_expected_env, "Expected environmental columns missing from workbook:")
print_full(workbook_cols_missing_dictionary, "Workbook columns without matching Data Dictionary CODE:")
print_full(dictionary_codes_missing_workbook, "Data Dictionary CODE values without matching workbook column:")
print_full(week_metadata_audit, "WN metadata consistency audit:")


# =============================================================================
# STAGE 2. ROW RECONCILIATION, YEAR COVERAGE, AND 52-WEEK COMPLETENESS
# =============================================================================

section_header("STAGE 2. ROW RECONCILIATION, YEAR COVERAGE, AND 52-WEEK COMPLETENESS")

structure <- bind_rows(
  row_reconciliation_52(df_qc, "QC Data", NULL),
  row_reconciliation_52(df_reg, "Regional Data", "REGION"),
  row_reconciliation_52(df_ms, "Multi-Setting Data", "SETTING")
)

year_cov <- bind_rows(
  year_coverage(df_qc, "QC Data", NULL),
  year_coverage(df_reg, "Regional Data", "REGION"),
  year_coverage(df_ms, "Multi-Setting Data", "SETTING")
)

wk_qc <- week_completeness_52(df_qc, "QC Data", NULL)
wk_reg <- week_completeness_52(df_reg, "Regional Data", "REGION")
wk_ms <- week_completeness_52(df_ms, "Multi-Setting Data", "SETTING")

week_summary <- bind_rows(wk_qc$summary, wk_reg$summary, wk_ms$summary)
missing_weeks <- bind_rows(wk_qc$missing, wk_reg$missing, wk_ms$missing)

print_full(structure, "Row reconciliation using the workbook's 52-week convention:")
print_full(year_cov, "Year coverage:")
print_full(week_summary %>% filter(!Complete_52_Weeks), "Incomplete observed geography-years:")
print_full(missing_weeks, "Missing week detail:")


# =============================================================================
# STAGE 3. ENVIRONMENTAL-VARIABLE COVERAGE AND MISSINGNESS
# =============================================================================

section_header("STAGE 3. ENVIRONMENTAL-VARIABLE COVERAGE AND MISSINGNESS")

env_coverage_all <- bind_rows(
  env_coverage(df_qc, "QC Data", env_qc),
  env_coverage(df_reg, "Regional Data", env_reg),
  env_coverage(df_ms, "Multi-Setting Data", env_ms)
) %>%
  left_join(
    dict %>% select(Code, Data_Source, Unit),
    by = c("Variable" = "Code")
  )

ms_sh_coverage_setting <- df_ms %>%
  group_by(SETTING) %>%
  summarise(
    Rows = n(),
    SH_MERRA2_Observed = sum(!is.na(SH_MERRA2)),
    SH_MERRA2_Missing = sum(is.na(SH_MERRA2)),
    Low_Coverage_Flagged = sum(FLAG_MERRA2_SH_LOW_COVERAGE == 1, na.rm = TRUE),
    Solid_Coverage = sum(FLAG_MERRA2_SH_LOW_COVERAGE == 0, na.rm = TRUE),
    Coverage_Pct = safe_pct(SH_MERRA2_Observed, Rows),
    Solid_Coverage_Pct = safe_pct(Solid_Coverage, Rows),
    .groups = "drop"
  )

print_full(env_coverage_all, "Environmental coverage summary:")
print_full(ms_sh_coverage_setting, "MERRA-2 specific-humidity coverage by setting:")


# =============================================================================
# STAGE 4. DUPLICATE KEYS AND TEMPORAL VALIDITY
# =============================================================================

section_header("STAGE 4. DUPLICATE KEYS AND TEMPORAL VALIDITY")

duplicate_table <- bind_rows(
  duplicate_summary(df_qc, "QC Data", c("YR", "WN")),
  duplicate_summary(df_reg, "Regional Data", c("REGION", "YR", "WN")),
  duplicate_summary(df_ms, "Multi-Setting Data", c("SETTING", "YR", "WN"))
)

duplicate_details <- bind_rows(
  duplicate_detail(df_qc, "QC Data", c("YR", "WN")),
  duplicate_detail(df_reg, "Regional Data", c("REGION", "YR", "WN")),
  duplicate_detail(df_ms, "Multi-Setting Data", c("SETTING", "YR", "WN"))
)

temporal_validity <- bind_rows(
  tibble(
    Dataset = "QC Data",
    Min_Year = min(df_qc$YR), Max_Year = max(df_qc$YR),
    Min_Week = min(df_qc$WN), Max_Week = max(df_qc$WN),
    Invalid_Week_n = sum(!(df_qc$WN %in% 1:52)),
    Missing_Year_n = sum(is.na(df_qc$YR)), Missing_Week_n = sum(is.na(df_qc$WN))
  ),
  tibble(
    Dataset = "Regional Data",
    Min_Year = min(df_reg$YR), Max_Year = max(df_reg$YR),
    Min_Week = min(df_reg$WN), Max_Week = max(df_reg$WN),
    Invalid_Week_n = sum(!(df_reg$WN %in% 1:52)),
    Missing_Year_n = sum(is.na(df_reg$YR)), Missing_Week_n = sum(is.na(df_reg$WN))
  ),
  tibble(
    Dataset = "Multi-Setting Data",
    Min_Year = min(df_ms$YR), Max_Year = max(df_ms$YR),
    Min_Week = min(df_ms$WN), Max_Week = max(df_ms$WN),
    Invalid_Week_n = sum(!(df_ms$WN %in% 1:52)),
    Missing_Year_n = sum(is.na(df_ms$YR)), Missing_Week_n = sum(is.na(df_ms$WN))
  )
)

print_full(duplicate_table, "Duplicate key summary:")
print_full(duplicate_details, "Duplicate key detail:")
print_full(temporal_validity, "Temporal validity:")


# =============================================================================
# STAGE 5. NUMERIC AND PHYSICAL-DOMAIN CHECKS
# =============================================================================

section_header("STAGE 5. NUMERIC AND PHYSICAL-DOMAIN CHECKS")

physical_domains <- tribble(
  ~Pattern, ~Lower, ~Upper, ~Meaning,
  "^RF_", 0, Inf, "Weekly rainfall must be non-negative",
  "^Temp", -60, 60, "Temperature in degrees C",
  "^RH", 0, 100, "Relative humidity in percent",
  "^Pressure", 800, 1100, "Surface pressure in hPa; broad QC envelope",
  "^VaporPressure", 0, 100, "Vapor pressure in hPa; broad QC envelope",
  "^SH_", 0, 50, "Specific humidity in g/kg; broad QC envelope"
)

run_env_domain_checks <- function(df, dataset, vars) {
  bind_rows(lapply(vars, function(v) {
    rule_idx <- which(vapply(physical_domains$Pattern, function(p) grepl(p, v), logical(1)))[1]
    if (is.na(rule_idx)) {
      return(tibble())
    }
    range_check(
      df, dataset, v,
      lower = physical_domains$Lower[rule_idx],
      upper = physical_domains$Upper[rule_idx],
      allow_na = TRUE
    ) %>%
      mutate(Rule = physical_domains$Meaning[rule_idx])
  }))
}

env_domain_checks <- bind_rows(
  run_env_domain_checks(df_qc, "QC Data", env_qc),
  run_env_domain_checks(df_reg, "Regional Data", env_reg),
  run_env_domain_checks(df_ms, "Multi-Setting Data", env_ms)
)

case_domain_checks <- bind_rows(
  range_check(df_qc, "QC Data", case_qc, lower = 0, upper = Inf, allow_na = FALSE),
  range_check(df_reg, "Regional Data", case_reg, lower = 0, upper = Inf, allow_na = FALSE),
  range_check(df_ms, "Multi-Setting Data", case_ms, lower = 0, upper = Inf, allow_na = FALSE)
)

print_full(env_domain_checks, "Environmental physical-domain checks:")
print_full(case_domain_checks, "Dengue count domain checks:")


# =============================================================================
# STAGE 6. INTERNAL ORDERING AND DERIVED-VARIABLE CONSISTENCY
# =============================================================================

section_header("STAGE 6. INTERNAL ORDERING AND DERIVED-VARIABLE CONSISTENCY")

ordering_checks <- bind_rows(
  ordering_check(df_qc, "QC Data", "TempMin_ERA5-Land", "Temp_ERA5-Land", "TempMax_ERA5-Land",
                 "ERA5-Land weekly mean daily minimum <= mean <= weekly mean daily maximum"),
  ordering_check(df_qc, "QC Data", "RHMin_ERA5-Land", "RH_ERA5-Land", "RHMax_ERA5-Land",
                 "ERA5-Land weekly mean daily RH minimum <= mean <= weekly mean daily RH maximum"),
  ordering_check(df_ms, "Multi-Setting Data", "TempMin_MERRA2", "Temp_MERRA2", "TempMax_MERRA2",
                 "MERRA-2 weekly mean daily minimum <= mean <= weekly mean daily maximum"),
  actual_vs_saturation_check(df_qc, "QC Data", "VaporPressureAct_ERA5-Land", "VaporPressureSat_ERA5-Land",
                             "ERA5-Land actual vapor pressure <= saturation vapor pressure"),
  actual_vs_saturation_check(df_qc, "QC Data", "VaporPressureAct_PAGASA", "VaporPressureSat_PAGASA",
                             "PAGASA actual vapor pressure <= saturation vapor pressure")
)

derived_consistency <- bind_rows(
  approx_derived_consistency(
    df_qc, "QC Data", "Temp_ERA5-Land", "RH_ERA5-Land", "Pressure_ERA5-Land",
    "VaporPressureSat_ERA5-Land", "VaporPressureAct_ERA5-Land", "SH_ERA5-Land", "ERA5-Land"
  ),
  approx_derived_consistency(
    df_qc, "QC Data", "Temp_PAGASA", "RH_PAGASA", "Pressure_ERA5-Land",
    "VaporPressureSat_PAGASA", "VaporPressureAct_PAGASA", "SH_PAGASA", "PAGASA plus ERA5-Land pressure"
  )
)

pressure_gap_consistency <- tibble(
  Check = c(
    "FLAG_PRESSURE_SH_GAP exactly marks 2025 weeks 40-52",
    "Pressure_ERA5-Land missing exactly when gap flag = 1",
    "SH_ERA5-Land missing exactly when gap flag = 1"
  ),
  Pass = c(
    identical(as.integer(df_qc$FLAG_PRESSURE_SH_GAP), as.integer(df_qc$YR == 2025L & df_qc$WN >= 40L)),
    identical(is.na(df_qc$`Pressure_ERA5-Land`), df_qc$FLAG_PRESSURE_SH_GAP == 1L),
    identical(is.na(df_qc$`SH_ERA5-Land`), df_qc$FLAG_PRESSURE_SH_GAP == 1L)
  )
)

print_full(ordering_checks, "Ordering checks:")
print_full(derived_consistency, "Approximate derived-variable consistency:")
print_full(pressure_gap_consistency, "Pressure and specific-humidity gap consistency:")


# =============================================================================
# STAGE 7. DATA-QUALITY FLAG VERIFICATION
# =============================================================================

section_header("STAGE 7. DATA-QUALITY FLAG VERIFICATION")

plaus_qc_for_flag <- plausibility_check(df_qc, "QC Data", case_qc, NULL)
plaus_reg_for_flag <- plausibility_check(df_reg, "Regional Data", case_reg, "REGION")

expected_qc_plaus <- rep(0L, nrow(df_qc))
if (nrow(plaus_qc_for_flag$detail) > 0) {
  for (i in seq_len(nrow(plaus_qc_for_flag$detail))) {
    idx <- which(
      df_qc$YR == plaus_qc_for_flag$detail$YR[i] &
        df_qc$WN == plaus_qc_for_flag$detail$WN[i]
    )
    if (length(idx) > 0) expected_qc_plaus[idx] <- 1L
  }
}

expected_reg_plaus <- rep(0L, nrow(df_reg))
if (nrow(plaus_reg_for_flag$detail) > 0) {
  for (i in seq_len(nrow(plaus_reg_for_flag$detail))) {
    idx <- which(
      df_reg$REGION == plaus_reg_for_flag$detail$Geography[i] &
        df_reg$YR == plaus_reg_for_flag$detail$YR[i] &
        df_reg$WN == plaus_reg_for_flag$detail$WN[i]
    )
    if (length(idx) > 0) expected_reg_plaus[idx] <- 1L
  }
}

flag_binary <- bind_rows(
  lapply(flag_qc, function(v) binary_flag_check(df_qc, "QC Data", v, allow_na = FALSE)),
  lapply(flag_reg, function(v) binary_flag_check(df_reg, "Regional Data", v, allow_na = FALSE)),
  lapply(flag_ms, function(v) binary_flag_check(
    df_ms, "Multi-Setting Data", v,
    allow_na = identical(v, "FLAG_MERRA2_SH_LOW_COVERAGE")
  ))
)

flag_verification <- tribble(
  ~Dataset, ~Flag, ~Verification_Level, ~Pass, ~Note,
  "QC Data", "FLAG_COVID", "Independent recalculation",
  identical(as.integer(df_qc$FLAG_COVID), as.integer(df_qc$YR %in% c(2020L, 2021L))),
  "Expected 1 for 2020-2021 only.",
  "QC Data", "FLAG_SINGLE_CELL_RF", "Structural rule",
  all(df_qc$FLAG_SINGLE_CELL_RF == 1L, na.rm = TRUE),
  "Workbook metadata states all QC IMERG rows use the QC centroid extraction.",
  "QC Data", "FLAG_PLAUSIBILITY", "Independent recalculation",
  identical(as.integer(df_qc$FLAG_PLAUSIBILITY), expected_qc_plaus),
  "Recalculated from week-to-week dengue changes with the year-gap guard.",
  "QC Data", "FLAG_PRESSURE_SH_GAP", "Independent recalculation",
  all(pressure_gap_consistency$Pass),
  "Expected 1 from 2025 week 40 onward and missing Pressure_ERA5-Land/SH_ERA5-Land.",
  "Regional Data", "FLAG_DEKADAL_APPROX", "Structural rule",
  all(df_reg$FLAG_DEKADAL_APPROX == 1L, na.rm = TRUE),
  "Workbook metadata states all RF_HDX rows use dekadal disaggregation.",
  "Regional Data", "FLAG_PLAUSIBILITY", "Independent recalculation",
  identical(as.integer(df_reg$FLAG_PLAUSIBILITY), expected_reg_plaus),
  "Recalculated from week-to-week dengue changes with the year-gap guard.",
  "Multi-Setting Data", "FLAG_SINGLE_CELL_RF", "Structural rule",
  all(df_ms$FLAG_SINGLE_CELL_RF == 1L, na.rm = TRUE),
  "Workbook metadata assigns this flag to all multi-setting IMERG rows.",
  "Multi-Setting Data", "FLAG_TERMINAL_GAP", "Binary and row-level inspection only",
  all(df_ms$FLAG_TERMINAL_GAP %in% c(0L, 1L), na.rm = TRUE),
  "Absent terminal weeks have no rows and cannot carry a row-level flag.",
  "Multi-Setting Data", "FLAG_MERRA2_SH_LOW_COVERAGE", "Structural consistency only",
  all(is.na(df_ms$FLAG_MERRA2_SH_LOW_COVERAGE) == is.na(df_ms$SH_MERRA2)),
  "Daily valid-day counts are not present, so the fewer-than-4-days rule cannot be independently recomputed."
)

terminal_gap_rows <- df_ms %>%
  filter(FLAG_TERMINAL_GAP == 1L) %>%
  select(SETTING, YR, WN, FLAG_TERMINAL_GAP)

print_full(flag_binary, "Binary flag checks:")
print_full(flag_verification, "Flag verification:")
print_full(terminal_gap_rows, "Rows carrying FLAG_TERMINAL_GAP = 1:")


# =============================================================================
# STAGE 8. DENGUE WEEK-TO-WEEK PLAUSIBILITY CHECKS
# =============================================================================

section_header("STAGE 8. DENGUE WEEK-TO-WEEK PLAUSIBILITY CHECKS")

plaus_ms <- plausibility_check(df_ms, "Multi-Setting Data", case_ms, "SETTING")
plaus_summary <- bind_rows(
  plaus_qc_for_flag$summary,
  plaus_reg_for_flag$summary,
  plaus_ms$summary
)
plaus_detail <- bind_rows(
  plaus_qc_for_flag$detail,
  plaus_reg_for_flag$detail,
  plaus_ms$detail
)

print_full(plaus_summary, "Plausibility summary:")
print_full(plaus_detail, "Plausibility detail:")


# =============================================================================
# STAGE 9. QC CROSS-SOURCE VALIDATION AGAINST PAGASA REFERENCE SERIES
# =============================================================================

section_header("STAGE 9. QC CROSS-SOURCE VALIDATION AGAINST PAGASA REFERENCE SERIES")

qc_pairs <- tribble(
  ~Pair_ID, ~Series_A, ~Series_B, ~Unit, ~Filter_Rule, ~Note,
  "QC_Rainfall_IMERG_vs_PAGASA", "RF_NASA", "RF_PAGASA", "mm/week", "none",
  "PAGASA rainfall is a supplied reference series. Exact station metadata are not present in the workbook.",
  "QC_Temperature_ERA5-Land_vs_PAGASA", "Temp_ERA5-Land", "Temp_PAGASA", "degrees C", "none",
  "PAGASA station mean temperature is used as the reference series.",
  "QC_RH_ERA5-Land_vs_PAGASA", "RH_ERA5-Land", "RH_PAGASA", "percent", "none",
  "PAGASA station relative humidity is used as the reference series.",
  "QC_VPSat_ERA5-Land_vs_PAGASA", "VaporPressureSat_ERA5-Land", "VaporPressureSat_PAGASA", "hPa", "none",
  "Both variables are derived from their source-specific temperature series.",
  "QC_VPAct_ERA5-Land_vs_PAGASA", "VaporPressureAct_ERA5-Land", "VaporPressureAct_PAGASA", "hPa", "none",
  "Both variables are derived from source-specific temperature and humidity.",
  "QC_SH_ERA5-Land_vs_PAGASA", "SH_ERA5-Land", "SH_PAGASA", "g/kg", "none",
  "SH_PAGASA uses PAGASA temperature/RH and ERA5-Land pressure, so this pair is not fully source-independent."
)

qc_cross_all <- run_pair_metrics(df_qc, qc_pairs, "All observed overlap", NULL, "QUEZON CITY")
qc_cross_primary <- run_pair_metrics(qc_primary, qc_pairs, "Primary analytic years", NULL, "QUEZON CITY")
qc_cross_source <- bind_rows(qc_cross_all, qc_cross_primary)

print_full(qc_cross_source, "QC cross-source metrics:")


# =============================================================================
# STAGE 10. MULTI-SETTING CROSS-SOURCE CONCORDANCE
# =============================================================================

section_header("STAGE 10. MULTI-SETTING CROSS-SOURCE CONCORDANCE")

ms_pairs <- tribble(
  ~Pair_ID, ~Series_A, ~Series_B, ~Unit, ~Filter_Rule, ~Note,
  "MS_Rainfall_IMERG_vs_ERA5-Land", "RF_NASA", "RF_ERA5-Land", "mm/week", "none",
  "Independent gridded rainfall products at the same setting-level support. Dataset Summary notes RF_ERA5-Land weekly boundaries can be offset by up to one day from the other environmental grids.",
  "MS_Temperature_ERA5-Land_vs_MERRA2", "Temp_ERA5-Land", "Temp_MERRA2", "degrees C", "none",
  "Two reanalysis products at the same setting-level support.",
  "MS_SH_ERA5-Land_vs_MERRA2_all", "SH_ERA5-Land", "SH_MERRA2", "g/kg", "none",
  "Uses every non-missing MERRA-2 weekly SH value, including low-coverage flagged weeks.",
  "MS_SH_ERA5-Land_vs_MERRA2_strict", "SH_ERA5-Land", "SH_MERRA2", "g/kg", "merra2_sh_strict",
  "Restricts MERRA-2 SH to weeks with at least 4 valid source days."
)

ms_cross_setting_all <- run_pair_metrics(df_ms, ms_pairs, "All observed overlap", "SETTING")
ms_cross_setting_primary <- run_pair_metrics(ms_primary, ms_pairs, "Primary analytic years", "SETTING")
ms_cross_pooled_all <- run_pair_metrics(df_ms, ms_pairs, "All observed overlap", NULL, "POOLED")
ms_cross_pooled_primary <- run_pair_metrics(ms_primary, ms_pairs, "Primary analytic years", NULL, "POOLED")

ms_cross_source <- bind_rows(
  ms_cross_setting_all,
  ms_cross_setting_primary,
  ms_cross_pooled_all,
  ms_cross_pooled_primary
)

print_full(ms_cross_source, "Multi-setting cross-source metrics:")


# =============================================================================
# STAGE 11. YEAR-SPECIFIC STABILITY OF CROSS-SOURCE AGREEMENT
# =============================================================================

section_header("STAGE 11. YEAR-SPECIFIC STABILITY OF CROSS-SOURCE AGREEMENT")

qc_by_year <- run_pair_metrics_by_year(
  qc_primary, qc_pairs, "Primary analytic years", NULL, min_pairs = 10L
) %>% mutate(Dataset = "QC Data", .before = 1)

ms_by_year <- run_pair_metrics_by_year(
  ms_primary, ms_pairs, "Primary analytic years", "SETTING", min_pairs = 10L
) %>% mutate(Dataset = "Multi-Setting Data", .before = 1)

temporal_source_stability <- bind_rows(qc_by_year, ms_by_year)

print_full(temporal_source_stability, "Cross-source agreement by year:")


# =============================================================================
# STAGE 12. LEAVE-ONE-YEAR-OUT CROSS-SOURCE CALIBRATION VALIDATION
# =============================================================================

section_header("STAGE 12. LEAVE-ONE-YEAR-OUT CROSS-SOURCE CALIBRATION VALIDATION")

qc_loyo <- loyo_calibration_cv(
  qc_primary, qc_pairs, "Primary analytic years", NULL,
  min_train = 20L, min_test = 5L
) %>% mutate(Dataset = "QC Data", .before = 1)

ms_loyo <- loyo_calibration_cv(
  ms_primary, ms_pairs, "Primary analytic years", "SETTING",
  min_train = 20L, min_test = 5L
) %>% mutate(Dataset = "Multi-Setting Data", .before = 1)

loyo_detail <- bind_rows(qc_loyo, ms_loyo)
loyo_summary <- summarise_loyo(loyo_detail)

print_full(loyo_detail, "LOYO cross-source calibration detail:")
print_full(loyo_summary, "LOYO cross-source calibration summary:")

cat(
  "\nLOYO interpretation: Series_B is calibrated from Series_A using all other eligible years,\n",
  "then tested on the held-out year. This assesses temporal transferability of the cross-source\n",
  "relationship. For non-PAGASA pairs it is a consistency test, not an accuracy test.\n",
  sep = ""
)


# =============================================================================
# STAGE 13. CROSS-SCALE REPRESENTATIVENESS CHECKS WITHIN THE PHILIPPINES
# =============================================================================

section_header("STAGE 13. CROSS-SCALE REPRESENTATIVENESS CHECKS WITHIN THE PHILIPPINES")

ncr_all <- df_reg %>% filter(REGION == "NCR")
ncr_primary <- reg_primary %>% filter(REGION == "NCR")
ph_all <- df_ms %>% filter(SETTING == "PHILIPPINES")
ph_primary <- ms_primary %>% filter(SETTING == "PHILIPPINES")

reg_mean_all <- df_reg %>%
  group_by(YR, WN) %>%
  summarise(
    RF_HDX_regmean = mean(RF_HDX, na.rm = TRUE),
    `Temp_ERA5-Land_regmean` = mean(`Temp_ERA5-Land`, na.rm = TRUE),
    `SH_ERA5-Land_regmean` = mean(`SH_ERA5-Land`, na.rm = TRUE),
    Regions_n = n_distinct(REGION),
    .groups = "drop"
  )

reg_mean_primary <- reg_primary %>%
  group_by(YR, WN) %>%
  summarise(
    RF_HDX_regmean = mean(RF_HDX, na.rm = TRUE),
    `Temp_ERA5-Land_regmean` = mean(`Temp_ERA5-Land`, na.rm = TRUE),
    `SH_ERA5-Land_regmean` = mean(`SH_ERA5-Land`, na.rm = TRUE),
    Regions_n = n_distinct(REGION),
    .groups = "drop"
  )

cross_scale_all <- bind_rows(
  cross_scale_metric(df_qc, ncr_all, "Temp_ERA5-Land", "Temp_ERA5-Land",
                     "QC_vs_NCR_Temp_ERA5-Land", "degrees C", "All observed overlap",
                     "QC ERA5-Land point temperature", "NCR ERA5-Land regional mean temperature",
                     "Same source family, different spatial support. Representativeness check only."),
  cross_scale_metric(df_qc, ncr_all, "SH_ERA5-Land", "SH_ERA5-Land",
                     "QC_vs_NCR_SH_ERA5-Land", "g/kg", "All observed overlap",
                     "QC ERA5-Land point SH", "NCR ERA5-Land regional mean SH",
                     "Same source family, different spatial support. Representativeness check only."),
  cross_scale_metric(df_qc, ph_all, "RF_NASA", "RF_NASA",
                     "QC_vs_PHL_RF_NASA", "mm/week", "All observed overlap",
                     "QC IMERG point rainfall", "Philippines IMERG setting mean rainfall",
                     "Same product, very different spatial support. Representativeness check only."),
  cross_scale_metric(df_qc, ph_all, "Temp_ERA5-Land", "Temp_ERA5-Land",
                     "QC_vs_PHL_Temp_ERA5-Land", "degrees C", "All observed overlap",
                     "QC ERA5-Land point temperature", "Philippines ERA5-Land setting mean temperature",
                     "Same source family, different spatial support."),
  cross_scale_metric(df_qc, ph_all, "SH_ERA5-Land", "SH_ERA5-Land",
                     "QC_vs_PHL_SH_ERA5-Land", "g/kg", "All observed overlap",
                     "QC ERA5-Land point SH", "Philippines ERA5-Land setting mean SH",
                     "Same source family, different spatial support."),
  cross_scale_metric(reg_mean_all, ph_all, "Temp_ERA5-Land_regmean", "Temp_ERA5-Land",
                     "RegMean_vs_PHL_Temp_ERA5-Land", "degrees C", "All observed overlap",
                     "Unweighted mean of 17 regional ERA5-Land temperatures", "Philippines ERA5-Land setting mean temperature",
                     "Regional mean is unweighted and does not reproduce the setting-level area weighting."),
  cross_scale_metric(reg_mean_all, ph_all, "SH_ERA5-Land_regmean", "SH_ERA5-Land",
                     "RegMean_vs_PHL_SH_ERA5-Land", "g/kg", "All observed overlap",
                     "Unweighted mean of 17 regional ERA5-Land SH", "Philippines ERA5-Land setting mean SH",
                     "Regional mean is unweighted and does not reproduce the setting-level area weighting."),
  cross_scale_metric(reg_mean_all, ph_all, "RF_HDX_regmean", "RF_NASA",
                     "RegMean_HDX_vs_PHL_IMERG", "mm/week", "All observed overlap",
                     "Unweighted mean of regional CHIRPS/HDX rainfall", "Philippines IMERG setting mean rainfall",
                     "Differs in both product and spatial aggregation."),
  cross_scale_metric(reg_mean_all, ph_all, "RF_HDX_regmean", "RF_ERA5-Land",
                     "RegMean_HDX_vs_PHL_ERA5-Land_Rain", "mm/week", "All observed overlap",
                     "Unweighted mean of regional CHIRPS/HDX rainfall", "Philippines ERA5-Land setting rainfall",
                     "Differs in both product and spatial aggregation.")
)

cross_scale_primary <- bind_rows(
  cross_scale_metric(qc_primary, ncr_primary, "Temp_ERA5-Land", "Temp_ERA5-Land",
                     "QC_vs_NCR_Temp_ERA5-Land", "degrees C", "Primary analytic years",
                     "QC ERA5-Land point temperature", "NCR ERA5-Land regional mean temperature",
                     "Same source family, different spatial support. Representativeness check only."),
  cross_scale_metric(qc_primary, ncr_primary, "SH_ERA5-Land", "SH_ERA5-Land",
                     "QC_vs_NCR_SH_ERA5-Land", "g/kg", "Primary analytic years",
                     "QC ERA5-Land point SH", "NCR ERA5-Land regional mean SH",
                     "Same source family, different spatial support. Representativeness check only."),
  cross_scale_metric(qc_primary, ph_primary, "RF_NASA", "RF_NASA",
                     "QC_vs_PHL_RF_NASA", "mm/week", "Primary analytic years",
                     "QC IMERG point rainfall", "Philippines IMERG setting mean rainfall",
                     "Same product, very different spatial support. Representativeness check only."),
  cross_scale_metric(qc_primary, ph_primary, "Temp_ERA5-Land", "Temp_ERA5-Land",
                     "QC_vs_PHL_Temp_ERA5-Land", "degrees C", "Primary analytic years",
                     "QC ERA5-Land point temperature", "Philippines ERA5-Land setting mean temperature",
                     "Same source family, different spatial support."),
  cross_scale_metric(qc_primary, ph_primary, "SH_ERA5-Land", "SH_ERA5-Land",
                     "QC_vs_PHL_SH_ERA5-Land", "g/kg", "Primary analytic years",
                     "QC ERA5-Land point SH", "Philippines ERA5-Land setting mean SH",
                     "Same source family, different spatial support."),
  cross_scale_metric(reg_mean_primary, ph_primary, "Temp_ERA5-Land_regmean", "Temp_ERA5-Land",
                     "RegMean_vs_PHL_Temp_ERA5-Land", "degrees C", "Primary analytic years",
                     "Unweighted mean of 17 regional ERA5-Land temperatures", "Philippines ERA5-Land setting mean temperature",
                     "Regional mean is unweighted and does not reproduce the setting-level area weighting."),
  cross_scale_metric(reg_mean_primary, ph_primary, "SH_ERA5-Land_regmean", "SH_ERA5-Land",
                     "RegMean_vs_PHL_SH_ERA5-Land", "g/kg", "Primary analytic years",
                     "Unweighted mean of 17 regional ERA5-Land SH", "Philippines ERA5-Land setting mean SH",
                     "Regional mean is unweighted and does not reproduce the setting-level area weighting."),
  cross_scale_metric(reg_mean_primary, ph_primary, "RF_HDX_regmean", "RF_NASA",
                     "RegMean_HDX_vs_PHL_IMERG", "mm/week", "Primary analytic years",
                     "Unweighted mean of regional CHIRPS/HDX rainfall", "Philippines IMERG setting mean rainfall",
                     "Differs in both product and spatial aggregation."),
  cross_scale_metric(reg_mean_primary, ph_primary, "RF_HDX_regmean", "RF_ERA5-Land",
                     "RegMean_HDX_vs_PHL_ERA5-Land_Rain", "mm/week", "Primary analytic years",
                     "Unweighted mean of regional CHIRPS/HDX rainfall", "Philippines ERA5-Land setting rainfall",
                     "Differs in both product and spatial aggregation.")
)

cross_scale_results <- bind_rows(cross_scale_all, cross_scale_primary)

regional_spatial_heterogeneity <- df_reg %>%
  group_by(YR, WN) %>%
  summarise(
    Regions_n = n_distinct(REGION),
    Rain_Mean = mean(RF_HDX, na.rm = TRUE),
    Rain_SD = sd(RF_HDX, na.rm = TRUE),
    Rain_Min = min(RF_HDX, na.rm = TRUE),
    Rain_Max = max(RF_HDX, na.rm = TRUE),
    Temp_Mean = mean(`Temp_ERA5-Land`, na.rm = TRUE),
    Temp_SD = sd(`Temp_ERA5-Land`, na.rm = TRUE),
    SH_Mean = mean(`SH_ERA5-Land`, na.rm = TRUE),
    SH_SD = sd(`SH_ERA5-Land`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rain_Range = Rain_Max - Rain_Min,
    Rain_CV_Pct = ifelse(Rain_Mean == 0, NA_real_, 100 * Rain_SD / Rain_Mean)
  )

print_full(cross_scale_results, "Cross-scale comparison metrics:")


# =============================================================================
# STAGE 14. LAGGED DENGUE-ENVIRONMENT SIGNAL SCREEN FOR ALL VARIABLES
# =============================================================================

section_header("STAGE 14. LAGGED DENGUE-ENVIRONMENT SIGNAL SCREEN FOR ALL VARIABLES")

lag_qc <- lagged_environment_association(
  qc_primary, "QC Data", case_qc, env_qc,
  lags = LAG_WEEKS, geo_var = NULL,
  quality_note = "Primary QC years only. Environmental variables are lagged within calendar year."
)

lag_reg <- lagged_environment_association(
  reg_primary, "Regional Data", case_reg, env_reg,
  lags = LAG_WEEKS, geo_var = "REGION",
  quality_note = "Primary analytic years only. Environmental variables are lagged within region and calendar year."
)

lag_ms <- lagged_environment_association(
  ms_primary_lag, "Multi-Setting Data", case_ms, env_ms,
  lags = LAG_WEEKS, geo_var = "SETTING",
  quality_note = paste0(
    "Primary analytic years only. Environmental variables are lagged within setting and calendar year. ",
    "SH_MERRA2 is restricted to weeks with at least 4 valid source days."
  )
)

lag_associations <- bind_rows(lag_qc, lag_reg, lag_ms)

print_full(lag_associations, "Lagged dengue-environment associations:")

cat(
  "\nThis stage is a descriptive source-robustness screen. It does not establish causality,\n",
  "does not select a final epidemiological model, and does not use future environmental values.\n",
  sep = ""
)


# =============================================================================
# STAGE 15. SOURCE-ROBUST LAG SUMMARY AND FINAL VALIDATION MANIFEST
# =============================================================================

section_header("STAGE 15. SOURCE-ROBUST LAG SUMMARY AND FINAL VALIDATION MANIFEST")

best_lags <- best_lag_summary(lag_associations)

source_pair_best_lags <- bind_rows(
  qc_pairs %>% transmute(Dataset = "QC Data", Pair_ID, Series_A, Series_B),
  ms_pairs %>%
    filter(Pair_ID != "MS_SH_ERA5-Land_vs_MERRA2_all") %>%
    transmute(Dataset = "Multi-Setting Data", Pair_ID, Series_A, Series_B)
) %>%
  left_join(
    best_lags %>%
      select(Dataset, Geography, Variable, Best_Lag_Weeks, Best_Spearman_rho) %>%
      rename(
        Series_A = Variable,
        Best_Lag_A = Best_Lag_Weeks,
        Best_Rho_A = Best_Spearman_rho
      ),
    by = c("Dataset", "Series_A")
  ) %>%
  left_join(
    best_lags %>%
      select(Dataset, Geography, Variable, Best_Lag_Weeks, Best_Spearman_rho) %>%
      rename(
        Series_B = Variable,
        Best_Lag_B = Best_Lag_Weeks,
        Best_Rho_B = Best_Spearman_rho
      ),
    by = c("Dataset", "Geography", "Series_B")
  ) %>%
  mutate(
    Lag_Difference_Weeks = Best_Lag_A - Best_Lag_B,
    Rho_Difference = Best_Rho_A - Best_Rho_B
  )

primary_eligibility <- bind_rows(
  tibble(
    Dataset = "QC Data",
    Rule = "QC primary seasons",
    Included_Years = paste(QC_PRIMARY_YEARS, collapse = ", "),
    Excluded_Years = paste(setdiff(sort(unique(df_qc$YR)), QC_PRIMARY_YEARS), collapse = ", "),
    Rows_Full = nrow(df_qc), Rows_Primary = nrow(qc_primary)
  ),
  tibble(
    Dataset = "Regional Data",
    Rule = "Exclude 2020, 2021, 2025",
    Included_Years = collapse_ints(reg_primary$YR),
    Excluded_Years = paste(PRIMARY_EXCLUDE_YEARS, collapse = ", "),
    Rows_Full = nrow(df_reg), Rows_Primary = nrow(reg_primary)
  ),
  tibble(
    Dataset = "Multi-Setting Data",
    Rule = "Exclude 2020, 2021, 2025",
    Included_Years = collapse_ints(ms_primary$YR),
    Excluded_Years = paste(PRIMARY_EXCLUDE_YEARS, collapse = ", "),
    Rows_Full = nrow(df_ms), Rows_Primary = nrow(ms_primary)
  )
)

validation_manifest <- tibble(
  Check = c(
    "All required sheets present",
    "All required columns present",
    "All expected environmental columns present",
    "No unexpected environmental-looking columns",
    "All workbook columns represented by Data Dictionary CODE values",
    "All Data Dictionary CODE values represented by workbook columns",
    "WN metadata internally consistent",
    "No duplicate composite keys",
    "All week values within 1-52",
    "Environmental values within broad physical domains",
    "Dengue counts non-negative",
    "Internal min/mean/max and vapor-pressure ordering checks",
    "Pressure/SH gap flag consistency",
    "Binary flag domain checks",
    "Flag verification rules supported by weekly data"
  ),
  Pass = c(
    length(missing_sheets) == 0,
    all(required_check$Present),
    nrow(missing_expected_env) == 0,
    nrow(unexpected_env_like) == 0,
    nrow(workbook_cols_missing_dictionary) == 0,
    nrow(dictionary_codes_missing_workbook) == 0,
    !week_metadata_conflict,
    all(duplicate_table$Duplicate_Key_Groups == 0),
    all(temporal_validity$Invalid_Week_n == 0),
    all(env_domain_checks$Pass),
    all(case_domain_checks$Pass),
    all(ordering_checks$Pass),
    all(pressure_gap_consistency$Pass),
    all(flag_binary$Pass),
    all(flag_verification$Pass)
  ),
  Note = c(
    "Workbook structure check.",
    "Schema check before coercion.",
    "Inventory is matched to the uploaded workbook.",
    "Pattern-based check for unregistered RF, temperature, RH, pressure, vapor-pressure, or SH fields.",
    "The uploaded workbook contains SETTING and RF_PAGASA without matching Data Dictionary CODE entries. The current Data Dictionary uses COUNTRY for the setting identifier.",
    "The current Data Dictionary contains COUNTRY, while no data sheet contains a COUNTRY column.",
    "The Data Dictionary calls WN ISO week, while Dataset Summary describes continuous 7-day blocks with rare 53rd blocks folded into WN 52.",
    "Composite keys are geography plus year plus week.",
    "Workbook convention is 52 weekly blocks, not ISO-week validation.",
    "Broad physical ranges are screening rules, not climatological reference intervals.",
    "Count domain check.",
    "Internal consistency only.",
    "Independent weekly rule check.",
    "FLAG_MERRA2_SH_LOW_COVERAGE permits NA when SH_MERRA2 is NA.",
    "MERRA-2 low-coverage threshold cannot be independently recomputed without daily valid-day counts."
  )
)

print_full(best_lags, "Best lag per environmental variable and geography:")
print_full(source_pair_best_lags, "Best-lag agreement between paired sources:")
print_full(primary_eligibility, "Primary analysis eligibility:")
print_full(validation_manifest, "Final validation manifest:")


# =============================================================================
# EXPORT OUTPUTS
# =============================================================================

section_header("EXPORTING OUTPUTS")

write_csv_safe(env_inventory, "stage01_environmental_inventory.csv")
write_csv_safe(required_check, "stage01_required_columns.csv")
write_csv_safe(schema_before, "stage01_schema_before.csv")
write_csv_safe(schema_after, "stage01_schema_after.csv")
write_csv_safe(unexpected_env_like, "stage01_unexpected_environment_columns.csv")
write_csv_safe(missing_expected_env, "stage01_missing_environment_columns.csv")
write_csv_safe(workbook_cols_missing_dictionary, "stage01_workbook_columns_missing_dictionary.csv")
write_csv_safe(dictionary_codes_missing_workbook, "stage01_dictionary_codes_missing_workbook.csv")
write_csv_safe(week_metadata_audit, "stage01_week_metadata_audit.csv")

write_csv_safe(structure, "stage02_row_reconciliation.csv")
write_csv_safe(year_cov, "stage02_year_coverage.csv")
write_csv_safe(week_summary, "stage02_week_completeness.csv")
write_csv_safe(missing_weeks, "stage02_missing_weeks.csv")

write_csv_safe(env_coverage_all, "stage03_environmental_coverage.csv")
write_csv_safe(ms_sh_coverage_setting, "stage03_merra2_sh_coverage_by_setting.csv")

write_csv_safe(duplicate_table, "stage04_duplicate_summary.csv")
write_csv_safe(duplicate_details, "stage04_duplicate_detail.csv")
write_csv_safe(temporal_validity, "stage04_temporal_validity.csv")

write_csv_safe(env_domain_checks, "stage05_environment_domain_checks.csv")
write_csv_safe(case_domain_checks, "stage05_dengue_domain_checks.csv")

write_csv_safe(ordering_checks, "stage06_ordering_checks.csv")
write_csv_safe(derived_consistency, "stage06_derived_consistency.csv")
write_csv_safe(pressure_gap_consistency, "stage06_pressure_sh_gap_consistency.csv")

write_csv_safe(flag_binary, "stage07_flag_binary_checks.csv")
write_csv_safe(flag_verification, "stage07_flag_verification.csv")
write_csv_safe(terminal_gap_rows, "stage07_terminal_gap_rows.csv")

write_csv_safe(plaus_summary, "stage08_dengue_plausibility_summary.csv")
write_csv_safe(plaus_detail, "stage08_dengue_plausibility_detail.csv")

write_csv_safe(qc_pairs, "stage09_qc_pair_definitions.csv")
write_csv_safe(qc_cross_source, "stage09_qc_cross_source_validation.csv")
write_csv_safe(ms_pairs, "stage10_multisetting_pair_definitions.csv")
write_csv_safe(ms_cross_source, "stage10_multisetting_cross_source.csv")
write_csv_safe(temporal_source_stability, "stage11_cross_source_by_year.csv")
write_csv_safe(loyo_detail, "stage12_loyo_detail.csv")
write_csv_safe(loyo_summary, "stage12_loyo_summary.csv")
write_csv_safe(cross_scale_results, "stage13_cross_scale_results.csv")
write_csv_safe(regional_spatial_heterogeneity, "stage13_regional_spatial_heterogeneity.csv")
write_csv_safe(lag_associations, "stage14_lagged_environment_associations.csv")
write_csv_safe(best_lags, "stage15_best_lags.csv")
write_csv_safe(source_pair_best_lags, "stage15_source_pair_best_lags.csv")
write_csv_safe(primary_eligibility, "stage15_primary_eligibility.csv")
write_csv_safe(validation_manifest, "stage15_validation_manifest.csv")

validation_workbook <- list(
  S01_EnvInventory = env_inventory,
  S01_RequiredCols = required_check,
  S01_SchemaAfter = schema_after,
  S01_DictColMismatch = workbook_cols_missing_dictionary,
  S01_DictCodeMismatch = dictionary_codes_missing_workbook,
  S01_WeekMetadata = week_metadata_audit,
  S02_Structure = structure,
  S02_YearCoverage = year_cov,
  S02_WeekCompleteness = week_summary,
  S02_MissingWeeks = missing_weeks,
  S03_EnvCoverage = env_coverage_all,
  S03_MERRA2_SH = ms_sh_coverage_setting,
  S04_Duplicates = duplicate_table,
  S04_Temporal = temporal_validity,
  S05_EnvDomains = env_domain_checks,
  S05_DengueDomains = case_domain_checks,
  S06_Ordering = ordering_checks,
  S06_Derived = derived_consistency,
  S06_PressureGap = pressure_gap_consistency,
  S07_FlagBinary = flag_binary,
  S07_FlagVerification = flag_verification,
  S07_TerminalGap = terminal_gap_rows,
  S08_PlausSummary = plaus_summary,
  S08_PlausDetail = plaus_detail,
  S09_QC_Pairs = qc_pairs,
  S09_QC_CrossSource = qc_cross_source,
  S10_MS_Pairs = ms_pairs,
  S10_MS_CrossSource = ms_cross_source,
  S11_ByYear = temporal_source_stability,
  S12_LOYO_Detail = loyo_detail,
  S12_LOYO_Summary = loyo_summary,
  S13_CrossScale = cross_scale_results,
  S14_LaggedAssoc = lag_associations,
  S15_BestLags = best_lags,
  S15_SourceLagPairs = source_pair_best_lags,
  S15_Manifest = validation_manifest
)

writexl::write_xlsx(
  validation_workbook,
  path = file.path(out_dir, "environmental_validation_report.xlsx")
)


# =============================================================================
# REPRODUCIBILITY LOG
# =============================================================================

run_ts <- as.character(Sys.time())

run_log <- c(
  "Dengue Environmental Variables Technical Validation",
  "Version 3.0",
  "",
  paste0("Input: ", normalizePath(PATH)),
  paste0("Output: ", normalizePath(out_dir)),
  paste0("Timestamp: ", run_ts),
  paste0("R: ", R.version$version.string),
  paste0("Seed: 12345"),
  paste0("Primary excluded years: ", paste(PRIMARY_EXCLUDE_YEARS, collapse = ", ")),
  paste0("QC primary years: ", paste(QC_PRIMARY_YEARS, collapse = ", ")),
  "",
  "Interpretation limits:",
  "- Cross-source agreement is not proof that either source is correct.",
  "- PAGASA series are references unless station provenance is documented.",
  "- SH_PAGASA is not fully independent of ERA5-Land because ERA5-Land pressure is used in its derivation.",
  "- MERRA-2 SH strict analyses retain only weeks with at least 4 valid source days.",
  "- Cross-scale comparisons assess representativeness, not direct interchangeability.",
  "- Lagged dengue-environment associations are descriptive and not causal."
)

writeLines(run_log, file.path(out_dir, "run_log.txt"))

sink(file.path(out_dir, "session_info.txt"))
cat("Timestamp:", run_ts, "\n\n")
sessionInfo()
sink()

cat("\nOutputs saved to: ", normalizePath(out_dir), "\n", sep = "")
cat("Master workbook: environmental_validation_report.xlsx\n")
cat("Run log: run_log.txt\n")
cat("Session information: session_info.txt\n")
cat("Validation workflow completed.\n")
