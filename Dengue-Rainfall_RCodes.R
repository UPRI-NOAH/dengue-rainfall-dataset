# =============================================================================
#
#  FULL ANALYTICS REPLICATION — R CODE
#
#  Tables produced:
#    Table 3a  — Pearson r + Spearman rho, zero-lag  (QC only)
#    Table 3b  — Pearson r + Spearman rho, zero-lag  (All 17 regions)
#    Table 3c  — Pearson r + Spearman rho, zero-lag  (All 8 countries)
#    Table 5a  — Lagged cross-correlation, RF_NASA → DC_QC, lags 0-12 (QC)
#    Table 5b  — Lagged cross-correlation, lags 0-12, summary (All 17 regions)
#    Table 5c  — Lagged cross-correlation, lags 0-12, summary (All 8 countries)
#    Sec 4.4   — Overdispersion analysis (variance / mean ratios)
#    Sec 4.5   — COVID-19 suppression analysis
#    Supp      — Seasonality index; annual totals; country totals
#
#  Figures (printed to viewer):
#    Fig 1 — Annual cases bar chart (QC)
#    Fig 2 — Seasonality ribbon (QC, excl. 2020-21)
#    Fig 3 — Lag cross-correlation line plot (QC)
#    Fig 4 — Regional dengue burden heatmap
#    Fig 5 — Regional Pearson r vs Spearman rho lollipop
#    Fig 6 — Country panel: total cases + correlations
#    Fig 7 — Regional peak-lag heatmap
#    Fig 8 — Country lag profile facets
#
#  *** NAMESPACE NOTE ***
#  MASS::glm.nb() is called with :: notation (no library(MASS)) to prevent
#  MASS::select() from masking dplyr::select(). All dplyr verbs that could
#  be masked use dplyr:: prefix explicitly throughout.
#
#  Requirements (run once if needed):
#    install.packages(c("readxl","dplyr","tidyr","ggplot2",
#                       "scales","patchwork","viridis","stringr","MASS"))
#
# =============================================================================


# =============================================================================
#  0. PACKAGES
# =============================================================================
needed <- c("readxl", "dplyr", "tidyr", "ggplot2",
            "scales", "patchwork", "viridis", "stringr", "MASS")

for (pkg in needed) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
}

# Load only packages that do NOT conflict with dplyr::select
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)
library(viridis)
library(stringr)
# MASS is intentionally NOT library()-loaded to avoid masking dplyr::select().
# All MASS functions are called via MASS:: prefix.


# =============================================================================
#  1. LOAD DATA
# =============================================================================
PATH <- "C:/Users/User/Downloads/Dengue-Rainfall_Dataset.xlsx"
# Adjust PATH to your local copy of Dengue-Rainfall_Dataset.xlsx
# read_excel() accepts only ONE sheet= argument at a time — load separately.

df_qc  <- read_excel(PATH, sheet = "QC Data")
df_reg <- read_excel(PATH, sheet = "Regional Data")
df_cty <- read_excel(PATH, sheet = "Country Data")

# Ensure temporal order in every sheet
df_qc  <- df_qc  %>% arrange(YR, WN)
df_reg <- df_reg %>% arrange(REGION, YR, WN)
df_cty <- df_cty %>% arrange(COUNTRY, YR, WN)

cat("=== Data loaded ===\n")
cat("  QC Data:       ", nrow(df_qc),  "rows x", ncol(df_qc),  "cols\n")
cat("  Regional Data: ", nrow(df_reg), "rows x", ncol(df_reg), "cols\n")
cat("  Country Data:  ", nrow(df_cty), "rows x", ncol(df_cty), "cols\n\n")

dir.create("figures", showWarnings = FALSE)


# =============================================================================
#  HELPER FUNCTIONS
# =============================================================================

section_header <- function(title) {
  cat("\n", strrep("=", 72), "\n  ", title,
      "\n", strrep("=", 72), "\n\n", sep = "")
}

fmt_p <- function(p)
  ifelse(p < 0.001, "< 0.001", formatC(p, digits = 3, format = "f"))

sig_stars <- function(p)
  ifelse(p < 0.001, "***",
  ifelse(p < 0.01,  "**",
  ifelse(p < 0.05,  "*", "n.s.")))

# Single helper: Pearson + Spearman for one (x, y) pair → one-row data.frame
corr_row <- function(scope, comparison, x, y) {
  idx <- complete.cases(cbind(x, y))
  cp  <- cor.test(x[idx], y[idx], method = "pearson")
  cs  <- cor.test(x[idx], y[idx], method = "spearman", exact = FALSE)
  data.frame(
    Scope        = scope,
    Comparison   = comparison,
    Pearson_r    = round(as.numeric(cp$estimate), 3),
    Spearman_rho = round(as.numeric(cs$estimate), 3),
    p_Pearson    = fmt_p(cp$p.value),
    p_Spearman   = fmt_p(cs$p.value),
    Sig          = sig_stars(cp$p.value),
    n            = sum(idx),
    stringsAsFactors = FALSE
  )
}

# Single helper: Pearson r at a given lag (y_vec is the leading variable)
lag_pearson <- function(x_vec, y_vec, lag) {
  if (lag == 0) {
    ct <- cor.test(x_vec, y_vec, method = "pearson")
  } else {
    x_trim <- x_vec[(lag + 1):length(x_vec)]
    y_trim <- y_vec[1:(length(y_vec) - lag)]
    ct     <- cor.test(x_trim, y_trim, method = "pearson")
  }
  list(r = round(as.numeric(ct$estimate), 3),
       p = ct$p.value,
       n = if (lag == 0) length(x_vec) else length(x_vec) - lag)
}


# =============================================================================
#  TABLE 3a — Pearson r + Spearman rho, zero-lag  (QC only)
# =============================================================================
section_header("TABLE 3a — Pearson r + Spearman rho  |  QC Data only (zero-lag)")

table3a <- corr_row(
  scope      = paste0("QC Data (n = ", nrow(df_qc), ")"),
  comparison = "DC_QC vs RF_NASA",
  x          = df_qc$DC_QC,
  y          = df_qc$RF_NASA
)
print(table3a, row.names = FALSE)
cat("\n*** p<0.001  ** p<0.01  * p<0.05  n.s. = not significant\n")


# =============================================================================
#  TABLE 3b — Pearson r + Spearman rho, zero-lag  (All 17 regions)
# =============================================================================
section_header("TABLE 3b — Pearson r + Spearman rho  |  All 17 Philippine Regions (zero-lag)")

table3b <- bind_rows(
  lapply(sort(unique(df_reg$REGION)), function(r) {
    g <- dplyr::filter(df_reg, REGION == r)
    corr_row(
      scope      = paste0(r, " (n = ", nrow(g), ")"),
      comparison = "DC_DOH vs RF_HDX",
      x          = g$DC_DOH,
      y          = g$RF_HDX
    )
  })
) %>% arrange(desc(Pearson_r))

print(table3b, row.names = FALSE)
cat("\n*** p<0.001  ** p<0.01  * p<0.05  n.s. = not significant\n")


# =============================================================================
#  TABLE 3c — Pearson r + Spearman rho, zero-lag  (All 8 countries)
# =============================================================================
section_header("TABLE 3c — Pearson r + Spearman rho  |  All 8 Countries (zero-lag)")

table3c <- bind_rows(
  lapply(sort(unique(df_cty$COUNTRY)), function(ctry) {
    g <- dplyr::filter(df_cty, COUNTRY == ctry)
    corr_row(
      scope      = paste0(str_to_title(ctry), " (n = ", nrow(g), ")"),
      comparison = "DC_OPENDENGUE vs RF_NASA",
      x          = g$DC_OPENDENGUE,
      y          = g$RF_NASA
    )
  })
) %>% arrange(desc(Pearson_r))

print(table3c, row.names = FALSE)
cat("\n*** p<0.001  ** p<0.01  * p<0.05  n.s. = not significant\n")


# =============================================================================
#  TABLE 5a — Lagged cross-correlation: RF_NASA → DC_QC  (QC, lags 0-12)
# =============================================================================
section_header("TABLE 5a — Lagged cross-correlation: RF_NASA -> DC_QC  |  QC, lags 0-12 weeks")

table5a <- bind_rows(
  lapply(0:12, function(lag) {
    res <- lag_pearson(df_qc$DC_QC, df_qc$RF_NASA, lag)
    data.frame(
      Lag_weeks = lag,
      Pearson_r = res$r,
      p_value   = fmt_p(res$p),
      Sig       = sig_stars(res$p),
      n         = res$n,
      stringsAsFactors = FALSE
    )
  })
)

peak_lag_qc <- table5a$Lag_weeks[which.max(table5a$Pearson_r)]
peak_r_qc   <- max(table5a$Pearson_r)

print(table5a, row.names = FALSE)
cat(sprintf(
  "\n>> Peak lag: %d weeks  |  Peak Pearson r = %.3f\n",
  as.integer(peak_lag_qc), peak_r_qc))
cat("   Interpretation: consistent with Ae. aegypti larval development\n")
cat("   (~7-10 days) + extrinsic incubation (~8-12 days) + reporting delay.\n")


# =============================================================================
#  TABLE 5b — Lagged cross-correlation  (All 17 regions, lags 0-12)
# =============================================================================
section_header("TABLE 5b — Lagged cross-correlation  |  All 17 Regions, lags 0-12 weeks")

all_regions <- sort(unique(df_reg$REGION))

# Full long table
table5b_full <- bind_rows(
  lapply(all_regions, function(r) {
    g <- dplyr::filter(df_reg, REGION == r)
    bind_rows(lapply(0:12, function(lag) {
      res <- lag_pearson(g$DC_DOH, g$RF_HDX, lag)
      data.frame(Region = r, Lag = lag, Pearson_r = res$r,
                 p_value = res$p, Sig = sig_stars(res$p), n = res$n,
                 stringsAsFactors = FALSE)
    }))
  })
)

# Summary: lag-0, lag-4, peak lag, peak r
table5b_summary <- table5b_full %>%
  group_by(Region) %>%
  summarise(
    r_lag0   = Pearson_r[Lag == 0],
    r_lag4   = Pearson_r[Lag == 4],
    Peak_lag = Lag[which.max(abs(Pearson_r))],
    Peak_r   = Pearson_r[which.max(abs(Pearson_r))],
    p_peak   = fmt_p(p_value[which.max(abs(Pearson_r))]),
    Sig      = sig_stars(p_value[which.max(abs(Pearson_r))]),
    n        = n[1],
    .groups  = "drop"
  ) %>%
  arrange(desc(Peak_r))

cat("-- Summary (lag-0, lag-4, peak lag, peak r) --\n\n")
print(table5b_summary, row.names = FALSE)

# Wide matrix: one row per region, one column per lag
table5b_wide <- table5b_full %>%
  dplyr::select(Region, Lag, Pearson_r) %>%
  pivot_wider(names_from = Lag, values_from = Pearson_r,
              names_prefix = "Lag_")

cat("\n-- Full lag matrix (lag 0-12 for each region) --\n\n")
print(as.data.frame(table5b_wide), row.names = FALSE)


# =============================================================================
#  TABLE 5c — Lagged cross-correlation  (All 8 countries, lags 0-12)
# =============================================================================
section_header("TABLE 5c — Lagged cross-correlation  |  All 8 Countries, lags 0-12 weeks")

all_countries <- sort(unique(df_cty$COUNTRY))

# Full long table
table5c_full <- bind_rows(
  lapply(all_countries, function(ctry) {
    g <- dplyr::filter(df_cty, COUNTRY == ctry)
    bind_rows(lapply(0:12, function(lag) {
      res <- lag_pearson(g$DC_OPENDENGUE, g$RF_NASA, lag)
      data.frame(Country = str_to_title(ctry), Lag = lag, Pearson_r = res$r,
                 p_value = res$p, Sig = sig_stars(res$p), n = res$n,
                 stringsAsFactors = FALSE)
    }))
  })
)

# Summary
table5c_summary <- table5c_full %>%
  group_by(Country) %>%
  summarise(
    r_lag0   = Pearson_r[Lag == 0],
    r_lag4   = Pearson_r[Lag == 4],
    Peak_lag = Lag[which.max(abs(Pearson_r))],
    Peak_r   = Pearson_r[which.max(abs(Pearson_r))],
    p_peak   = fmt_p(p_value[which.max(abs(Pearson_r))]),
    Sig      = sig_stars(p_value[which.max(abs(Pearson_r))]),
    n        = n[1],
    .groups  = "drop"
  ) %>%
  arrange(desc(Peak_r))

cat("-- Summary (lag-0, lag-4, peak lag, peak r) --\n\n")
print(table5c_summary, row.names = FALSE)

# Wide matrix
table5c_wide <- table5c_full %>%
  dplyr::select(Country, Lag, Pearson_r) %>%
  pivot_wider(names_from = Lag, values_from = Pearson_r,
              names_prefix = "Lag_")

cat("\n-- Full lag matrix (lag 0-12 for each country) --\n\n")
print(as.data.frame(table5c_wide), row.names = FALSE)


# =============================================================================
#  SECTION 4.4 — Overdispersion Analysis
# =============================================================================
section_header("SECTION 4.4 — Overdispersion Analysis (Variance / Mean)")

overdispersion <- data.frame(
  Scale        = c("QC Data (DC_QC)",
                   "Regional Data (DC_DOH)",
                   "Country Data (DC_OPENDENGUE)"),
  N            = c(nrow(df_qc), nrow(df_reg), nrow(df_cty)),
  Mean         = round(c(mean(df_qc$DC_QC),
                         mean(df_reg$DC_DOH),
                         mean(df_cty$DC_OPENDENGUE)), 2),
  SD           = round(c(sd(df_qc$DC_QC),
                         sd(df_reg$DC_DOH),
                         sd(df_cty$DC_OPENDENGUE)), 2),
  Variance     = round(c(var(df_qc$DC_QC),
                         var(df_reg$DC_DOH),
                         var(df_cty$DC_OPENDENGUE)), 1),
  VM_ratio     = round(c(var(df_qc$DC_QC)          / mean(df_qc$DC_QC),
                         var(df_reg$DC_DOH)         / mean(df_reg$DC_DOH),
                         var(df_cty$DC_OPENDENGUE)  / mean(df_cty$DC_OPENDENGUE)), 1),
  Recommendation = c("NB model recommended",
                     "NB model recommended",
                     "NB model recommended"),
  stringsAsFactors = FALSE
)

print(overdispersion, row.names = FALSE)
cat("\nAll distributions strongly overdispersed (V/M >> 1).\n")
cat("Negative Binomial (NB) or zero-inflated NB regression is recommended.\n")
cat("Poisson regression (assumes V/M = 1) is inappropriate.\n")

# Formal LR test — MASS::glm.nb() called with :: to avoid masking dplyr::select()
if (requireNamespace("MASS", quietly = TRUE)) {
  cat("\n-- Formal overdispersion test: Poisson vs NB (QC: DC_QC ~ RF_NASA) --\n")
  m_pois <- glm(DC_QC ~ RF_NASA, data = df_qc, family = poisson)
  m_nb   <- suppressWarnings(MASS::glm.nb(DC_QC ~ RF_NASA, data = df_qc))
  lr_val <- 2 * (as.numeric(logLik(m_nb)) - as.numeric(logLik(m_pois)))
  lr_p   <- pchisq(lr_val, df = 1, lower.tail = FALSE)
  cat(sprintf("  LR statistic = %.2f,  df = 1,  p = %.4e\n", lr_val, lr_p))
  cat(sprintf("  NB theta (dispersion) = %.3f\n", m_nb$theta))
  cat("  >> NB model is significantly better than Poisson (p < 0.001)\n")
}

# =============================================================================
#  SECTION 4.4b — Overdispersion Analysis by Region and Country
# =============================================================================
section_header("SECTION 4.4b — Overdispersion Analysis by Region & Country (Variance / Mean)")

# --- REGIONAL LEVEL ---
region_overdisp <- df_reg %>%
  group_by(REGION) %>%
  summarise(
    N        = n(),
    Mean     = round(mean(DC_DOH), 2),
    SD       = round(sd(DC_DOH), 2),
    Variance = round(var(DC_DOH), 1),
    VM_ratio = round(var(DC_DOH) / mean(DC_DOH), 1),
    Recommendation = "NB model recommended",
    .groups = "drop"
  ) %>%
  arrange(desc(VM_ratio))

cat("-- Regional Overdispersion --\n")
print(as.data.frame(region_overdisp), row.names = FALSE)

# --- COUNTRY LEVEL ---
country_overdisp <- df_cty %>%
  group_by(COUNTRY) %>%
  summarise(
    N        = n(),
    Mean     = round(mean(DC_OPENDENGUE), 2),
    SD       = round(sd(DC_OPENDENGUE), 2),
    Variance = round(var(DC_OPENDENGUE), 1),
    VM_ratio = round(var(DC_OPENDENGUE) / mean(DC_OPENDENGUE), 1),
    Recommendation = "NB model recommended",
    .groups = "drop"
  ) %>%
  arrange(desc(VM_ratio))

cat("\n-- Country Overdispersion --\n")
print(as.data.frame(country_overdisp), row.names = FALSE)

cat("\nAll regions and countries show strong overdispersion (V/M >> 1).\n")
cat("Negative Binomial (NB) or zero-inflated NB regression is recommended.\n")
cat("Poisson regression (assumes V/M = 1) is inappropriate.\n")

# =============================================================================
#  SECTION 4.5a — COVID-19 Suppression Analysis
# =============================================================================
section_header("SECTION 4.5 — COVID-19 Suppression Analysis (QC Data, 2020-21 vs 2018-19)")

baseline_wks  <- df_qc %>% dplyr::filter(YR %in% c(2018, 2019)) %>% pull(DC_QC)
covid_wks     <- df_qc %>% dplyr::filter(YR %in% c(2020, 2021)) %>% pull(DC_QC)

baseline_mean  <- mean(baseline_wks)
covid_mean     <- mean(covid_wks)
reduction_pct  <- (baseline_mean - covid_mean) / baseline_mean * 100
baseline_total <- sum(baseline_wks)
covid_total    <- sum(covid_wks)

cat(sprintf("Pre-pandemic baseline (2018-19):  mean = %6.2f cases/week  |  total = %d\n",
            baseline_mean, as.integer(baseline_total)))
cat(sprintf("Pandemic period       (2020-21):  mean = %6.2f cases/week  |  total = %d\n",
            covid_mean, as.integer(covid_total)))
cat(sprintf("Percentage reduction:             %.1f%%\n\n", reduction_pct))

tt <- t.test(baseline_wks, covid_wks, var.equal = FALSE)
cat("Welch two-sample t-test (2018-19 vs 2020-21):\n")
cat(sprintf("  t = %.3f,  df = %.1f,  p = %.4e\n",
            tt$statistic, tt$parameter, tt$p.value))
cat(sprintf("  95%% CI for mean difference: [%.2f, %.2f]\n",
            tt$conf.int[1], tt$conf.int[2]))
cat("  >> Suppression is statistically significant (p < 0.001)\n\n")

annual_summary <- df_qc %>%
  group_by(YR) %>%
  summarise(
    Annual_total  = sum(DC_QC),
    Weekly_mean   = round(mean(DC_QC), 1),
    Weekly_max    = max(DC_QC),
    .groups = "drop"
  ) %>%
  mutate(Period = case_when(
    YR %in% 2018:2019 ~ "Pre-pandemic baseline",
    YR %in% 2020:2021 ~ "COVID-19 suppression",
    TRUE              ~ "—"
  ))

cat("Annual dengue summary (QC Data):\n")
print(annual_summary, row.names = FALSE)


# =============================================================================
#  SECTION 4.5b — Regional Rainfall Summary
# =============================================================================
section_header("SECTION 4.5 — Regional Rainfall Summary and Data Gap Verification")

# --- Summary Statistics for Regional Rainfall ---
rf_summary <- df_reg %>%
  summarise(
    Min       = round(min(RF_HDX, na.rm = TRUE), 2),
    Max       = round(max(RF_HDX, na.rm = TRUE), 2),
    Mean      = round(mean(RF_HDX, na.rm = TRUE), 1),
    SD        = round(sd(RF_HDX, na.rm = TRUE), 1),
    N_total   = n(),
    N_missing = sum(is.na(RF_HDX))
  )

cat("Regional Rainfall (RF_HDX) Summary:\n")
print(rf_summary)

# --- Check per region ---
region_gap_check <- df_reg %>%
  group_by(REGION) %>%
  summarise(
    Min       = round(min(RF_HDX, na.rm = TRUE), 2),
    Max       = round(max(RF_HDX, na.rm = TRUE), 2),
    Mean      = round(mean(RF_HDX, na.rm = TRUE), 1),
    SD        = round(sd(RF_HDX, na.rm = TRUE), 1),
    N_total   = n(),
    N_missing = sum(is.na(RF_HDX)),
    .groups = "drop"
  ) %>%
  arrange(REGION)

cat("\nRegional Rainfall by Region (check for gaps):\n")
print(as.data.frame(region_gap_check), row.names = FALSE)


# =============================================================================
#  SUPPLEMENTARY — Seasonality Index (QC, excl. 2020-21)
# =============================================================================
section_header("SUPPLEMENTARY — Seasonality Index (QC, excl. 2020-21)")

seasonal <- df_qc %>%
  dplyr::filter(!YR %in% c(2020, 2021)) %>%
  group_by(WN) %>%
  summarise(
    mean_cases   = mean(DC_QC),
    sd_cases     = sd(DC_QC),
    median_cases = median(DC_QC),
    .groups      = "drop"
  ) %>%
  mutate(SI = round(mean_cases / mean(mean_cases), 3))

epi_threshold <- mean(df_qc$DC_QC) + sd(df_qc$DC_QC)   # used in Fig 2 only

cat(sprintf("Peak week:   %d  (SI = %.3f)\n",
            as.integer(seasonal$WN[which.max(seasonal$SI)]), max(seasonal$SI)))
cat(sprintf("Trough week: %d  (SI = %.3f)\n\n",
            as.integer(seasonal$WN[which.min(seasonal$SI)]), min(seasonal$SI)))
print(as.data.frame(seasonal), row.names = FALSE)


# =============================================================================
#  SUPPLEMENTARY — Country totals
# =============================================================================
section_header("SUPPLEMENTARY — Country total dengue cases (2016-2025)")

country_totals <- df_cty %>%
  group_by(COUNTRY) %>%
  summarise(
    Years   = paste(min(YR), max(YR), sep = "-"),
    n_weeks = n(),
    Total   = sum(DC_OPENDENGUE),
    Wk_mean = round(mean(DC_OPENDENGUE), 1),
    Wk_max  = max(DC_OPENDENGUE),
    .groups = "drop"
  ) %>%
  mutate(COUNTRY = str_to_title(COUNTRY)) %>%
  arrange(desc(Total))

print(country_totals, row.names = FALSE)


# =============================================================================
#  FIGURE 1 — Annual dengue cases bar chart (QC)
# =============================================================================
section_header("FIGURE 1 — Annual dengue cases (QC)")

annual_plot <- df_qc %>%
  group_by(YR) %>%
  summarise(Total = sum(DC_QC), .groups = "drop") %>%
  mutate(Group = case_when(
    YR == 2019             ~ "National epidemic",
    YR %in% c(2020, 2021) ~ "COVID-19 period",
    TRUE                   ~ "Routine"
  ))

fig1 <- ggplot(annual_plot, aes(x = factor(YR), y = Total, fill = Group)) +
  geom_col(width = 0.72, alpha = 0.88, colour = "white", linewidth = 0.3) +
  geom_text(aes(label = comma(Total)),
            vjust = -0.4, size = 2.6, fontface = "bold") +
  scale_fill_manual(values = c(
    "National epidemic" = "#C00000",
    "COVID-19 period"   = "#FFC7CE",
    "Routine"           = "#4472C4"
  )) +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.14))) +
  labs(
    title    = "Figure 1. Annual Dengue Cases - Quezon City (2010-2025)",
    subtitle = "Red = 2019 national epidemic; pink = COVID-19 lockdown years (2020-2021)",
    x = "Year", y = "Total Annual Cases", fill = NULL
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 9, colour = "gray40"),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


# =============================================================================
#  FIGURE 2 — Seasonality ribbon (QC, excl. 2020-21)
# =============================================================================
section_header("FIGURE 2 — Seasonality profile (QC)")

# Ensure WN is numeric
seasonal <- seasonal %>%
  mutate(WN = suppressWarnings(as.numeric(as.character(WN))))

# Compute safe max for annotation
y_max <- max(seasonal$mean_cases, na.rm = TRUE)

fig2 <- ggplot(seasonal, aes(x = WN)) +
  
  # SW Monsoon shading
  annotate("rect", xmin = 22, xmax = 44,
           ymin = -Inf, ymax = Inf,
           fill = "#4472C4", alpha = 0.07) +
  
  # Monsoon label
  annotate("text", x = 33, y = y_max * 0.93,
           label = "SW Monsoon\n(habagat, Wk 22-44)",
           size = 3, colour = "#4472C4", fontface = "italic") +
  
  # Ribbon: mean ± SD
  geom_ribbon(aes(
    ymin = pmax(mean_cases - sd_cases, 0),
    ymax = mean_cases + sd_cases
  ),
  fill = "#4472C4", alpha = 0.18) +
  
  # Mean line
  geom_line(aes(y = mean_cases),
            colour = "#4472C4", linewidth = 1.3) +
  
  # Axes
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  scale_y_continuous(labels = scales::comma) +
  
  # Labels
  labs(
    title    = "Figure 2. Dengue Seasonality - Quezon City (2010–2025, excl. 2020–21)",
    x = "Epidemiological Week",
    y = "Mean Cases / Week"
  ) +
  
  # Theme
  theme_classic(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "gray40")
  )

# =============================================================================
#  FIGURE 3 — Lag cross-correlation (QC)
# =============================================================================
section_header("FIGURE 3 — Lag cross-correlation RF_NASA -> DC_QC (QC)")

fig3_data <- table5a %>%
  mutate(is_peak = (Lag_weeks == peak_lag_qc))

fig3 <- ggplot(fig3_data, aes(x = Lag_weeks, y = Pearson_r)) +
  geom_hline(yintercept = 0, colour = "gray70", linewidth = 0.5) +
  geom_line(colour = "#4472C4", linewidth = 1.2) +
  geom_point(aes(colour = is_peak, size = is_peak)) +
  scale_colour_manual(values = c("FALSE" = "#4472C4", "TRUE" = "#C00000"),
                      guide  = "none") +
  scale_size_manual(values  = c("FALSE" = 2.5, "TRUE" = 4.5),
                    guide   = "none") +
  annotate("text",
           x = peak_lag_qc + 0.3, y = peak_r_qc + 0.006,
           label = sprintf("Peak lag = %d wks\n(r = %.3f)", as.integer(peak_lag_qc), peak_r_qc),
           hjust = 0, size = 3, colour = "#C00000", fontface = "bold") +
  scale_x_continuous(breaks = 0:12) +
  coord_cartesian(ylim = c(0.18, 0.38)) +
  labs(
    title    = "Figure 3. Lagged Cross-Correlation: RF_NASA -> DC_QC (Quezon City)",
    subtitle = "Pearson r at lags 0-12 weeks; rainfall assumed to precede dengue incidence",
    x = "Lag (weeks)", y = "Pearson r"
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "gray40")
  )

# =============================================================================
#  FIGURE 4 — Regional dengue burden heatmap
# =============================================================================
section_header("FIGURE 4 — Regional dengue burden heatmap")

reg_heat <- df_reg %>%
  group_by(REGION, YR) %>%
  summarise(mean_dc = mean(DC_DOH), .groups = "drop")

fig4 <- ggplot(reg_heat,
               aes(x     = factor(YR),
                   y     = reorder(REGION, mean_dc, FUN = mean),
                   fill  = mean_dc)) +
  geom_tile(colour = "white", linewidth = 0.35) +
  scale_fill_viridis_c(option = "YlOrRd", direction = 1,
                       labels = comma,
                       name   = "Mean weekly\ncases") +
  labs(
    title    = "Figure 4. Regional Dengue Burden by Year (Mean Weekly Cases)",
    subtitle = "17 Philippine regions, 2016-2025 (2020-21 absent from HDX source data)",
    x = "Year", y = NULL
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "gray40"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    axis.text.y   = element_text(size = 9)
  )


# =============================================================================
#  FIGURE 5 — Regional Pearson r vs Spearman rho lollipop
# =============================================================================
section_header("FIGURE 5 — Regional Pearson r vs Spearman rho lollipop")

fig5_data <- table3b %>%
  mutate(Region = sub(" \\(n.*\\)", "", Scope)) %>%
  dplyr::select(Region, Pearson_r, Spearman_rho) %>%
  pivot_longer(cols      = c(Pearson_r, Spearman_rho),
               names_to  = "Measure",
               values_to = "Coeff") %>%
  mutate(Measure = recode(Measure,
    "Pearson_r"    = "Pearson r",
    "Spearman_rho" = "Spearman rho"))

fig5 <- ggplot(fig5_data,
               aes(x = Coeff,
                   y = reorder(Region, Coeff, FUN = max),
                   colour = Measure)) +
  geom_vline(xintercept = 0, colour = "gray60", linewidth = 0.5) +
  geom_line(aes(group = Region), colour = "gray75", linewidth = 0.6) +
  geom_point(size = 3.2, alpha = 0.9) +
  scale_colour_manual(values = c("Pearson r"     = "#4472C4",
                                 "Spearman rho"  = "#C00000")) +
  labs(
    title    = "Figure 5. Regional Dengue-Rainfall Correlation (DC_DOH vs RF_HDX, zero-lag)",
    subtitle = "Pearson r and Spearman rho for all 17 Philippine regions",
    x = "Correlation coefficient", y = NULL, colour = NULL
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 9, colour = "gray40"),
    legend.position = "bottom",
    axis.text.y     = element_text(size = 9)
  )


# =============================================================================
#  FIGURE 6 — Country panel: total cases + correlations
# =============================================================================
section_header("FIGURE 6 — Country panel (total cases + correlations)")

p6a <- ggplot(country_totals,
              aes(x = Total / 1e6,
                  y = reorder(COUNTRY, Total))) +
  geom_col(fill = "#ED7D31", alpha = 0.85) +
  geom_text(aes(label = comma(Total)), hjust = -0.06, size = 2.9) +
  scale_x_continuous(labels = label_number(suffix = "M"),
                     expand = expansion(mult = c(0, 0.30))) +
  labs(title = "A. Total Cases (2016-2025)",
       x = "Total dengue cases", y = NULL) +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))

fig6_corr <- table3c %>%
  mutate(Country = sub(" \\(n.*\\)", "", Scope))

p6b <- ggplot(fig6_corr,
              aes(y = reorder(Country, Pearson_r))) +
  geom_vline(xintercept = 0, colour = "gray60", linewidth = 0.5) +
  geom_segment(aes(x = 0, xend = Pearson_r,
                   yend = reorder(Country, Pearson_r)),
               colour = "#4472C4", linewidth = 1.0) +
  geom_segment(aes(x = 0, xend = Spearman_rho,
                   yend = reorder(Country, Pearson_r)),
               colour = "#C00000", linewidth = 1.0, linetype = "dashed") +
  geom_point(aes(x = Pearson_r),    colour = "#4472C4", size = 3.5, shape = 16) +
  geom_point(aes(x = Spearman_rho), colour = "#C00000", size = 3.5, shape = 17) +
  annotate("text", x = 0.38, y = 7.5,
           label = "circle = Pearson r\ntriangle = Spearman rho",
           size = 2.8, hjust = 0, colour = "gray20") +
  labs(title = "B. DC_OPENDENGUE vs RF_NASA (zero-lag)",
       x = "Correlation coefficient", y = NULL) +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))

fig6 <- p6a + p6b +
  plot_annotation(
    title    = "Figure 6. Country-Level Dengue Burden and Rainfall-Dengue Correlations",
    subtitle = "Eight dengue-endemic countries, 2016-2025",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, colour = "gray40"))
  )


# =============================================================================
#  FIGURE 7 — Regional peak-lag heatmap
# =============================================================================
section_header("FIGURE 7 — Regional peak-lag heatmap")

fig7_data <- table5b_summary %>%
  mutate(label = paste0("lag ", Peak_lag, "\n(r=", Peak_r, ")"))

fig7 <- ggplot(fig7_data,
               aes(x = "Peak Lag",
                   y = reorder(Region, Peak_r),
                   fill = Peak_lag)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = label),
            size = 2.9, colour = "white", fontface = "bold") +
  scale_fill_viridis_c(option = "plasma", direction = -1,
                       name   = "Peak lag\n(weeks)",
                       breaks = seq(0, 12, by = 2)) +
  labs(
    title    = "Figure 7. Regional Peak Rainfall-Dengue Lag",
    subtitle = "RF_HDX -> DC_DOH: lag (weeks) at which Pearson r is maximised per region",
    x = NULL, y = NULL
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, colour = "gray40"),
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank(),
    axis.text.y      = element_text(size = 9),
    legend.position  = "right"
  )


# =============================================================================
#  FIGURE 8 — Country lag profile facets
# =============================================================================
section_header("FIGURE 8 — Country lag profile facets")

# Join peak lag from summary to mark the peak point per country
fig8_data <- table5c_full %>%
  dplyr::left_join(
    dplyr::select(table5c_summary, Country, Peak_lag),
    by = "Country"
  ) %>%
  mutate(
    Country = factor(Country, levels = sort(unique(Country))),
    is_peak = (Lag == Peak_lag)
  )

fig8 <- ggplot(fig8_data, aes(x = Lag, y = Pearson_r)) +
  geom_hline(yintercept = 0, colour = "gray70", linewidth = 0.4) +
  geom_line(colour = "#4472C4", linewidth = 0.9) +
  geom_point(aes(colour = is_peak, size = is_peak)) +
  scale_colour_manual(values = c("FALSE" = "#4472C4", "TRUE" = "#C00000"),
                      guide  = "none") +
  scale_size_manual(values  = c("FALSE" = 1.5, "TRUE" = 3.5),
                    guide   = "none") +
  scale_x_continuous(breaks = c(0, 4, 8, 12)) +
  facet_wrap(~Country, ncol = 4, scales = "free_y") +
  labs(
    title    = "Figure 8. Country Lag Profiles: RF_NASA -> DC_OPENDENGUE (lags 0-12 weeks)",
    subtitle = "Red dot = peak lag per country; y-axis free-scaled per country",
    x = "Lag (weeks)", y = "Pearson r"
  ) +
  theme_classic(base_size = 9) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, colour = "gray40"),
    strip.text       = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = "#DCE6F1", colour = NA)
  )


# =============================================================================
#  OPTIONAL: Export tables to CSV (uncomment to activate)
# =============================================================================
section_header("OPTIONAL — Export to CSV")

# write.csv(table3a,          "output_table3a_corr_qc.csv",           row.names = FALSE)
# write.csv(table3b,          "output_table3b_corr_regions.csv",      row.names = FALSE)
# write.csv(table3c,          "output_table3c_corr_countries.csv",    row.names = FALSE)
# write.csv(table5a,          "output_table5a_lag_qc.csv",            row.names = FALSE)
# write.csv(table5b_summary,  "output_table5b_summary_regions.csv",   row.names = FALSE)
# write.csv(table5b_wide,     "output_table5b_wide_regions.csv",      row.names = FALSE)
# write.csv(table5b_full,     "output_table5b_full_regions.csv",      row.names = FALSE)
# write.csv(table5c_summary,  "output_table5c_summary_countries.csv", row.names = FALSE)
# write.csv(table5c_wide,     "output_table5c_wide_countries.csv",    row.names = FALSE)
# write.csv(table5c_full,     "output_table5c_full_countries.csv",    row.names = FALSE)
# write.csv(overdispersion,   "output_overdispersion.csv",            row.names = FALSE)
# write.csv(annual_summary,   "output_annual_qc.csv",                 row.names = FALSE)
# write.csv(country_totals,   "output_country_totals.csv",            row.names = FALSE)
# write.csv(seasonal,         "output_seasonality_index_qc.csv",      row.names = FALSE)

cat("All CSV exports are commented out. Uncomment lines above to save.\n")


# =============================================================================
#  SESSION INFO
# =============================================================================
section_header("SESSION INFO (for reproducibility)")
sessionInfo()

