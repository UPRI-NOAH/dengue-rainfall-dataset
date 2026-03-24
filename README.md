# Dengue–Rainfall Dataset
### Multi-Scale Weekly Dengue–Rainfall Dataset Across the Philippines and Endemic Countries (2010–2025)

---

## Overview

This repository contains a harmonized, analysis-ready, multi-scale weekly dataset linking **dengue cases** with **concurrent rainfall measurements** at three nested geographic levels:

| Sheet | Geographic Scope | Period | Records |
|---|---|---|---|
| **QC Data** | Quezon City, Metro Manila | 2010–2025 | 832 |
| **Regional Data** | 17 Philippine Regions | 2016–2025 (excl. 2020–21) | 7,055 |
| **Country Data** | 8 endemic countries | 2016–2025 | 3,215 |

**Total: 11,102 complete, week-resolution records.**

The dataset integrates surveillance data from four primary sources:

- **QCESD** – Quezon City Epidemiology and Surveillance Division
- **NASA** – National Aeronautics and Space Administration (satellite rainfall)
- **HDX** – Philippines Subnational datasets, Humanitarian Data Exchange
- **OpenDengue** – Global harmonised dengue surveillance database

---

## Repository Structure

```
dengue-rainfall-dataset/
│
├── data/
│   └── Dengue-Rainfall_Dataset.xlsx      # Main dataset (4 sheets)
│
├── scripts/
│   └── Dengue-Rainfall_RCodes.R            # Full analytics replication script 
│
├── CITATION.cff                           # Citation metadata
└── README.md                              # This file
```

---

## Dataset Description

### Sheet 1 — QC Data

Weekly dengue surveillance and rainfall data for **Quezon City**, the most populous city in Metro Manila, Philippines.

| Variable | Description | Source | Coverage |
|---|---|---|---|
| `YR` | Year | — | 2010–2025 |
| `WN` | Epidemiological week (1–52) | — | All years |
| `DC_QC` | Weekly dengue cases | QCESD | Complete (0% missing) |
| `RF_NASA` | Weekly rainfall – satellite (mm) | NASA | Complete (0% missing) |

> **Note:** RF_NASA covers the full period.

### Sheet 2 — Regional Data

Weekly dengue and rainfall data for **all 17 administrative regions** of the Philippines.

| Variable | Description | Source |
|---|---|---|
| `REGION` | Region name (NCR, CAR, REGION I–XIII, MIMAROPA, BARMM) | — |
| `YR` | Year | — |
| `WN` | Epidemiological week | — |
| `DC_HDX` | Weekly dengue cases | HDX Philippines Subnational |
| `RF_HDX` | Weekly rainfall (mm) | HDX Philippines Subnational |

> **Note:** Years 2020–2021 are absent from the HDX release for all regions (likely COVID-19 surveillance disruption).

### Sheet 3 — Country Data

Weekly dengue and rainfall data for **8 dengue-endemic countries**.

| Variable | Description | Source |
|---|---|---|
| `COUNTRY` | Country name | — |
| `YR` | Year | — |
| `WN` | Epidemiological week | — |
| `RF_NASA` | Weekly rainfall – satellite (mm) | NASA |
| `DC_OPENDENGUE` | Weekly dengue cases | OpenDengue |

Countries included: **Brazil, Colombia, Mexico, Peru, Philippines, Singapore, Sri Lanka, Taiwan**

---

## Quick Start

### Requirements

```R
R >= 4.1
```

### Required Packages

```R
readxl >= 1.4.0
dplyr >= 1.1.0
tidyr >= 1.3.0
ggplot2 >= 3.4.0
scales >= 1.2.0
patchwork >= 1.1.0
viridis >= 0.6.0
stringr >= 1.5.0
MASS >= 7.3-0
```

### Install

Install the required packages from CRAN:

```R
install.packages(c(
  "readxl", "dplyr", "tidyr", "ggplot2",
  "scales", "patchwork", "viridis", "stringr", "MASS"
))
```

### Load the Dataset

Place Dengue-Rainfall_Dataset.xlsx in your project folder, then update the file path as needed.

```R
library(readxl)
library(dplyr)

PATH <- "data/Dengue-Rainfall_Dataset.xlsx"

df_qc  <- read_excel(PATH, sheet = "QC Data") %>% arrange(YR, WN)
df_reg <- read_excel(PATH, sheet = "Regional Data") %>% arrange(REGION, YR, WN)
df_cty <- read_excel(PATH, sheet = "Country Data") %>% arrange(COUNTRY, YR, WN)

cat("QC Data:       ", nrow(df_qc),  "records\n")
cat("Regional Data: ", nrow(df_reg), "records\n")
cat("Country Data:  ", nrow(df_cty), "records\n")
```

# Run the Full Replication Script

```R
source("scripts/full_analytics_replication.R")
```

This script reproduces:
1. Tables 3a–3c: zero-lag Pearson and Spearman correlations
2. Tables 5a–5c: lagged cross-correlation analysis
3. Section 4.4: overdispersion analysis
4. Section 4.5: COVID-19 suppression analysis
5. Supplementary outputs: seasonality index, annual summaries, and country totals
6. Figures 1–8: QC, regional, and country-level visualizations

### Sample: Basic Correlation Analysis

To run a minimal example before executing the full script:

```R
library(readxl)
library(dplyr)

PATH <- "data/Dengue-Rainfall_Dataset.xlsx"

df_qc  <- read_excel(PATH, sheet = "QC Data") %>% arrange(YR, WN)
df_reg <- read_excel(PATH, sheet = "Regional Data") %>% arrange(REGION, YR, WN)

# Zero-lag correlation: QC dengue vs NASA rainfall
cor.test(df_qc$DC_QC, df_qc$RF_NASA, method = "pearson")

# Lagged correlation: 4-week lag
cor.test(
  df_qc$DC_QC[5:nrow(df_qc)],
  df_qc$RF_NASA[1:(nrow(df_qc) - 4)],
  method = "pearson"
)

# Regional zero-lag correlations
regional_corr <- bind_rows(
  lapply(sort(unique(df_reg$REGION)), function(r) {
    g <- dplyr::filter(df_reg, REGION == r)
    ct <- cor.test(g$DC_HDX, g$RF_HDX, method = "pearson")
    data.frame(
      REGION = r,
      Pearson_r = round(as.numeric(ct$estimate), 3),
      p_value = ct$p.value
    )
  })
)

print(regional_corr)
```

### Plot Dengue Seasonality (QC)

```R
library(readxl)
library(dplyr)
library(ggplot2)

PATH <- "data/Dengue-Rainfall_Dataset.xlsx"

df_qc <- read_excel(PATH, sheet = "QC Data") %>% arrange(YR, WN)

seasonal <- df_qc %>%
  filter(!YR %in% c(2020, 2021)) %>%
  group_by(WN) %>%
  summarise(
    mean_cases = mean(DC_QC, na.rm = TRUE),
    sd_cases   = sd(DC_QC, na.rm = TRUE),
    .groups    = "drop"
  )

dir.create("figures", showWarnings = FALSE)

ggplot(seasonal, aes(x = WN, y = mean_cases)) +
  annotate("rect", xmin = 22, xmax = 44, ymin = -Inf, ymax = Inf,
           fill = "blue", alpha = 0.08) +
  geom_ribbon(aes(
    ymin = pmax(mean_cases - sd_cases, 0),
    ymax = mean_cases + sd_cases
  ), alpha = 0.15, fill = "#C0392B") +
  geom_line(linewidth = 1.2, colour = "#C0392B") +
  labs(
    title = "Average Dengue Seasonality - Quezon City (2010-2025, excl. 2020-2021)",
    x = "Epidemiological Week",
    y = "Mean Weekly Dengue Cases"
  ) +
  theme_classic()
```

---

## Key Dataset Characteristics
Quezon City (QC) Data Summary

The QC Data sheet contains 832 weekly observations spanning 2010–2025 (52 epidemiological weeks per year). It includes weekly dengue case counts from the Quezon City Epidemiology and Surveillance Division (DC_QC) and weekly NASA satellite-derived rainfall totals (RF_NASA).

Dengue case counts range from 1 to 697 cases per week (mean = 110.2, SD = 102.0), while weekly rainfall ranges from 1.4 to 456.0 mm (mean = 55.6 mm, SD = 58.8 mm). The QC series shows a pronounced seasonal pattern, with annual dengue peaks typically occurring between Weeks 32 and 36, broadly corresponding to the southwest monsoon period.

### QC Data Summary Statistics

| Metric             | DC_QC |  RF_NASA |
| ------------------ | ----: | -------: |
| Count              |   832 |      832 |
| Mean               | 110.2 |  55.6 mm |
| Standard Deviation | 102.0 |  58.8 mm |
| Minimum            |     1 |   1.4 mm |
| Maximum            |   697 | 456.0 mm |

### Selected Epidemiological Features in QC

Several important epidemiological patterns are captured in the QC time series:
1. **2019** recorded the largest outbreak peak, reaching 697 cases in a single week.
2. **2020–2021** show marked suppression of reported dengue cases, coinciding with COVID-19-related disruptions.
3. **2025** recorded the highest running annual total (11,107 cases) in the QC series.

### Notable Epidemiological Events

| Year | Epidemiological Feature     | Value  | Interpretation                                    |
| ---- | --------------------------- | ------ | ------------------------------------------------- |
| 2019 | Peak weekly dengue cases    | 697    | Major epidemic-year peak in QC                    |
| 2020 | Maximum weekly dengue cases | 183    | Suppressed reporting/transmission during COVID-19 |
| 2021 | Maximum weekly dengue cases | 49     | Continued suppression during COVID-19             |
| 2025 | Running annual total        | 11,107 | Highest running annual total in the QC series     |

---

## Known Limitations
2. **Regional 2020–2021 gap:** HDX did not release regional data for these years; likely COVID-19-related.
3. **QC 2020–2021 cases:** Likely under-reported due to health system disruption. Treat cautiously in trends.
4. **Country-level RF:** Spatially averaged over entire countries; intra-country heterogeneity not captured.
5. **Zero-lag only:** All correlations reported here are contemporaneous. Explore lagged associations for modelling.

---

## Citation

If you use this dataset, please cite:

```bibtex
@article{Matavia2025,
  title   = {Multi-Scale Weekly Dengue--Rainfall Dataset Across the Philippines and Endemic Countries (2010--2025)},
  author  = {Matavia, Troy Owen and Pelitro, Keanu John and Manzano, Julia Fye and Soriano, Kylone and Bilbao, Klara and Garcia, Gereka Marie and Delos Angeles, Aira Joy and Lagmay, Alfredo Mahar and Bandoy, DJ Darwin},
  journal = {xxxx},
  volume  = {},
  number  = {},
  pages   = {},
  year    = {2025},
  doi     = {10.XXXX/XXXXXX},
  url     = {https://doi.org/10.XXXX/XXXXXX}
}
```

---

## Licence

No formal licence has been assigned to this dataset yet.

For permissions regarding use, distribution, or adaptation, please contact the authors.

## Contact

For questions about the dataset, please open a GitHub Issue or contact:

**Keanu John A. Pelitro***  
[kapelitro@up.edu]  
University of the Philippines Diliman

---

*Data descriptor manuscript in preparation.*
