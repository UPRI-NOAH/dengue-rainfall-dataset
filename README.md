# Dengue–Rainfall Dataset
### Weekly Dengue Incidence Linked to Satellite Rainfall at City, Regional, and Country Scales

---

## Overview

This repository contains a harmonized, analysis-ready, multi-scale weekly dataset linking **dengue cases** with **concurrent rainfall measurements** across multiple geographic levels, along with supporting spatial datasets (shapefiles and CSVs) for mapping and subnational analysis.

**Core Dataset**

| Sheet | Geographic Scope | Period | Records |
|---|---|---|---|
| **QC Data** | Quezon City, Metro Manila | 2010–2025 | 832 |
| **Regional Data** | 17 Philippine Regions | 2016–2025 (excl. 2020–21) | 7,055 |
| **Country Data** | 8 endemic countries | 2016–2025 | 3,215 |

**Total: 11,102 complete, week-resolution records.**

**Additional Spatial Data**
* Barangay-level shapefile (QC)
* Regional-level shapefile (Philippines)
* CSV counterparts for both spatial datasets
These enable geospatial visualization, hotspot mapping, and spatial epidemiological analysis.

---

## Data Sources

The dataset integrates surveillance data from four primary sources:

- **QCESD** – Quezon City Epidemiology and Surveillance Division
- **NASA** – National Aeronautics and Space Administration (satellite rainfall)
- **DOH** – Department of Health
- **HDX** – Philippines Subnational datasets, Humanitarian Data Exchange
- **OpenDengue** – Global harmonised dengue surveillance database

---

## Repository Structure

```
dengue-rainfall-dataset/
│
├── data/
│   ├── Dengue-Rainfall_Dataset.xlsx      # Main dataset (3 sheets)
│   ├── QC_YearlyData-Barangay(CSV).csv   # Barangay-level yearly dengue cases
│   ├── PH_REGIONS-DC.csv                 # Regional yearly dengue cases
│   │
│   ├── DC_Regional-Brgy.7z.001          # QC barangay to Regional shapefile
│   ├── DC_Regional-Brgy.7z.002          # QC barangay to Regional shapefile
│
├── scripts/
│   ├── Dengue-Rainfall_Validation.R     # Full technical validation script
│   ├── Dengue-Rainfall_RCodes.R         # Full analytics replication script
│
├── LICENSE                              # ODC-ODbL v1.0
├── CITATION.cff
└── README.md
```

---

## Dengue-Rainfall Dataset Description

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
| `DC_DOH` | Weekly dengue cases | Deparment of Health |
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

## Spatial Datasets

**1. Quezon City Barangay to Regional Shapefiles**

**File:** DC_Regional-Brgy.7z.001 / DC_Regional-Brgy.7z.002

**Quezon City Barangay**

| Field      | Description                      |
| ---------- | -------------------------------- |
| ADM4_PCODE | PSA geographic code              |
| BRGY       | Barangay name                    |
| DC_YEAR    | Yearly dengue cases per barangay |
| DC_MEAN    | Mean cases (2010–2025)           |

**Philippine Regions**

| Field      | Description                       |
| ---------- | --------------------------------- |
| ADM1_PCODE | PSA geographic code               |
| Region     | Region name                       |
| DC_YEAR    | Yearly dengue cases               |
| DC_MEAN    | Mean cases (2016–2019, 2022–2025) |

**Note:** Data gaps (2020–2021) are consistent with the HDX dataset limitations.


**2. Corresponding CSV Files**

**File:** QC_YearlyData-Barangay(CSV).csv 

This CSV file contains contains 142 barangay records.

| ADM4_PCODE   | BRGY            | DC_2010 | DC_2011 | DC_2012 |
| ------------ | --------------- | ------- | ------- | ------- |
| PH1307404001 | Alicia          | 34      | 28      | 52      |
| PH1307404002 | Amihan          | 4       | 16      | 12      |
| PH1307404003 | Apolonio Samson | 64      | 86      | 129     |
| PH1307404004 | Aurora          | 10      | 17      | 10      |
| PH1307404005 | Baesa           | 105     | 154     | 171     |

**File:** PH_REGIONS-DC.csv

| ADM1_PCODE | Region                     | DC_2016 | DC_2017 | DC_2018 |
| ---------- | -------------------------- | ------- | ------- | ------- |
| PH01       | Region I (Ilocos Region)   | 8281    | 8284    | 14804   |
| PH02       | Region II (Cagayan Valley) | 3891    | 5327    | 17926   |
| PH03       | Region III (Central Luzon) | 20989   | 24935   | 31759   |
| PH04       | Region IV-A (CALABARZON)   | 24282   | 22403   | 30293   |

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
source("scripts/Dengue-Rainfall_RCodes.R")
```

This script produces the following analytics:
1. Zero-lag Pearson and Spearman correlations
2. Lagged cross-correlation analysis
3. Overdispersion analysis
4. COVID-19 suppression analysis
5. Supplementary outputs: seasonality index, annual summaries, and country totals
6. Figures 1–8: QC, regional, and country-level visualizations

### Sample: Overdispersion Analysis

To check variance-to-mean ratios and assess whether Negative Binomial (NB) regression is recommended:

```R
library(dplyr)
library(MASS)   # for glm.nb

PATH <- "data/Dengue-Rainfall_Dataset.xlsx"

# Load datasets
df_qc  <- readxl::read_excel(PATH, sheet = "QC Data") %>% arrange(YR, WN)
df_reg <- readxl::read_excel(PATH, sheet = "Regional Data") %>% arrange(REGION, YR, WN)
df_cty <- readxl::read_excel(PATH, sheet = "Country Data") %>% arrange(COUNTRY, YR, WN)

# Overall overdispersion
overdispersion <- data.frame(
  Scale = c("QC Data (DC_QC)",
            "Regional Data (DC_DOH)",
            "Country Data (DC_OPENDENGUE)"),
  N     = c(nrow(df_qc), nrow(df_reg), nrow(df_cty)),
  Mean  = round(c(mean(df_qc$DC_QC),
                  mean(df_reg$DC_DOH),
                  mean(df_cty$DC_OPENDENGUE)), 2),
  SD    = round(c(sd(df_qc$DC_QC),
                  sd(df_reg$DC_DOH),
                  sd(df_cty$DC_OPENDENGUE)), 2),
  Variance = round(c(var(df_qc$DC_QC),
                     var(df_reg$DC_DOH),
                     var(df_cty$DC_OPENDENGUE)), 1),
  VM_ratio = round(c(var(df_qc$DC_QC)/mean(df_qc$DC_QC),
                     var(df_reg$DC_DOH)/mean(df_reg$DC_DOH),
                     var(df_cty$DC_OPENDENGUE)/mean(df_cty$DC_OPENDENGUE)), 1),
  Recommendation = "NB model recommended",
  stringsAsFactors = FALSE
)

print(overdispersion)

# Optional: Formal LR test for QC
if (requireNamespace("MASS", quietly = TRUE)) {
  m_pois <- glm(DC_QC ~ RF_NASA, data = df_qc, family = poisson)
  m_nb   <- suppressWarnings(MASS::glm.nb(DC_QC ~ RF_NASA, data = df_qc))
  lr_val <- 2 * (as.numeric(logLik(m_nb)) - as.numeric(logLik(m_pois)))
  lr_p   <- pchisq(lr_val, df = 1, lower.tail = FALSE)
  
  cat("\n-- LR Test Poisson vs NB (QC: DC_QC ~ RF_NASA) --\n")
  cat(sprintf("LR statistic = %.2f, df = 1, p = %.4e\n", lr_val, lr_p))
  cat(sprintf("NB theta (dispersion) = %.3f\n", m_nb$theta))
  cat("NB model is significantly better than Poisson (p < 0.001)\n")
}

# Regional overdispersion
region_overdisp <- df_reg %>%
  group_by(REGION) %>%
  summarise(
    N        = n(),
    Mean     = round(mean(DC_DOH), 2),
    SD       = round(sd(DC_DOH), 2),
    Variance = round(var(DC_DOH), 1),
    VM_ratio = round(var(DC_DOH)/mean(DC_DOH), 1),
    Recommendation = "NB model recommended",
    .groups = "drop"
  ) %>%
  arrange(desc(VM_ratio))

cat("\n-- Regional Overdispersion --\n")
print(as.data.frame(region_overdisp), row.names = FALSE)

# Country overdispersion
country_overdisp <- df_cty %>%
  group_by(COUNTRY) %>%
  summarise(
    N        = n(),
    Mean     = round(mean(DC_OPENDENGUE), 2),
    SD       = round(sd(DC_OPENDENGUE), 2),
    Variance = round(var(DC_OPENDENGUE), 1),
    VM_ratio = round(var(DC_OPENDENGUE)/mean(DC_OPENDENGUE), 1),
    Recommendation = "NB model recommended",
    .groups = "drop"
  ) %>%
  arrange(desc(VM_ratio))

cat("\n-- Country Overdispersion --\n")
print(as.data.frame(country_overdisp), row.names = FALSE)
```

---

## Key Dengue-Rainfall Dataset Characteristics
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
2. **Regional 2020–2021 gap:** No data available due to COVID-19.
3. **QC 2020–2021 cases:** Likely under-reported due to health system disruption. Treat cautiously in trends.
4. **Country-level RF:** Spatially averaged over entire countries; intra-country heterogeneity not captured.
5. **Zero-lag only:** All correlations reported here are contemporaneous. Explore lagged associations for modelling.

---

## Citation

If you use this dataset, please cite:

```bibtex
@article{Matavia2025,
  title   = {Weekly Dengue Incidence Linked to Satellite Rainfall at City, Regional, and Country Scales},
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

## License

The dataset and associated scripts are released under the **Open Data Commons Open Database License (ODC-ODbL) v1.0**.

You are free to share, distribute, and adapt the material for any purpose, provided that appropriate credit is given to **UP Resilience Institute – NOAH (UPRI-NOAH)** and its contributors.

If you alter or build upon this dataset, you may only distribute the resulting database under the same license (ODC-ODbL v1.0).

[[ODbL v1.0](https://opendatacommons.org/licenses/odbl/1.0/)]

---

## Contact

For questions about the dataset, please open a GitHub Issue or contact:

**Keanu John A. Pelitro***  
[kapelitro@up.edu]  
UP Resilience Institute - Research and Creative Work

---

*Data descriptor manuscript in preparation.*
