# Dengue–Rainfall Dataset
### Weekly Dengue Incidence Linked to Satellite Rainfall at City, Regional, and Country Scales

---

## Overview

This repository contains a harmonised, analysis-ready, multi-scale dataset linking **weekly dengue surveillance records** with **concurrent satellite-derived rainfall** across three geographic levels — city, region, and country — together with supporting spatial datasets for mapping and subnational analysis. All files are distributed at the repository root.

**Core Dataset — `Dengue-Rainfall_Dataset.xlsx`**

| Sheet | Geographic scope | Period | Records |
|---|---|---|---|
| **QC Data** | Quezon City, Metro Manila | 2010–2025 | 832 |
| **Regional Data** | 17 Philippine administrative regions | 2016–2025 (excl. 2020–2021) | 7,072 |
| **Country Data** | 8 dengue-endemic countries | 2016–2025 (country-specific) | 3,216 |

**Total: 11,120 complete week-resolution records.**

Each sheet contains 7 columns: four core data variables and three binary quality-flag columns. A separate **Data Dictionary** sheet documents all variables.

**Spatial and Supplementary Data**

- Barangay-level shapefile for Quezon City (split archive: `DC_Regional-Brgy.7z.001` / `DC_Regional-Brgy.7z.002`)
- Regional-level shapefile for the Philippines (same archive)
- CSV counterparts for both spatial datasets (`QC_YearlyData-Barangay(CSV).csv`, `PH_REGIONS-DC.csv`)

These files support geospatial visualisation, hotspot mapping, and spatial epidemiological analysis.

---

## Repository Structure

All files are distributed at the repository root:

```
dengue-rainfall-dataset/
│
├── Dengue-Rainfall_Dataset.xlsx          # Main dataset (QC Data, Regional Data, Country Data, Data Dictionary)
├── technical_validation_report.xlsx      # Full 10-stage validation outputs (32 sheets)
│
├── Dengue-Rainfall_Validation.R          # Technical validation script (10-stage QC pipeline)
├── Dengue-Rainfall_RCodes.R              # Analytics replication script (Figures 1–8)
│
├── QC_YearlyData-Barangay(CSV).csv       # Barangay-level yearly dengue cases (142 barangays)
├── PH_REGIONS-DC.csv                     # Regional yearly dengue cases (17 regions)
│
├── DC_Regional-Brgy.7z.001              # Shapefile archive — part 1 of 2
├── DC_Regional-Brgy.7z.002              # Shapefile archive — part 2 of 2
│
├── LICENSE                               # ODC-ODbL v1.0
├── CITATION.cff
└── README.md
```

> **Shapefile access:** Extract `DC_Regional-Brgy.7z.001` and `DC_Regional-Brgy.7z.002` together using [7-Zip](https://www.7-zip.org/) or WinRAR before use.

---

## Data Sources

| Source | Description | Used for |
|---|---|---|
| **QCESD** | Quezon City Epidemiology and Surveillance Division (PIDSR) | QC dengue cases |
| **NASA GES DISC** | GPM IMERG Final Run v07 (0.1° × 0.1°) | QC and Country rainfall |
| **DOH** | Department of Health — Freedom of Information (FOI form 2026-031, released 30 Jan 2026) | Regional dengue cases |
| **HDX** | Philippines Subnational Rainfall Indicators — CHIRPS v2 dekadal data | Regional rainfall |
| **OpenDengue** | Clarke et al. (2024) *Sci. Data* 11:296 | Country dengue cases |

---

## Dataset Description

### Sheet 1 — QC Data (832 records)

Weekly dengue surveillance and rainfall for **Quezon City**, Metro Manila — the most populous city in the Philippines.

| Variable | Type | Unit | Description | Source |
|---|---|---|---|---|
| `YR` | Integer | Year | Calendar year (2010–2025) | — |
| `WN` | Integer | Week | ISO epidemiological week number (1–52 or 1–53) | — |
| `DC_QC` | Integer | Count | Weekly dengue cases | QCESD / PIDSR |
| `RF_NASA` | Numeric | mm/week | Weekly rainfall from NASA GPM IMERG Final Run v07, single 0.1° × 0.1° grid cell nearest to city centroid | NASA |
| `FLAG_COVID` | Integer | 0/1 | 1 = record falls in 2020–2021 COVID-19 disruption period | — |
| `FLAG_SINGLE_CELL_RF` | Integer | 0/1 | 1 = RF_NASA derived from a single IMERG centroid cell (all QC rows = 1) | — |
| `FLAG_PLAUSIBILITY` | Integer | 0/1 | 1 = week-over-week dengue change exceeds ±500% (advisory; no QC rows flagged) | — |

> **Structural gaps:** Week 53 is absent in 2015 and 2020 (ISO long years not captured by PIDSR). `FLAG_COVID = 1` for all 104 records in 2020–2021; these rows reflect a mixture of genuine transmission reduction and potential under-reporting.

---

### Sheet 2 — Regional Data (7,072 records)

Weekly dengue and rainfall for **all 17 Philippine administrative regions**: NCR, CAR, REGION I–XIII, MIMAROPA, and BARMM.

| Variable | Type | Unit | Description | Source |
|---|---|---|---|---|
| `REGION` | Text | — | Philippine administrative region name | — |
| `YR` | Integer | Year | Calendar year (2016–2025, excluding 2020–2021) | — |
| `WN` | Integer | Week | ISO epidemiological week number (1–52 or 1–53) | — |
| `DC_DOH` | Integer | Count | Weekly dengue cases by region | DOH / FOI 2026-031 |
| `RF_HDX` | Numeric | mm/week | Weekly rainfall disaggregated from CHIRPS v2 dekadal totals via HDX | HDX / CHIRPS v2 |
| `FLAG_DEKADAL_APPROX` | Integer | 0/1 | 1 = RF_HDX is a proportional disaggregation estimate from 10-day totals (all Regional rows = 1) | — |
| `FLAG_PLAUSIBILITY` | Integer | 0/1 | 1 = week-over-week dengue change exceeds ±500% (advisory; 6 Regional rows flagged) | — |

> **Structural gap:** Years 2020–2021 are absent for all 17 regions because the DOH database contains no records for this period, likely due to COVID-19 disruption of routine surveillance. Both years are wholly absent — there are no rows to flag. `RF_HDX` values are proportional disaggregates of 10-day CHIRPS totals and should not be compared directly with `RF_NASA` values from other sheets without prior harmonisation.

> **FLAG_PLAUSIBILITY rows (6):** BARMM 2017 WK50, REGION I 2023 WK1, REGION II 2016 WK52, REGION VI 2019 WK52, REGION VII 2017 WK1, REGION X 2023 WK2. All arise from year-boundary or small-count transitions; none indicate data errors.

---

### Sheet 3 — Country Data (3,216 records)

Weekly dengue and rainfall for **8 dengue-endemic countries** with continuous weekly reporting available in OpenDengue from 2016 onward.

| Variable | Type | Unit | Description | Source |
|---|---|---|---|---|
| `COUNTRY` | Text | — | Country name | — |
| `YR` | Integer | Year | Calendar year (range varies by country) | — |
| `WN` | Integer | Week | ISO epidemiological week number (1–52 or 1–53) | — |
| `DC_OPENDENGUE` | Integer | Count | Weekly dengue cases | OpenDengue |
| `RF_NASA` | Numeric | mm/week | Weekly rainfall from NASA GPM IMERG Final Run v07, national area-weighted mean | NASA |
| `FLAG_SINGLE_CELL_RF` | Integer | 0/1 | 1 = RF_NASA is an area-weighted mean across all IMERG cells (note: all Country rows = 1) | — |
| `FLAG_TERMINAL_GAP` | Integer | 0/1 | 1 = row is present but falls at the reporting boundary where completeness may be uncertain | — |

Countries: **Brazil, Colombia, Mexico, Peru, Philippines, Singapore, Sri Lanka, Taiwan**

**Country-level year coverage:**

| Country | Years covered | Missing years |
|---|---|---|
| Brazil | 2016–2023 | 2020 |
| Colombia | 2016–2023 | 2019, 2020 |
| Mexico | 2016–2023 | 2020 |
| Peru | 2016–2023 | 2020 |
| Philippines | 2016–2023 | 2021 |
| Singapore | 2016–2025 | — |
| Sri Lanka | 2016–2024 | — |
| Taiwan | 2016–2024 | — |

> **Structural week gaps:** Philippines 2020 WK53, 2022 WK51–52, 2023 WK46–50; Singapore 2020 WK53; Sri Lanka 2020 WK53, 2023 WK52; Taiwan 2020 WK53 — all structurally absent (no rows). Philippines 2023 WK51–52 are present with `FLAG_TERMINAL_GAP = 1`. Vietnam and Indonesia were excluded because weekly-resolution case series were not available in OpenDengue at the time of assembly.

---

## Data Quality Flags

Five binary columns encode record-level quality metadata. A value of `1` indicates the condition applies; `0` indicates it does not. All flag columns were verified against expected values in `technical_validation_report.xlsx` (Stage 10, all Match = TRUE).

| Flag | Sheet(s) | Rows = 1 | Meaning |
|---|---|---|---|
| `FLAG_COVID` | QC Data | 104 | Record falls in 2020–2021; may reflect surveillance disruption |
| `FLAG_SINGLE_CELL_RF` | QC Data, Country Data | 832 (QC), 3,216 (Country) | RF_NASA derived from single IMERG cell (QC) or area-weighted mean (Country) |
| `FLAG_PLAUSIBILITY` | QC Data, Regional Data | 0 (QC), 6 (Regional) | Week-over-week case change exceeds ±500% (advisory only) |
| `FLAG_DEKADAL_APPROX` | Regional Data | 7,072 | RF_HDX is a proportional disaggregation of 10-day CHIRPS totals |
| `FLAG_TERMINAL_GAP` | Country Data | 2 | Row present at terminal reporting boundary; completeness uncertain |

---

## Spatial Datasets

### Shapefiles — `DC_Regional-Brgy.7z.001` / `DC_Regional-Brgy.7z.002`

Extract both parts together to obtain the geopackage `DC_Regional-Brgy.gpkg`, which contains two layers:

**`QC_Brgy.shp` — Quezon City barangay boundaries**

| Field | Description |
|---|---|
| `ADM4_PCODE` | PSA geographic code (Philippine Standard Geographic Code) |
| `BRGY` | Barangay name |
| `DC_YEAR` | Annual dengue case count per barangay (2010–2025) |
| `DC_MEAN` | Mean annual cases per barangay, 2010–2025 |

**`PH_Regions-DC.shp` — Philippine regional boundaries**

| Field | Description |
|---|---|
| `ADM1_PCODE` | PSA geographic code |
| `Region` | Administrative region name |
| `DC_YEAR` | Annual dengue case count per region |
| `DC_MEAN` | Mean annual cases per region, 2016–2019 and 2022–2025 |

---

### CSV Files

**`QC_YearlyData-Barangay(CSV).csv`** — 142 barangay records, yearly dengue totals for Quezon City.

| ADM4_PCODE | BRGY | DC_2010 | DC_2011 | DC_2012 | … |
|---|---|---|---|---|---|
| PH1307404001 | Alicia | 34 | 28 | 52 | … |
| PH1307404002 | Amihan | 4 | 16 | 12 | … |
| PH1307404003 | Apolonio Samson | 64 | 86 | 129 | … |
| PH1307404004 | Aurora | 10 | 17 | 10 | … |
| PH1307404005 | Baesa | 105 | 154 | 171 | … |

**`PH_REGIONS-DC.csv`** — 17 region records, yearly dengue totals for Philippine regions.

| ADM1_PCODE | Region | DC_2016 | DC_2017 | DC_2018 | … |
|---|---|---|---|---|---|
| PH01 | Region I (Ilocos Region) | 8,281 | 8,284 | 14,804 | … |
| PH02 | Region II (Cagayan Valley) | 3,891 | 5,327 | 17,926 | … |
| PH03 | Region III (Central Luzon) | 20,989 | 24,935 | 31,759 | … |
| PH04 | Region IV-A (CALABARZON) | 24,282 | 22,403 | 30,293 | … |

---

## Technical Validation

The dataset was validated using a 10-stage quality-control pipeline implemented in `Dengue-Rainfall_Validation.R`. All outputs are compiled in `technical_validation_report.xlsx` (32 sheets).

| Stage | Check | QC Data | Regional Data | Country Data |
|---|---|---|---|---|
| 1 | Dataset structure and row reconciliation | 832 ✓ | 7,072 ✓ | 3,216 ✓ |
| 2 | Cell-level completeness | 0 NA | 0 NA | 0 NA |
| 3 | Structural year coverage | Complete | 2020–2021 absent | Variable by country |
| 4 | Structural week completeness | WK53 absent: 2015, 2020 | Complete | Multiple terminal gaps |
| 5 | Duplicate composite keys | 0 | 0 | 0 |
| 6 | Value-domain validation | No negatives | No negatives | No negatives |
| 7 | Schema and type verification | WN coerced to integer | WN coerced to integer | WN coerced to integer |
| 8 | Temporal validity and plausibility | 0 flags | 6 advisory flags | — |
| 9 | Cross-scale descriptive summaries | Annual + seasonal CSV | Annual + seasonal CSV | Annual + seasonal CSV |
| 10 | Data quality flag verification | All Match = TRUE | All Match = TRUE | All Match = TRUE |

Running `Dengue-Rainfall_Validation.R` on the distributed `Dengue-Rainfall_Dataset.xlsx` reproduces all Stage 10 results with Match = TRUE for every flag column.

---

## Quick Start

### Requirements

```
R >= 4.1.0
```

### Required Packages — Validation Script

```r
readxl, dplyr, tidyr, stringr, purrr, writexl, tibble
```

### Required Packages — Analytics Script

```r
readxl, dplyr, tidyr, ggplot2, scales, patchwork, viridis, stringr, MASS
```

### Install

```r
install.packages(c(
  "readxl", "dplyr", "tidyr", "ggplot2",
  "scales", "patchwork", "viridis", "stringr", "MASS",
  "purrr", "writexl", "tibble"
))
```

### Load the Dataset

Set the `PATH` variable to the location of `Dengue-Rainfall_Dataset.xlsx` on your system before running either script.

```r
library(readxl)
library(dplyr)

# Update path to your local copy
PATH <- "Dengue-Rainfall_Dataset.xlsx"

df_qc  <- read_excel(PATH, sheet = "QC Data",       skip = 1) %>% arrange(YR, WN)
df_reg <- read_excel(PATH, sheet = "Regional Data",  skip = 1) %>% arrange(REGION, YR, WN)
df_cty <- read_excel(PATH, sheet = "Country Data",   skip = 1) %>% arrange(COUNTRY, YR, WN)

cat("QC Data:       ", nrow(df_qc),  "records\n")   # expected: 832
cat("Regional Data: ", nrow(df_reg), "records\n")   # expected: 7,072
cat("Country Data:  ", nrow(df_cty), "records\n")   # expected: 3,216
```

> **Note:** Each sheet has a one-row informational banner (row 1) followed by column headers (row 2). The `skip = 1` argument in `read_excel()` handles this correctly. The `Dengue-Rainfall_Validation.R` script uses the same convention.

### Run the Validation Script

```r
# Update PATH inside the script before running (line 83)
source("Dengue-Rainfall_Validation.R")
```

Outputs are written to a `technical_validation_outputs/` subdirectory and consolidated into `technical_validation_report.xlsx`.

### Run the Analytics Replication Script

```r
# Update PATH inside the script before running
source("Dengue-Rainfall_RCodes.R")
```

This script produces:

1. Zero-lag Pearson and Spearman correlations
2. Lagged cross-correlation analysis
3. Overdispersion analysis (variance-to-mean ratios; NB vs. Poisson LR test)
4. COVID-19 suppression analysis
5. Supplementary outputs: seasonality index, annual summaries, country totals
6. Figures 1–8: QC, regional, and country-level visualisations

### Sample: Overdispersion Analysis

```r
library(dplyr)
library(MASS)

PATH <- "Dengue-Rainfall_Dataset.xlsx"

df_qc  <- readxl::read_excel(PATH, sheet = "QC Data",      skip = 1) %>% arrange(YR, WN)
df_reg <- readxl::read_excel(PATH, sheet = "Regional Data", skip = 1) %>% arrange(REGION, YR, WN)
df_cty <- readxl::read_excel(PATH, sheet = "Country Data",  skip = 1) %>% arrange(COUNTRY, YR, WN)

# Overall variance-to-mean ratios
overdispersion <- data.frame(
  Scale    = c("QC Data (DC_QC)",
               "Regional Data (DC_DOH)",
               "Country Data (DC_OPENDENGUE)"),
  N        = c(nrow(df_qc), nrow(df_reg), nrow(df_cty)),
  Mean     = round(c(mean(df_qc$DC_QC),
                     mean(df_reg$DC_DOH),
                     mean(df_cty$DC_OPENDENGUE)), 2),
  SD       = round(c(sd(df_qc$DC_QC),
                     sd(df_reg$DC_DOH),
                     sd(df_cty$DC_OPENDENGUE)), 2),
  Variance = round(c(var(df_qc$DC_QC),
                     var(df_reg$DC_DOH),
                     var(df_cty$DC_OPENDENGUE)), 1),
  VM_ratio = round(c(var(df_qc$DC_QC)          / mean(df_qc$DC_QC),
                     var(df_reg$DC_DOH)         / mean(df_reg$DC_DOH),
                     var(df_cty$DC_OPENDENGUE)  / mean(df_cty$DC_OPENDENGUE)), 1)
)
print(overdispersion)

# Formal LR test: Poisson vs. Negative Binomial (QC)
m_pois <- glm(DC_QC ~ RF_NASA, data = df_qc, family = poisson)
m_nb   <- suppressWarnings(MASS::glm.nb(DC_QC ~ RF_NASA, data = df_qc))
lr_val <- 2 * (as.numeric(logLik(m_nb)) - as.numeric(logLik(m_pois)))
lr_p   <- pchisq(lr_val, df = 1, lower.tail = FALSE)
cat(sprintf("\nLR test (QC): statistic = %.2f, p = %.4e\n", lr_val, lr_p))
cat(sprintf("NB theta = %.3f  → NB model strongly preferred\n", m_nb$theta))
```

---

## Dataset Characteristics

### QC Data Summary Statistics

| Metric | DC_QC | RF_NASA |
|---|---|---|
| Records | 832 | 832 |
| Mean | 110.2 cases/week | 55.6 mm/week |
| Standard deviation | 102.0 | 58.8 mm |
| Minimum | 1 | 1.4 mm |
| Maximum | 697 | 456.0 mm |

The QC series shows a pronounced seasonal pattern, with dengue peaks typically in weeks 32–36 (southwest monsoon period). The minimum RF_NASA value of 1.4 mm/week reflects IMERG's gauge-calibration quantisation floor, not an absence of rainfall; zero-rainfall weeks are not present in the QC series.

### Notable Epidemiological Events in QC

| Year | Feature | Value | Interpretation |
|---|---|---|---|
| 2019 | Peak weekly dengue cases | 697 | Major epidemic-year peak |
| 2020 | Maximum weekly dengue cases | 183 | Suppressed during COVID-19 (FLAG_COVID = 1) |
| 2021 | Maximum weekly dengue cases | 49 | Continued suppression (FLAG_COVID = 1) |
| 2025 | Running annual total | 11,107 | Highest running annual total in the QC series |

---

## Known Limitations

1. **Regional 2020–2021 data gap.** Years 2020 and 2021 are wholly absent for all 17 regions in the DOH database, reflecting COVID-19 disruption to routine surveillance. Long-term trend analyses should use interrupted time-series methods or imputation.

2. **QC 2020–2021 suppression.** Quezon City data for 2020–2021 (`FLAG_COVID = 1`) reflect a mixture of genuine transmission reduction and under-reporting. These records are retained but should be treated as a structural break in secular trend analyses.

3. **RF_HDX temporal approximation.** Regional rainfall (`RF_HDX`) is derived by proportional disaggregation of 10-day CHIRPS totals to ISO weeks. This assumes a uniform within-dekad daily distribution and may misrepresent short-duration high-intensity typhoon events. `FLAG_DEKADAL_APPROX = 1` for all regional rows. ERA5-Land and NASA MERRA-2 provide directly weekly-aggregated alternatives.

4. **RF_NASA single-cell extraction (QC).** City-level rainfall is derived from the single 0.1° × 0.1° IMERG grid cell nearest the Quezon City centroid; sub-city spatial heterogeneity is not captured. `FLAG_SINGLE_CELL_RF = 1` for all QC rows.

5. **RF_NASA country-level averaging.** Country-level rainfall is a national area-weighted mean; sub-national variability in large or heterogeneous countries (Brazil, Colombia) is not resolved. `FLAG_SINGLE_CELL_RF = 1` for all Country rows.

6. **RF_NASA and RF_HDX are not directly comparable.** The two rainfall variables differ in retrieval algorithm, spatial resolution, temporal resolution before aggregation, and gauge calibration. They should not be pooled across sheets without prior harmonisation or bias correction.

7. **Country-level temporal coverage varies.** Brazil, Colombia, Mexico, and Peru extend through 2023 only; Colombia also lacks 2019. Singapore and Taiwan extend to 2025 and 2024 respectively; Sri Lanka to 2024. See the country coverage table above.

8. **Dengue counts are unadjusted surveillance data.** Under-reporting rates vary across sites and time periods. Site-specific adjustment should be applied in comparative analyses where estimates are available.

---

## Rainfall Product Notes

| Product | Used for | Spatial resolution | Temporal input | Notes |
|---|---|---|---|---|
| NASA GPM IMERG Final Run v07 | QC Data, Country Data | 0.1° × 0.1° | Daily → weekly sum | IMERG v07 has documented biases toward overestimating low-intensity and underestimating high-intensity events |
| CHIRPS v2 via HDX | Regional Data | 0.05° | Dekadal → weekly (proportional) | Area-weighted regional mean; disaggregation introduces uncertainty during typhoon episodes |

---

## Citation

If you use this dataset or any of its components, please cite:

```bibtex
@article{Matavia2025,
  title   = {Weekly Dengue Incidence Linked to Satellite Rainfall
             at City, Regional, and Country Scales},
  author  = {Matavia, Troy Owen and Pelitro, Keanu John and
             Soriano, Kylone and Garcia, Gereka Marie and
             Manzano, Julia Fye and Bilbao, Klara and
             Delos Angeles, Aira Joy and Lagmay, Alfredo Mahar and
             Bandoy, DJ Darwin},
  journal = {Nature Health Data},
  year    = {2025},
  doi     = {10.5281/zenodo.19347474},
  url     = {https://doi.org/10.5281/zenodo.19347474}
}
```

**OpenDengue** (source for country-level case data):

> Clarke, J. et al. A global dataset of publicly available dengue case count data. *Sci. Data* **11**, 296 (2024). https://doi.org/10.1038/s41597-024-02960-3

---

## License

The dataset and associated scripts are released under the **Open Data Commons Open Database License (ODC-ODbL) v1.0**.

You are free to share, distribute, and adapt the material for any purpose, provided that appropriate credit is given to **UP Resilience Institute – NOAH (UPRI-NOAH)** and its contributors. Any derivative database must be distributed under the same license (ODC-ODbL v1.0). Redistribution of derived OpenDengue outputs under ODbL is consistent with the OpenDengue licence terms as published in Clarke et al. (2024).

[ODbL v1.0](https://opendatacommons.org/licenses/odbl/1.0/)

---

## Ethics

This study was approved by the University of the Philippines Research Ethics Committee (REC Protocol No: 2024-0004-F-FMDS).

---

## Funding

Supported by the National Institute of Environmental Health Sciences of the National Institutes of Health (NIH) under Award Number P20ES036118, through the Center for Climate and Health Global Research on Disasters (CORD).

---

## Contact

For questions about the dataset, please open a GitHub Issue or contact:

**Keanu John A. Pelitro**
[kapelitro@up.edu.ph](mailto:kapelitro@up.edu.ph)
UP Resilience Institute — Research and Creative Work

---

*Data descriptor under review at Nature Health Data.*
