# Happiness, Unemployment & Trust in Government — ESS Data Analysis

## Overview

This project analyses data from the **European Social Survey (ESS)** to investigate factors associated with **trust in government**.

The analysis focuses on whether individual characteristics and socioeconomic factors — particularly **happiness, unemployment, age, gender, and occupation** — are associated with levels of trust in government.

The project uses **R** for data cleaning, descriptive statistics, visualisation, correlation analysis, and multiple linear regression.

## Research Question

**To what extent are happiness, unemployment, age, gender, and occupation associated with trust in government?**

## Variables

The analysis uses the following key variables:

| Variable | Description |
|---|---|
| `happy` | Happiness, measured from 0–10 |
| `stfgov` | Trust/satisfaction with government, measured from 0–10 |
| `uemp3m` | Whether the respondent was unemployed during the previous 3 months |
| `agea` | Respondent's age |
| `gndr` | Respondent's gender |
| `isco08` | Occupation classification using ISCO-08 |
| `occ_group` | Grouped occupational categories |

### Occupational Groups

ISCO-08 occupation codes are grouped into:

- Managers
- Professionals
- Technicians
- Clerical
- Service
- Skilled Manual
- Machine Operators
- Elementary
- Other

### Age Groups

Age is also categorised into:

- 18–24
- 25–34
- 35–44
- 45–54
- 55–64
- 65+

## Data Preparation

Several ESS datasets are imported and merged using the respondent identification variable `idno`.

The datasets include:

- `age.csv`
- `gender.csv`
- `occupation.csv`
- `ue.csv`
- `happy.csv`
- `stfgov.csv`

Duplicate observations are removed so that each respondent contributes one observation to the analysis.

The data are then cleaned by:

- Removing invalid unemployment responses
- Restricting happiness and government trust measures to their 0–10 scales
- Removing invalid occupation codes
- Removing missing observations
- Converting gender and unemployment variables into factors
- Creating grouped age and occupation variables

## Statistical Analysis

### Descriptive Statistics

Descriptive statistics are calculated for:

- Age
- Happiness
- Trust in government
- Occupation
- Unemployment

The analysis includes measures such as:

- Minimum
- Maximum
- Mean
- Median
- Standard deviation

Frequency and percentage tables are also produced for categorical variables such as gender, unemployment status, age group, and occupation group.

## Visualisations

Two main visualisations are produced.

### Figure 1 — Happiness vs Trust in Government

A scatterplot with a LOESS trend line is used to examine the relationship between happiness and trust in government.

The resulting figure is saved as:

`Figure1_Happiness_Trust.png`

### Figure 2 — Unemployment vs Trust in Government

A boxplot compares levels of trust in government between respondents who were and were not unemployed during the previous three months.

The resulting figure is saved as:

`Figure2_Unemployment_Trust.png`

## Correlation Analysis

A correlation matrix is calculated for the numerical variables:

- Age
- Happiness
- Trust in government
- ISCO-08 occupation code
- Unemployment

Both correlation coefficients and associated p-values are examined.

## Regression Analysis

Multiple linear regression models are used to investigate predictors of trust in government.

The main model is:

```r
lm(stfgov ~ happy + fct_unemp + agea + fct_gender + occ_group,
   data = data_clean)
