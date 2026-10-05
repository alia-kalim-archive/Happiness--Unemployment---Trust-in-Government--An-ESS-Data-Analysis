setwd("C:/Users/Alia/Documents/sost")  
getwd()

library(tidyverse)  
library(haven)
library(janitor)
library(broom)
library(psych)

# Loading Datasets
age        <- read.csv("age.csv")
gender     <- read.csv("gender.csv")
occupation <- read.csv("occupation.csv")
ue         <- read.csv("ue.csv")
happy      <- read.csv("happy.csv")
stfgov     <- read.csv("stfgov.csv")  

age        <- age        %>% group_by(idno) %>% slice(1) %>% ungroup()
happy      <- happy      %>% group_by(idno) %>% slice(1) %>% ungroup()
occupation <- occupation %>% group_by(idno) %>% slice(1) %>% ungroup()
ue         <- ue         %>% group_by(idno) %>% slice(1) %>% ungroup()
gender     <- gender     %>% group_by(idno) %>% slice(1) %>% ungroup()
stfgov  <- stfgov        %>% group_by(idno) %>% slice(1) %>% ungroup()

# Merging all datasets into one
ess <- gender %>%
  full_join(age,        by = "idno") %>%
  full_join(happy,      by = "idno") %>%
  full_join(occupation, by = "idno") %>%
  full_join(ue,         by = "idno") %>%
  full_join(stfgov,     by = "idno")

#Cleaning the Data
data_clean <- ess %>%
  filter(
    uemp3m.y %in% c(1, 2),
    happy.x <= 10,
    agea.x <= 99,
    !isco08.x %in% c(66666, 77777, 88888, 99999),
    stfgov.y <= 10
  ) %>%
  select(happy = happy.x, gndr = gndr.x, agea = agea.x,
         isco08 = isco08.x, uemp3m = uemp3m.y, stfgov = stfgov.y) %>%
  mutate(
    fct_gender = factor(gndr, labels = c("Male", "Female")),
    fct_unemp  = factor(uemp3m, levels = c(1, 2), labels = c("Yes", "No"))
  ) %>%
  drop_na()

#grouping types of occupation
data_clean <- data_clean %>%
  mutate(
    occ_group = case_when(
      isco08 >= 0    & isco08 <= 1999  ~ "Managers",
      isco08 >= 2000 & isco08 <= 2999  ~ "Professionals",
      isco08 >= 3000 & isco08 <= 3999  ~ "Technicians",
      isco08 >= 4000 & isco08 <= 4999  ~ "Clerical",
      isco08 >= 5000 & isco08 <= 5999  ~ "Service",
      isco08 >= 6000 & isco08 <= 6999  ~ "Skilled Manual",
      isco08 >= 7000 & isco08 <= 7999  ~ "Machine Operators",
      isco08 >= 8000 & isco08 <= 8999  ~ "Elementary",
      TRUE ~ "Other"
    )
  )

data_clean <- data_clean %>%
  mutate(
    fct_age = case_when(
      agea < 25 ~ "18–24",
      agea < 35 ~ "25–34",
      agea < 45 ~ "35–44",
      agea < 55 ~ "45–54",
      agea < 65 ~ "55–64",
      agea >= 65 ~ "65+"
    ),
    fct_age = factor(fct_age, levels = c("18–24", "25–34", "35–44", "45–54", "55–64", "65+"))
  )

describe(data_clean %>% select(agea, happy, stfgov, uemp3m, isco08))

data_clean %>%
  tabyl(fct_gender) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()

data_clean %>%
  tabyl(fct_unemp) %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting()

data_clean %>%
  tabyl(stfgov) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting()

data_clean %>%
  tabyl(occ_group) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting()

data_clean %>%
  tabyl(agea) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting()

data_clean %>%
  tabyl(fct_age) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()

data_clean <- data_clean %>%
  filter(isco08 < 80000)

glimpse(data_clean)

#Table 1: Summary 
summary_table <- tibble::tibble(
  Variable = c("agea", "happy", "stfgov", "isco08", "uemp3m"),
  Type     = c("Continuous", "Ordinal", "Ordinal", "Continuous", "Binary"),
  Min      = sapply(data_clean[c("agea", "happy", "stfgov", "isco08", "uemp3m")], min),
  Max      = sapply(data_clean[c("agea", "happy", "stfgov", "isco08", "uemp3m")], max),
  Mean     = sapply(data_clean[c("agea", "happy", "stfgov", "isco08", "uemp3m")], mean),
  Median   = sapply(data_clean[c("agea", "happy", "stfgov", "isco08", "uemp3m")], median),
  SD       = sapply(data_clean[c("agea", "happy", "stfgov", "isco08", "uemp3m")], sd)
)

print(summary_table)


table(data_clean$fct_gender)
table(data_clean$fct_unemp)

# --- Visualisations ---

#visualation 1 between happiness and stgov (privilege)
ggplot(data_clean, aes(x = happy, y = stfgov)) +
  geom_jitter(alpha = 0.3, color = "gold", width = 0.3, height = 0.3) +
  geom_smooth(method = "loess", se = TRUE, color = "mediumvioletred") +
  labs(
    title = "Figure 1: Scatterplot of Happiness vs Trust in Government",
    x = "Happiness (0–10)",
    y = "Trust in Government (0–10)"
  ) 


#visualation 2 bw unemployement and stgov (socioeconomic)
ggplot(data_clean, aes(x = fct_unemp, y = stfgov)) +
  geom_boxplot(fill = "pink") +
  labs(title = "Figure 2: Boxplot of Trust in Government vs Unemployment Status",
       x = "Unemployed in Last 3 Months", y = "Trust in Government")



#Correlation matrix
numeric_vars <- data_clean %>% select(agea, happy, stfgov, isco08, uemp3m)
cor_matrix <- corr.test(numeric_vars)
cor_matrix$r
cor_matrix$p

#Regression

# Load required packages
library(tidyverse)
library(broom)
library(stargazer)

# Fit multiple linear regression model
model <- lm(stfgov ~ happy + fct_unemp + agea + fct_gender + isco08, data = data_clean)

# Tidy the model output
tidy_model <- tidy(model, conf.int = TRUE)
glance_model <- glance(model)

# Optional: View in console
print(tidy_model)
print(glance_model)

# Export regression table as HTML (copy-pasteable into Word)
stargazer(model,
          type = "html",
          out = "regression_output.html",  # Saves to file
          title = "Regression Results: Predicting Trust in Government",
          dep.var.labels = "Trust in Government",
          covariate.labels = c("Happiness", "Unemployed (Yes)", "Age", "Gender (Female)", "Occupation (ISCO08)"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          omit.stat = c("f", "ser"))  # Optional: omit F-stat and residual SE

# Export model fit stats separately if needed
model_fit <- data.frame(
  R_squared = glance_model$r.squared,
  Adjusted_R2 = glance_model$adj.r.squared,
  AIC = glance_model$AIC,
  BIC = glance_model$BIC,
  Num_Obs = glance_model$nobs
)
write.csv(model_fit, "model_fit_stats.csv", row.names = FALSE)


#Multiple Linear Regression
model <- lm(stfgov ~ happy + fct_unemp + agea + fct_gender + occ_group, data = data_clean)

# summary table
library(broom)
tidy_model <- tidy(model, conf.int = TRUE)
glance_model <- glance(model)

# regression table
print(tidy_model)
print(glance_model)


library(knitr)

kable(summary_table, caption = "Table 1: Summary of Continuous Variables")
kable(tidy_model, caption = "Table 2: Regression Results")

write.csv(summary_table, "summary_table.csv", row.names = FALSE)
write.csv(tidy_model, "regression_table_numeric_occ.html", row.names = FALSE)



install.packages("stargazer") 
library(stargazer)

# Run regression using ISCO08 as a continuous predictor
model_numeric_occ <- lm(stfgov ~ happy + fct_unemp + agea + fct_gender + isco08, data = data_clean)

# Export clean table
library(stargazer)
stargazer(model_numeric_occ, type = "html", out = "regression_table_numeric_occ.html",
          title = "Regression Results (Occupation as Continuous)",
          covariate.labels = c("Happiness", "Unemployed (Yes)", "Age", "Gender (Female)", "Occupation (ISCO08)"),
          dep.var.labels = "Trust in Government",
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = c("f", "ser"),
          digits = 3)
