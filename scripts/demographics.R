#################################################################################
##                                                                            ##
##                      Protective Factors Project                            ##
##                           Demographics                                     ##
##                                                                            ##
#################################################################################

################Hannah Morgan 19JAN2026#########################################
##                                                                            ##
################################################################################

#install.packages("dplyr")
#install.packages("psych")
#nstall.packages("knitr")


library(dplyr)
library(psych)
library(knitr)

#Read the RDS file and assign it to a variable - this should have variables saved properly
df_cov <- readRDS("data/processed/Protective_Factors_Data_Cleaned_04DEC2025.Rds")














#################################################################################
##                                                                            ##
##                         Demographics                                       ##
##                                                                            ##
#################################################################################
summary(df_cov$pex_bm_apa_apa2_depr_promisrawscore)
mean(df_cov$pex_bm_apa_apa2_depr_promisrawscore, na.rm = TRUE)
sd(df_cov$pex_bm_apa_apa2_depr_promisrawscore, na.rm = TRUE)

summary(df_cov$child_sex)

summary(df_cov$V2_T2_vol_adjusted_age)
mean(df_cov$V2_T2_vol_adjusted_age, na.rm = TRUE)
sd(df_cov$V2_T2_vol_adjusted_age, na.rm = TRUE)











#################################################################################
##                                                                            ##
##                         Demographics Table                                 ##
##                                                                            ##
#################################################################################

#Replace df with your dataset name
vars <- c("pex_bm_apa_apa2_depr_promisrawscore", "PACES", "V2_T2_vol_adjusted_age",
          "maternal_age_delivery", "mat_ed_5cat", "child_sex", "ICV_z")

#Separate continuous and categorical
cont_vars <- c("pex_bm_apa_apa2_depr_promisrawscore", "PACES", 
               "V2_T2_vol_adjusted_age", "maternal_age_delivery", "ICV_z")
cat_vars <- c("mat_ed_5cat", "child_sex")

#Continuous variable summary (M, SD, Range)
cont_summary <- df_cov %>%
  select(all_of(cont_vars)) %>%
  psych::describe() %>%
  select(mean, sd, min, max) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  tibble::rownames_to_column("Variable")

#Categorical variable summary (N, %)
cat_summary <- df_cov %>%
  select(all_of(cat_vars)) %>%
  lapply(function(x) {
    tbl <- prop.table(table(x)) * 100
    data.frame(
      Variable = names(tbl),
      N = as.numeric(table(x)),
      Percent = round(tbl, 1)
    )
  }) %>%
  bind_rows(.id = "Covariate")

#Optional: rename variables for readability
nice_names <- c(
  pex_bm_apa_apa2_depr_promisrawscore = "Prenatal Depression Score",
  PACES = "Protective Factors (PACES)",
  V2_T2_vol_adjusted_age = "Infant Age at Scan (weeks)",
  maternal_age_delivery = "Maternal Age at Delivery (years)",
  mat_ed_5cat = "Maternal Education (5 categories)",
  child_sex = "Child Sex",
  ICV_z = "Intracranial Volume (z-score)"
)

cont_summary$Variable <- nice_names[cont_summary$Variable]
cat_summary$Covariate <- nice_names[cat_summary$Covariate]

#Show results
kable(cont_summary, caption = "Table 1. Descriptive Statistics for Continuous Covariates (M, SD, Range)", 
      digits = 2, align = "lcccc")

kable(cat_summary, caption = "Table 2. Frequencies and Percentages for Categorical Covariates", 
      digits = 1, align = "lccc")
