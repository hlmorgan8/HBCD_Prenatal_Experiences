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

df_cov <- readRDS("data/processed/Protective_Factors_Data_APA_PROMIS_Cleaned_2_11MAR2026.Rds")
#df_cov <- readRDS("data/processed/Protective_Factors_Data_APA_PrePostNatal_Cleaned_2_12MAR2026.Rds")
#df_cov <- readRDS("data/processed/Protective_Factors_Data_APA_Ses2_Cleaned_2_12MAR2026.Rds")


#df_cov <- readRDS("data/processed/Protective_Factors_Data_PROMIS_Cleaned_2_24FEB2026.Rds")
##Protective_Factors_Data_Cleaned_2_17FEB2026.Rds - sites removed, without PROMIS
###Protective_Factors_Data_PROMIS_Cleaned_2_20FEB2026.Rds - sites removed, with PROMIS, outliers removed PACEs










##For full transparency (this removes NAs of depression)
df_cov <- df_cov %>%
  filter(!is.na(pex_bm_apa_apa2_depr_promisrawscore))


#################################################################################
##                                                                            ##
##                         Demographics                                       ##
##                                                                            ##
#################################################################################
summary(df_cov$child_sex)

summary(df_cov$V2_T2_vol_adjusted_age)
mean(df_cov$V2_T2_vol_adjusted_age, na.rm = TRUE)
sd(df_cov$V2_T2_vol_adjusted_age, na.rm = TRUE)


summary(df_cov$pex_bm_apa_apa2_depr_promisrawscore)
mean(df_cov$pex_bm_apa_apa2_depr_promisrawscore, na.rm = TRUE)
sd(df_cov$pex_bm_apa_apa2_depr_promisrawscore, na.rm = TRUE)

summary(df_cov$sed_bm_strsup_total_score)
mean(df_cov$sed_bm_strsup_total_score, na.rm = TRUE)
sd(df_cov$sed_bm_strsup_total_score, na.rm = TRUE)


summary(df_cov$sed_bm_strsup_total_raw_score)
mean(df_cov$sed_bm_strsup_total_raw_score, na.rm = TRUE)
sd(df_cov$sed_bm_strsup_total_raw_score, na.rm = TRUE)



#################################################################################
##                                                                            ##
##                         Demographics Table                                 ##
##                                                                            ##
#################################################################################

#Replace df with your dataset name
vars <- c("pex_bm_apa_apa2_depr_promisrawscore", "V2_T2_vol_adjusted_age",
          "maternal_age_delivery", "mat_ed_cat", "child_sex", "ICV_z")

#Separate continuous and categorical
cont_vars <- c("pex_bm_apa_apa2_depr_promisrawscore",
               "V2_T2_vol_adjusted_age", "maternal_age_delivery", "ICV_z")
cat_vars <- c("mat_ed_cat", "child_sex")

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
  V2_T2_vol_adjusted_age = "Infant Age at Scan (weeks)",
  maternal_age_delivery = "Maternal Age at Delivery (years)",
  mat_ed_cat = "Maternal Education",
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





####################################################################################
##
##                            Diagnostics
##
##################################################################################

# Histograms + density + saving with ggsave
vars <- c(
  "pex_bm_apa_apa2_depr_promisrawscore",
  "sed_bm_strsup_total_score",
  "sed_bm_strsup_total_raw_score"
)

for (v in vars) {
  
  p <- ggplot(df_cov, aes(.data[[v]])) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 30,
                   fill = "steelblue",
                   alpha = .6) +
    geom_density(color = "red", linewidth = 1) +
    ggtitle(v) +
    theme_minimal()
  
  print(p)
  
  ggsave(
    filename = paste0("output/Depression/hist_density_", v, ".png"),
    plot = p,
    width = 6,
    height = 4,
    dpi = 300
  )
}

####################################################################

vars <- df_cov %>%
  select(pex_bm_apa_apa2_depr_promisrawscore, PACES, sed_bm_strsup_total_score, sed_bm_strsup_total_raw_score)

cor(vars, use = "pairwise.complete.obs")


####################################################################
##Pre and postnatal
vars <- df_cov %>%
  select(pex_bm_apa_apa2_depr_promisrawscore_prenatal, pex_bm_apa_apa2_depr_promisrawscore_postnatal)

cor(vars, use = "pairwise.complete.obs")

cor.test(
  df_cov$pex_bm_apa_apa2_depr_promisrawscore_prenatal,
  df_cov$pex_bm_apa_apa2_depr_promisrawscore_postnatal,
  use = "pairwise.complete.obs"
)



######Scatterplot
ggplot(df_cov, aes(
  x = pex_bm_apa_apa2_depr_promisrawscore_prenatal,    #pex_bm_apa_apa2_depr_promisrawscore; V2_T2_adjusted_age
  y = V2_T2_vol_Right_Amygdala
)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Depression (v01)",
    y = "Right Amygdala Volume",
    title = "Right Amygdala vs. Depression"
  ) +
  theme_minimal()


ggsave(
  filename = "output/Depression/Depression_Amygdala_Scatter_plot_12MAR2026.png",   # file name (can be .png, .pdf, .jpeg, etc.)
  width = 8,                      # width in inches
  height = 6,                     # height in inches
  dpi = 300                        # resolution (good for publications)
)



