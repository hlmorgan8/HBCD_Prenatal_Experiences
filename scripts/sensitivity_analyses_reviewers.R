#################################################################################
##                                                                            ##
##                      Protective Factors Project                            ##
##                       Reviewer Follow-Ups                                  ##
##                                                                            ##
#################################################################################

################Hannah Morgan 29JUN2026#########################################
##                                                                            ##
################################################################################





library(tidyr)
library(purrr)
library(broom.mixed)
library(ggpubr)
library(here)
library(readr)
library(stringr)
library(dplyr)
library(interactions)
library(emmeans)
library(patchwork)
library(lme4)
library(lmerTest)
library(psych)



#Read in csv if preferred, but warning that it will not have variables saved properly!!!!
#df_cov <- read.csv("data/processed/Protective_Factors_Data_Cleaned_27OCT2025.csv")


#Read the RDS file and assign it to a variable - this should have variables saved properly
##Protective_Factors_Data_Cleaned_2_17FEB2026.Rds - sites removed, without PROMIS
##Protective_Factors_Data_Cleaned_2_20FEB2026.Rds - without PROMIS
###Protective_Factors_Data_PROMIS_Cleaned_2_20FEB2026.Rds - sites removed, with PROMIS, outliers removed PACEs


##Critical Dfs
#df_cov <- readRDS("data/processed/Protective_Factors_Data_APA_Ses2_Cleaned_2_12MAR2026.Rds") #Postanatal APA2
df_cov <- readRDS("data/processed/Protective_Factors_Data_APA_PROMIS_Cleaned_2_11MAR2026.Rds") #Prenatal APA2

#################################################################################
##                                                                            ##
##                     Dummy Coding Check                                     ##
##                                                                            ##
#################################################################################
str(df_cov[, c("site", "mat_ed_cat", "child_sex")])

levels(df_cov$mat_ed_cat)
levels(df_cov$site)
levels(df_cov$child_sex) #1 is Male, 0 is female


summary(df_cov$pex_bm_apa_gestational_age)
hist(df_cov$pex_bm_apa_gestational_age)




###################################################################################
#This is for the ROI outcomes
outcomes <- c(4:24)
#outcomes <-  c("V2_T2_vol_Left_Amygdala", "V2_T2_vol_Right_Amygdala")


#Reshape data long
df_long <- df_cov %>%
  dplyr::select(all_of(outcomes)) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "variable", values_to = "value")


#Faceted histograms
ggplot(df_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Selected Outcomes")



ggsave(
  filename = "output/Depression/Amygdala_Histogram_plot_04MAR2026.png",   # file name (can be .png, .pdf, .jpeg, etc.)
  width = 8,                      # width in inches
  height = 6,                     # height in inches
  dpi = 300                        # resolution (good for publications)
)








#################################################################################
##                                                                            ##
##                   Skew & Kurtosis of Depression Concern                    ##
##                                                                            ##
#################################################################################









skew(df_cov$pex_bm_apa_apa2_depr_promisrawscore)
kurtosi(df_cov$pex_bm_apa_apa2_depr_promisrawscore)

ggplot(df_cov, aes(x = pex_bm_apa_apa2_depr_promisrawscore)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "Density of Maternal Depressive Symptoms")


shapiro.test(df_cov$pex_bm_apa_apa2_depr_promisrawscore)

qqnorm(df_cov$pex_bm_apa_apa2_depr_promisrawscore)
qqline(df_cov$pex_bm_apa_apa2_depr_promisrawscore, col = "red")


#make a log transformed variable 
df_cov$dep_log <- log(df_cov$pex_bm_apa_apa2_depr_promisrawscore + 1)


##checking the new variable
skew(df_cov$dep_log)
kurtosi(df_cov$dep_log)



shapiro.test(df_cov$dep_log)

###Histograpm with forcing y axis to match non-log transformed score
hist(df_cov$dep_log)

hist(df_cov$dep_log,
     breaks = 20,
     xlab = "PROMIS Raw Score",
     main = "Distribution of LOG Transformed PROMIS Depression Scores",
     ylim = c(0, 1000))

##Here is the other histogram again to compare
hist(df_cov$pex_bm_apa_apa2_depr_promisrawscore)







#################################################################################
##                                                                            ##
##          Linear Mixed Effects Models - Main Analyses                       ##
##                                                                            ##
#################################################################################

#Replace with the actual ROI column names
#roi_outcomes <- c("V2_T2_vol_Left_Amygdala", "V2_T2_vol_Right_Amygdala")
roi_outcomes <- colnames(df_cov)[4:24] 











#Function to fit model for one ROI
fit_lmem <- function(outcome, data, outdir = "output") {
  #Make sure output folder exists
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  #Build formula dynamically
  fml <- as.formula(
    paste0(outcome, " ~ pex_bm_apa_apa2_depr_promisrawscore + child_sex + mat_ed_cat + V2_T2_vol_adjusted_age + maternal_age_delivery + ICV_z + (1|site)") 
  )
  
  ##Covariates = mat_ed_5cat   PACES  pex_bm_apa_apa2_depr_promisrawscore maternal_age_delivery
  ##APA gestational age = pex_bm_apa_gestational_age
  ###Gestational age = sed_basic_demographics_gestational_age_delivery
  ###Other infant age = V2_T2_vol_candidate_age
  ###log transformed depression = dep_log
  
  #Fit model
  m <- lmerTest::lmer(fml, data = data)
  
  #Save full summary to text file
  outfile <- file.path(outdir, paste0(outcome, "_model_summary.txt"))
  capture.output(summary(m), file = outfile)
  
  #Still return tidy Depression effect for summary table
  broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE, p.value = TRUE) %>%
    filter(term == "dep_log") %>%  #if you want to look at interaction: pex_bm_apa_apa2_depr_promisrawscore:child_sex0
    mutate(outcome = outcome)
}









########################################################################################

#Run across all ROIs
results <- map_dfr(roi_outcomes, fit_lmem, data = df_cov)  #df_age here too for sensitivity analyses





#Flag significant Depression effects - UNCORRECTED
sig_results <- results %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

#Seeing output
results
sig_results




#Add multiple correction adjustment (BH)
results <- results %>%
  mutate(p_adj = p.adjust(p.value, method = "BH"))

#Flag significant results after FDR
sig_results_adj <- results %>%
  filter(p_adj < 0.05) %>%
  arrange(p_adj)

#Seeing output
results
sig_results_adj

















#################################################################################
##                                                                            ##
##          Linear Mixed Effects Models - PARITY                              ##
##                                                                            ##
#################################################################################



###Clear before running

df_cov <- readRDS("data/processed/Protective_Factors_Data_PARITY_2_01JUL2026.Rds") #Prenatal APA2



#################################################################################
##                                                                            ##
##                     Dummy Coding Check                                     ##
##                                                                            ##
#################################################################################
str(df_cov[, c("site", "mat_ed_cat", "child_sex")])

levels(df_cov$mat_ed_cat)
levels(df_cov$site)
levels(df_cov$child_sex) #1 is Male, 0 is female


##Changing pre enrollment parity to numeric 
df_cov$pex_bm_health_preg__healthhx__preghx_001 <-
  as.numeric(df_cov$pex_bm_health_preg__healthhx__preghx_001)

class(df_cov$pex_bm_health_preg__healthhx__preghx_001)


##For full transparency for demographics (this removes NAs of depression, anyone without MRI V02 data is already removed)
###LME handles complete cases already
df_cov_depr_removed <- df_cov %>%
  filter(!is.na(pex_bm_apa_apa2_depr_promisrawscore))
summary(df_cov_depr_removed$child_sex)

summary(df_cov_depr_removed$pex_bm_health_preg__healthhx__preghx_001)
mean(df_cov_depr_removed$pex_bm_health_preg__healthhx__preghx_001, na.rm = TRUE)
sd(df_cov_depr_removed$pex_bm_health_preg__healthhx__preghx_001, na.rm = TRUE)






#################################################################################
##                                                                            ##
##          Linear Mixed Effects Models - Main Model - Gravidity              ##
##                                                                            ##
#################################################################################

#Replace with the actual ROI column names
#roi_outcomes <- c("V2_T2_vol_Left_Amygdala", "V2_T2_vol_Right_Amygdala")
roi_outcomes <- colnames(df_cov)[4:24] 











#Function to fit model for one ROI
fit_lmem <- function(outcome, data, outdir = "output") {
  #Make sure output folder exists
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  #Build formula dynamically
  fml <- as.formula(
    paste0(outcome, " ~ pex_bm_apa_apa2_depr_promisrawscore + child_sex + mat_ed_cat + V2_T2_vol_adjusted_age + pex_bm_health_preg__healthhx__preghx_001 + maternal_age_delivery + ICV_z + (1|site)") 
  )
  
  ##Covariates = mat_ed_5cat   PACES  pex_bm_apa_apa2_depr_promisrawscore maternal_age_delivery
  ##APA gestational age = pex_bm_apa_gestational_age
  ###Gestational age = sed_basic_demographics_gestational_age_delivery
  ###Other infant age = V2_T2_vol_candidate_age
  ###log transformed depression = dep_log
  
  #Fit model
  m <- lmerTest::lmer(fml, data = data)
  
  #Save full summary to text file
  outfile <- file.path(outdir, paste0(outcome, "_model_summary.txt"))
  capture.output(summary(m), file = outfile)
  
  #Still return tidy Depression effect for summary table
  broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE, p.value = TRUE) %>%
    filter(term == "pex_bm_apa_apa2_depr_promisrawscore") %>%  #if you want to look at interaction: pex_bm_apa_apa2_depr_promisrawscore:child_sex0
    mutate(outcome = outcome)
}









########################################################################################

#Run across all ROIs
results <- map_dfr(roi_outcomes, fit_lmem, data = df_cov)  





#Flag significant Depression effects - UNCORRECTED
sig_results <- results %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

#Seeing output
results
sig_results




#Add multiple correction adjustment (BH)
results <- results %>%
  mutate(p_adj = p.adjust(p.value, method = "BH"))

#Flag significant results after FDR
sig_results_adj <- results %>%
  filter(p_adj < 0.05) %>%
  arrange(p_adj)

#Seeing output
results
sig_results_adj










































#################################################################################
##                                                                            ##
##          Linear Mixed Effects Models - WEIGHT                              ##
##                                                                            ##
#################################################################################



###Clear before running

df_cov <- readRDS("data/processed/Protective_Factors_Data_InfantHealth_2_02JUL2026.Rds") #Prenatal APA2



#################################################################################
##                                                                            ##
##                     Dummy Coding Check                                     ##
##                                                                            ##
#################################################################################
str(df_cov[, c("site", "mat_ed_cat", "child_sex")])

levels(df_cov$mat_ed_cat)
levels(df_cov$site)
levels(df_cov$child_sex) #1 is Male, 0 is female


##Changing pre enrollment parity to numeric 
df_cov$pex_bm_health_preg__healthhx__preghx_001 <-
  as.numeric(df_cov$pex_bm_health_preg__healthhx__preghx_001)

class(df_cov$pex_bm_health_preg__healthhx__preghx_001)

###birthweight - ounces
df_cov$pex_bm_healthv2_inf_001__01 <-
  as.numeric(df_cov$pex_bm_healthv2_inf_001__01)

class(df_cov$pex_bm_healthv2_inf_001__01)

##birthweight pounds
df_cov$pex_bm_healthv2_inf_001__02 <-
  as.numeric(df_cov$pex_bm_healthv2_inf_001__02)

class(df_cov$pex_bm_healthv2_inf_001__02)



##########################Making new weight variable 

###making new baby weight - first change pounds (_02) column to ounces and then combine with ounces column (_01)
df_cov$birth_weight_oz_total <- (df_cov$pex_bm_healthv2_inf_001__02 * 16) + df_cov$pex_bm_healthv2_inf_001__01

###making grams - new variable 
df_cov$birth_weight_grams_total <- df_cov$birth_weight_oz_total * 28.3495



##For full transparency for demographics (this removes NAs of depression, anyone without MRI V02 data is already removed)
###LME handles complete cases already
df_cov_depr_removed <- df_cov %>%
  filter(!is.na(pex_bm_apa_apa2_depr_promisrawscore))
summary(df_cov_depr_removed$child_sex)

#ounces
summary(df_cov_depr_removed$pex_bm_healthv2_inf_001__01)
mean(df_cov_depr_removed$pex_bm_healthv2_inf_001__01, na.rm = TRUE)
sd(df_cov_depr_removed$pex_bm_healthv2_inf_001__01, na.rm = TRUE)

#pounds
summary(df_cov_depr_removed$pex_bm_healthv2_inf_001__02)
mean(df_cov_depr_removed$pex_bm_healthv2_inf_001__02, na.rm = TRUE)
sd(df_cov_depr_removed$pex_bm_healthv2_inf_001__02, na.rm = TRUE)

#total ounces
summary(df_cov_depr_removed$birth_weight_oz_total)
mean(df_cov_depr_removed$birth_weight_oz_total, na.rm = TRUE)
sd(df_cov_depr_removed$birth_weight_oz_total, na.rm = TRUE)



#gravidity pre enrollment 
summary(df_cov_depr_removed$pex_bm_health_preg__healthhx__preghx_001)
mean(df_cov_depr_removed$pex_bm_health_preg__healthhx__preghx_001, na.rm = TRUE)
sd(df_cov_depr_removed$pex_bm_health_preg__healthhx__preghx_001, na.rm = TRUE)




#################################################################################
##                                                                            ##
##          Linear Mixed Effects Models - Main Model - BirthWeight            ##
##                                                                            ##
#################################################################################

#Replace with the actual ROI column names
#roi_outcomes <- c("V2_T2_vol_Left_Amygdala", "V2_T2_vol_Right_Amygdala")
roi_outcomes <- colnames(df_cov)[4:24] 











#Function to fit model for one ROI
fit_lmem <- function(outcome, data, outdir = "output") {
  #Make sure output folder exists
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  #Build formula dynamically
  fml <- as.formula(
    paste0(outcome, " ~ pex_bm_apa_apa2_depr_promisrawscore + child_sex + mat_ed_cat + V2_T2_vol_adjusted_age + birth_weight_oz_total + maternal_age_delivery + ICV_z + (1|site)") 
  )
  
  ##Covariates = mat_ed_5cat   PACES  pex_bm_apa_apa2_depr_promisrawscore maternal_age_delivery
  ##APA gestational age = pex_bm_apa_gestational_age
  ###Gestational age = sed_basic_demographics_gestational_age_delivery
  ###Other infant age = V2_T2_vol_candidate_age
  ###log transformed depression = dep_log
  #Weight = birth_weight_oz_total pex_bm_healthv2_inf_001__02
  ##Gravidity = pex_bm_health_preg__healthhx__preghx_001
  
  #Fit model
  m <- lmerTest::lmer(fml, data = data)
  
  #Save full summary to text file
  outfile <- file.path(outdir, paste0(outcome, "_model_summary.txt"))
  capture.output(summary(m), file = outfile)
  
  #Still return tidy Depression effect for summary table
  broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE, p.value = TRUE) %>%
    filter(term == "pex_bm_apa_apa2_depr_promisrawscore") %>%  #if you want to look at interaction: pex_bm_apa_apa2_depr_promisrawscore:child_sex0
    mutate(outcome = outcome)
}









########################################################################################

#Run across all ROIs
results <- map_dfr(roi_outcomes, fit_lmem, data = df_cov)  





#Flag significant Depression effects - UNCORRECTED
sig_results <- results %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

#Seeing output
results
sig_results




#Add multiple correction adjustment (BH)
results <- results %>%
  mutate(p_adj = p.adjust(p.value, method = "BH"))

#Flag significant results after FDR
sig_results_adj <- results %>%
  filter(p_adj < 0.05) %>%
  arrange(p_adj)

#Seeing output
results
sig_results_adj







