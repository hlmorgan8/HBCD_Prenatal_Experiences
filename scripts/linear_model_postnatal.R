#################################################################################
##                                                                            ##
##                      Protective Factors Project                            ##
##                       Postnatal Depression LMEM                            ##
##                                                                            ##
#################################################################################

################Hannah Morgan 08APR2026#########################################
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



df_cov <- readRDS("data/processed/Protective_Factors_Data_APA_PrePostNatal_Cleaned_2_12MAR2026.Rds")







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
    paste0(outcome, " ~ pex_bm_apa_apa2_depr_promisrawscore_prenatal + pex_bm_apa_apa2_depr_promisrawscore_postnatal + child_sex + mat_ed_cat + V2_T2_vol_adjusted_age + maternal_age_delivery + ICV_z + (1|site)") 
  )
  
  ##Covariates = mat_ed_5cat   PACES  pex_bm_apa_apa2_depr_promisrawscore
  ###Adding in the interaction, below
  
  #Fit model
  m <- lmerTest::lmer(fml, data = data)
  
  #Save full summary to text file
  outfile <- file.path(outdir, paste0(outcome, "_model_summary.txt"))
  capture.output(summary(m), file = outfile)
  
  #Still return tidy Depression effect for summary table
  broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE, p.value = TRUE) %>%
    filter(term == "pex_bm_apa_apa2_depr_promisrawscore_prenatal") %>%  #if you want to look at interaction: pex_bm_apa_apa2_depr_promisrawscore:child_sex0
    mutate(outcome = outcome)
}











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

