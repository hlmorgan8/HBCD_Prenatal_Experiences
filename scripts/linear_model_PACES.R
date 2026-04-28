#################################################################################
##                                                                            ##
##                      Protective Factors Project                            ##
##                            PACES LMEM                                      ##
##                                                                            ##
#################################################################################

################Hannah Morgan 19JAN2026#########################################
##                                                                            ##
################################################################################

#install.packages("tidyr")
#install.packages("purrr")
#install.packages("lme4")
#install.packages("broom.mixed")
#install.packages("lmerTest")
#install.packages("ggpubr")
#install.packages("here")
#install.packages("papaja")




library(dplyr)
library(tidyr)
library(purrr)
library(lme4)
library(broom.mixed)
library(lmerTest)
library(ggpubr)
library(here)
library(readr)
library(stringr)
library(papaja)
library(patchwork)




#Read in csv if preferred, but warning that it will not have variables saved properly!!!!
#df_cov <- read.csv("data/processed/Protective_Factors_Data_Cleaned_24OCT2025.csv")


#Read the RDS file and assign it to a variable - this should have variables saved properly
df_cov <- readRDS("data/processed/Protective_Factors_Data_Cleaned_2_17FEB2026.Rds")
#df_cov <- readRDS("data/processed/Protective_Factors_Data_Cleaned_08DEC2025.Rds") ##This is 1.1 data



#################################################################################
##                                                                            ##
##                     Check ROI Distribution                                 ##
##                                                                            ##
#################################################################################

#This is for the ROI outcomes
outcomes <- c(4:24)



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





#################################################################################
##                                                                            ##
##             Dummy Coding/Variable Type Check                               ##
##                                                                            ##
#################################################################################
str(df_cov[, c("site", "mat_ed_cat", "child_sex")])

levels(df_cov$mat_ed_cat)
levels(df_cov$site)
levels(df_cov$child_sex) #1 is Male, 0 is female
#mat_ed_5cat in 1.1










#################################################################################
##                                                                            ##
##                     Linear Mixed Effects Models                            ##
##                                                                            ##
#################################################################################

#Replace with the actual ROI column names
##[4,26] in 1.1
##[4, 24] in 2.0
#roi_outcomes <- colnames(df_cov)[4:24] 


##Function to fit model for one ROI
fit_lmem <- function(outcome, data, outdir = "output") {
  #Make sure output folder exists
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  #Build formula dynamically
  fml <- as.formula(
    paste0(outcome, "~ PACES + V2_T2_vol_adjusted_age + maternal_age_delivery + child_sex + ICV_z + (1|site)") 
  )
  
  ##Covariates = mat_ed_5cat, maternal_age_delivery 
  
  #Fit model
  m <- lmer(fml, data = data)
  
  #Save full summary to text file
  outfile <- file.path(outdir, paste0(outcome, "_model_summary.txt"))
  capture.output(summary(m), file = outfile)
  
  #Still return tidy PACES effect for summary table
  broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE, p.value = TRUE) %>%
    filter(term == "PACES") %>%
    mutate(outcome = outcome)
}













#Run across all ROIs
results <- map_dfr(roi_outcomes, fit_lmem, data = df_cov)



######################Flag significant PACES effects - UNCORRECTED####################
sig_results <- results %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

#Seeing output
results
sig_results



###################Add multiple correction adjustment (BH)############################
results <- results %>%
  mutate(p_adj = p.adjust(p.value, method = "BH"))

#Flag significant results after FDR
sig_results_adj <- results %>%
  filter(p_adj < 0.05) %>%
  arrange(p_adj)

#Seeing output
results
sig_results_adj



