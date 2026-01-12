#################################################################################
##                                                                            ##
##                      Protective Factors Project                            ##
##                       DEPRESSION LMEM                                      ##
##                                                                            ##
#################################################################################

################Hannah Morgan 27OCT2025#########################################
##                                                                            ##
################################################################################

#install.packages("tidyr")
#install.packages("purrr")
#install.packages("lme4")
#install.packages("broom.mixed")
#install.packages("lmerTest")
#install.packages("ggpubr")
#install.packages("here")




library(tidyr)
library(purrr)
library(lme4)
library(broom.mixed)
library(lmerTest)
library(ggpubr)
library(here)
library(readr)
library(stringr)
library(dplyr)




#Read in csv if preferred, but warning that it will not have variables saved properly!!!!
#df_cov <- read.csv("data/processed/Protective_Factors_Data_Cleaned_27OCT2025.csv")


#Read the RDS file and assign it to a variable - this should have variables saved properly
df_cov <- readRDS("data/processed/Protective_Factors_Data_Cleaned_08DEC2025.Rds")




#################################################################################
##                                                                            ##
##                     Dummy Coding Check                                     ##
##                                                                            ##
#################################################################################
str(df_cov[, c("site", "mat_ed_5cat", "child_sex")])

levels(df_cov$mat_ed_5cat)
levels(df_cov$site)
levels(df_cov$child_sex) #1 is Male, 0 is female






















#################################################################################
##                                                                            ##
##                     Linear Mixed Effects Models                            ##
##                                                                            ##
#################################################################################

#Replace with the actual ROI column names
roi_outcomes <- c("V2_T2_vol_Left_Amygdala", "V2_T2_vol_Right_Amygdala")







#
#Function to fit model for one ROI
#---------------------------------------
fit_lmem <- function(outcome, data, outdir = "output") {
  # make sure output folder exists
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  # build formula dynamically
  fml <- as.formula(
    paste0(outcome, " ~ pex_bm_apa_apa2_depr_promisrawscore + V2_T2_vol_adjusted_age + maternal_age_delivery + child_sex + ICV_z + (1|site)") 
  )
  
  ##covariates = mat_ed_5cat   PACES

  # fit model
  m <- lmer(fml, data = data)
  
  # save full summary to text file
  outfile <- file.path(outdir, paste0(outcome, "_model_summary.txt"))
  capture.output(summary(m), file = outfile)
  
  # still return tidy Depression effect for summary table
  broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE, p.value = TRUE) %>%
    filter(term == "pex_bm_apa_apa2_depr_promisrawscore") %>%
    mutate(outcome = outcome)
}










#---------------------------------------
# 3. Run across all ROIs
#---------------------------------------
results <- map_dfr(roi_outcomes, fit_lmem, data = df_cov)




#---------------------------------------
# 4. Flag significant Depression effects - UNCORRECTED
#---------------------------------------
sig_results <- results %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

#Inspect
results
sig_results



#---------------------------------------
# 5. Add multiple correction adjustment (Benjamini-Hochberg)
#---------------------------------------
results <- results %>%
  mutate(p_adj = p.adjust(p.value, method = "BH"))

# Flag significant results after FDR
sig_results_adj <- results %>%
  filter(p_adj < 0.05) %>%
  arrange(p_adj)

# Inspect
results
sig_results_adj




#####################Saving model output with all effects######################################
# Define new output folder nested under PACES
my_output_folder <- here("output", "Depression", "Reduced_Model_Output")

#Directories: Full_Model_Output   "Reduced_Model_Output"

# Run across all ROIs to save them in the output folder
results <- map_dfr(roi_outcomes, fit_lmem, data = df_cov, outdir = my_output_folder)







###If you want to save just the significant effects
# Make sure output folder exists
my_output_folder <- here("output", "Depression", "Reduced_Model_Output")
if (!dir.exists(my_output_folder)) dir.create(my_output_folder, recursive = TRUE)

#---------------------------------------
# 1. Save ALL results (including p_adj column)
#---------------------------------------
write_csv(results, file = file.path(my_output_folder, "All_ROI_Results_with_p_adj.csv"))

#---------------------------------------
# 2. Save significant results UNCORRECTED
#---------------------------------------
write_csv(sig_results, file = file.path(my_output_folder, "Significant_ROI_Results_Uncorrected.csv"))

#---------------------------------------
# 3. Save significant results AFTER FDR correction
#---------------------------------------
write_csv(sig_results_adj, file = file.path(my_output_folder, "Significant_ROI_Results_FDR.csv"))










#################################################################################
##
##            PLOTS
##
#################################################################################

# Fit models for all ROIs and combine
results <- purrr::map_dfr(roi_outcomes, ~fit_lmem(.x, df_cov))

# Make ROI names nicer (optional)
results <- results %>%
  mutate(roi = gsub("V2_T2_vol_", "", outcome),
         roi = gsub("_", " ", roi))


##Forest plot with sig results
ggplot(results, aes(x = estimate, y = reorder(roi, estimate))) +
  geom_point(aes(color = p.value < 0.05), size = 3) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"), 
                     labels = c("FALSE" = "ns", "TRUE" = "p < .05"),
                     name = "Significance") +
  labs(
    x = "Estimate (B)",
    y = "Brain Region (ROI)",
    title = "Associations Between Prenatal Depression and ROI Volumes"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 15),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_blank()
  )



###Fix this to add significance
##Grouped bar plot
ggplot(results, aes(y = estimate, x = reorder(roi, estimate))) +
  geom_col(fill = "#3182bd") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(
    y = "B (Depression → ROI Volume)",
    x = "Brain Region",
    title = "Regions Associated with Prenatal Depression Scores"
  ) +
  theme_classic(base_size = 14)




