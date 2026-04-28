#################################################################################
##                                                                            ##
##                      Protective Factors Project                            ##
##                       DEPRESSION LMEM                                      ##
##                                                                            ##
#################################################################################

################Hannah Morgan 19JAN2026#########################################
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


##################Age Breakdown Check

df_cov %>%
  count(V2_T2_vol_adjusted_age) %>%
  mutate(percent = n / sum(n) * 100)

df_cov %>%
  filter(!is.na(pex_bm_apa_apa2_depr_promisrawscore)) %>%
  count(V2_T2_vol_adjusted_age) %>%
  mutate(percent = n / sum(n) * 100)

##for below models using a sensitivity analysis
df_age <- df_cov %>%
  filter(V2_T2_vol_adjusted_age < 12)  ##change number here



###checking, and great, it matches all results. 
#df_cov_check <- df_cov %>%
#  filter(!is.na(pex_bm_apa_apa2_depr_promisrawscore))

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
  
  ##Covariates = mat_ed_5cat   PACES  pex_bm_apa_apa2_depr_promisrawscore
  ###Adding in the interaction, below

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










#####################Saving model output with all effects######################################
#Define new output folder nested under PACES
my_output_folder <- here("output", "Depression", "Prenatal_Model_Output")

#Directories: Full_Model_Output   "Reduced_Model_Output"

#Run across all ROIs to save them in the output folder
results <- map_dfr(roi_outcomes, fit_lmem, data = df_cov, outdir = my_output_folder)







###If you want to save just the significant effects
#Make sure output folder exists
my_output_folder <- here("output", "Depression", "Prenatal_Model_Output")
if (!dir.exists(my_output_folder)) dir.create(my_output_folder, recursive = TRUE)


#Save ALL results (including p_adj column)
write_csv(results, file = file.path(my_output_folder, "All_ROI_Results_with_p_adj.csv"))


#Save significant results UNCORRECTED
write_csv(sig_results, file = file.path(my_output_folder, "Significant_ROI_Results_Uncorrected.csv"))


#Save significant results AFTER FDR correction
write_csv(sig_results_adj, file = file.path(my_output_folder, "Significant_ROI_Results_FDR.csv"))










#################################################################################
##
##            Paper & Diagnostic PLOTS
##
#################################################################################

#Make ROI names nicer (optional)
results <- results %>%
  mutate(roi = gsub("V2_T2_vol_", "", outcome),
         roi = gsub("_", " ", roi))


##Forest plot with sig results
ggplot(results, aes(x = estimate, y = reorder(roi, estimate))) +
  geom_point(aes(color = p_adj < 0.05), size = 3) +
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
    x = "Estimate (β)",
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

ggsave(
  filename = "output/Depression/Depression_forest_plot_04MAR2026.png",   # file name (can be .png, .pdf, .jpeg, etc.)
  width = 8,                      # width in inches
  height = 6,                     # height in inches
  dpi = 300                        # resolution (good for publications)
)



#########Separated Forest Plot
####Breaking up brain areas into graphs
results <- results %>%
  mutate(
    roi_group = case_when(
      roi %in% c(
        "Left Cerebral White Matter",
        "Right Cerebral White Matter",
        "Right Cerebral Cortex",
        "Left Cerebral Cortex",
        "Right Cerebellum Cortex",
        "Left Cerebellum Cortex", 
        "Vermis"
      ) ~ "Cerebellum",
      TRUE ~ "Other ROIs"
    )
  )



p_cereb <- ggplot(
  results %>% filter(roi_group == "Cerebellum"),
  aes(x = estimate, y = reorder(roi, estimate))
) +
  geom_point(aes(color = p_adj < 0.05), size = 3, show.legend = FALSE) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Cerebellum & Cortical ROIs",
    x = "Estimate (β)",
    y = "Brain Region (ROI)"
  ) +
  scale_color_manual(
    values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"),
    labels = c("FALSE" = "ns", "TRUE" = "p_adj < .05"),
    name = "Significance"
  ) +
  theme_minimal(base_size = 16)

p_other <- ggplot(
  results %>% filter(roi_group == "Other ROIs"),
  aes(x = estimate, y = reorder(roi, estimate))
) +
  geom_point(aes(color = p_adj < 0.05), size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_continuous(limits = c(-4, 2)) +
  labs(title = "Subcortical ROIs",
       x = "Estimate (β)",
       y = "Brain Region (ROI)") +
  scale_color_manual(
    values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"),
    labels = c("FALSE" = "ns", "TRUE" = "p < .05"),
    name = "Significance"
  ) +
  theme_minimal(base_size = 16)


##Combine plots

p_other + p_cereb +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")



ggsave(
  filename = "output/Depression/All_ROIs_Depression_forest_plot_27APR2026.png",   # file name (can be .png, .pdf, .jpeg, etc.)
  width = 14,                      # width in inches
  height = 8,                     # height in inches
  dpi = 300                        # resolution (good for publications)
)






















###########################################################################################
ggplot(df_cov, aes(
  x = pex_bm_apa_apa2_depr_promisrawscore,    #pex_bm_apa_apa2_depr_promisrawscore; V2_T2_adjusted_age
  y = V2_T2_vol_Right_Amygdala
)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Depression (v02)",
    y = "Right Amygdala Volume",
    title = "Right Amygdala vs Depression"
  ) +
  theme_minimal()


ggsave(
  filename = "output/Depression/Depression_v02_Right_Amygdala_Scatter_plot_12MAR2026.png",   # file name (can be .png, .pdf, .jpeg, etc.)
  width = 8,                      # width in inches
  height = 6,                     # height in inches
  dpi = 300                        # resolution (good for publications)
)

#Checking linearity before continuing
ggplot(df_cov, aes(pex_bm_apa_apa2_depr_promisrawscore, V2_T2_vol_Right_Amygdala)) +
  geom_point(alpha = .2) +
  geom_smooth(method = "loess") +
  theme_minimal()



ggplot(df_cov, aes(
  x = pex_bm_apa_apa2_depr_promisrawscore,    #pex_bm_apa_apa2_depr_promisrawscore; V2_T2_adjusted_age
  y = V2_T2_vol_Right_Amygdala
)) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Depression (v02)",
    y = "Right Amygdala Volume",
    title = "Right Amygdala vs Depression"
  ) +
  theme_minimal()


ggsave(
  filename = "output/Depression/Depression_Slope_Right_Amygdala_plot_10APR2026.png",   # file name (can be .png, .pdf, .jpeg, etc.)
  width = 10,                      # width in inches
  height = 10,                     # height in inches
  dpi = 300                        # resolution (good for publications)
)












#################################################################################
##                                                                            ##
##          Linear Mixed Effects Models - Boys & Girls                        ##
##                                                                            ##
#################################################################################
df_sex <- df_cov %>%
  filter(child_sex == 1) #0 is female, 1 is male




#Replace with the actual ROI column names
#roi_outcomes <- c("V2_T2_vol_Left_Amygdala", "V2_T2_vol_Right_Amygdala", "V2_T2_vol_Left_Pallidum", "V2_T2_vol_Right_Pallidum", 
#                  "V2_T2_vol_Left_Accumbens_area", "V2_T2_vol_Right_Accumbens_area", "V2_T2_vol_Left_Putamen",
#                  "V2_T2_vol_Right_Putamen")
#roi_outcomes <- colnames(df_cov)[4:24] 

#roi_outcomes <- c("V2_T2_vol_Left_Amygdala", "V2_T2_vol_Right_Amygdala")
roi_outcomes <- colnames(df_cov)[4:24] 









#Function to fit model for one ROI
fit_lmem <- function(outcome, data, outdir = "output") {
  #Make sure output folder exists
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  #Build formula dynamically
  fml <- as.formula(
    paste0(outcome, " ~ pex_bm_apa_apa2_depr_promisrawscore + mat_ed_cat + V2_T2_vol_adjusted_age + maternal_age_delivery + ICV_z + (1|site)") 
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
    filter(term == "pex_bm_apa_apa2_depr_promisrawscore") %>%
    mutate(outcome = outcome)
}











#Run across all ROIs
results <- map_dfr(roi_outcomes, fit_lmem, data = df_sex) 





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
##                         Family History and Depression                      ##
##                                                                            ##
#################################################################################
df_cov <- readRDS("data/processed/Protective_Factors_Data_Family_Hx_Cleaned_2_16MAR2026.Rds")

summary(df_cov$pex_bm_psych_bm_005) ##Just to check the numbers
class(df_cov$pex_bm_psych_bm_005) ##Just to check that this is properly a factor

#pex_bm_psych_dep_001 - 006: need to make a factor that is yes or no if any of these is marked as a 1
df_cov <- df_cov %>%
  mutate(
    family_history_depression = if_else(
      if_any(
        pex_bm_psych_dep_001:pex_bm_psych_dep_006,
        ~ . == "1"
      ),
      "Yes",
      "No"
    )
  )%>%
      relocate(family_history_depression, .after = pex_bm_psych_dep_006)
#This should naturally be ignoring NAs, 777 and 999
##At least one "1" → “Yes” - EVEN if there is an NA, missing values don’t matter because you already have confirmation
##Only "0" + NA → ambiguous. You don’t actually know if family history is absent. You only know it wasn’t endorsed in observed responses

##This is making a continuous variable to see if it is a "dose effect"
###Note that if any column has an NA we will get an NA
df_cov <- df_cov %>%
  mutate(
    family_history_cnt = rowSums(
      across(
        pex_bm_psych_dep_001:pex_bm_psych_dep_006,
        ~ case_when(
          . %in% c("777", "999") ~ NA_real_,
          TRUE ~ as.numeric(.)
        )
      ),
      na.rm = FALSE   # IMPORTANT: keeps NA if any missing
    )
  )



table(df_cov$family_history_depression)
class(df_cov$family_history_depression) #another way to check

##################################################################################

df_sex <- df_cov %>%
  filter(child_sex == 1) #0 is female, 1 is male

##############################################################################


#Replace with the actual ROI column names
#roi_outcomes <- c("V2_T2_vol_Left_Amygdala", "V2_T2_vol_Right_Amygdala")
#roi_outcomes <- ("V2_T2_vol_Right_Amygdala")
roi_outcomes <- colnames(df_cov)[4:24] 











#Function to fit model for one ROI
fit_lmem <- function(outcome, data, outdir = "output") {
  #Make sure output folder exists
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  #Build formula dynamically
  fml <- as.formula(
    paste0(outcome, " ~ pex_bm_apa_apa2_depr_promisrawscore + family_history_depression + mat_ed_cat + child_sex + V2_T2_vol_adjusted_age + maternal_age_delivery + ICV_z + (1|site)") 
  )
  
  ##Covariates = mat_ed_5cat  pex_bm_apa_apa2_depr_promisrawscore
  ##Here the covariate to add will be pex_bm_psych_bm_005
  
  
  
  #Fit model
  m <- lmer(fml, data = data)
  
  #Save full summary to text file
  outfile <- file.path(outdir, paste0(outcome, "_model_summary.txt"))
  capture.output(summary(m), file = outfile)
  
  broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE, p.value = TRUE) %>%
    filter(term %in% c("pex_bm_apa_apa2_depr_promisrawscore")) %>%
    mutate(
      outcome = outcome,
      predictor = case_when(
        term == "pex_bm_apa_apa2_depr_promisrawscore" ~ "depression"
      )
    )
}











#Run across all ROIs
results <- map_dfr(roi_outcomes, fit_lmem, data = df_cov) ##Remember to change depending





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




######################follow up tests

model_fh <- lm(
  pex_bm_apa_apa2_depr_promisrawscore ~ pex_bm_psych_bm_005,
  data = df_cov
)

summary(model_fh)



t.test(
  pex_bm_apa_apa2_depr_promisrawscore ~ pex_bm_psych_bm_005, na.rm = TRUE,
  data = df_cov
)

df_cov %>%
  group_by(pex_bm_psych_bm_005) %>%
  summarise(
    mean_dep = mean(pex_bm_apa_apa2_depr_promisrawscore, na.rm = TRUE),
    sd_dep = sd(pex_bm_apa_apa2_depr_promisrawscore, na.rm = TRUE),
    n = n()
  )




