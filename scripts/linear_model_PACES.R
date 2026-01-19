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
df_cov <- readRDS("data/processed/Protective_Factors_Data_Cleaned_08DEC2025.Rds")



#################################################################################
##                                                                            ##
##                     Check ROI Distribution                                 ##
##                                                                            ##
#################################################################################

#This is for the ROI outcomes
outcomes <- c(4:26)



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
roi_outcomes <- colnames(df_cov)[4:26] 


##Function to fit model for one ROI
fit_lmem <- function(outcome, data, outdir = "output") {
  #Make sure output folder exists
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  #Build formula dynamically
  fml <- as.formula(
    paste0(outcome, "~ PACES + V2_T2_vol_adjusted_age + maternal_age_delivery + child_sex + ICV_z + (1|site)") 
  )
  
  ##Covariates = mat_ed_5cat 
  
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




#####################Saving model output with all effects######################################
#Define your output folder
my_output_folder <- here("output", "PACES", "Reduced_Model_Output")  #Change name here depending on how you run your model

#Directories: Full_Model_Output

#Run across all ROIs to save them in the output folder
results <- map_dfr(roi_outcomes, fit_lmem, data = df_cov, outdir = my_output_folder)






###If you want to save just the significant effects
#Make sure output folder exists
my_output_folder <- here("output", "PACES", "Reduced_Model_Output")
if (!dir.exists(my_output_folder)) dir.create(my_output_folder, recursive = TRUE)


#Save ALL results (including p_adj column)
write_csv(results, file = file.path(my_output_folder, "All_ROI_Results_with_p_adj.csv"))


#Save significant results UNCORRECTED
write_csv(sig_results, file = file.path(my_output_folder, "Significant_ROI_Results_Uncorrected.csv"))


#Save significant results AFTER FDR correction
write_csv(sig_results_adj, file = file.path(my_output_folder, "Significant_ROI_Results_FDR.csv"))






#################################################################################
##
##            PLOTS
##
#################################################################################

#Fit models for all ROIs and combine
results <- purrr::map_dfr(roi_outcomes, ~fit_lmem(.x, df_cov))

#Make ROI names nicer (optional)
results <- results %>%
  mutate(roi = gsub("V2_T2_vol_", "", outcome),
         roi = gsub("_", " ", roi))


##Forest plot with UNCORRECTED sig results
ggplot(results, aes(x = estimate, y = reorder(roi, estimate))) +
  geom_point(aes(color = p.value < 0.05), size = 3) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("TRUE" = "#de2d26", "FALSE" = "#2b8cbe"),
                     labels = c("FALSE" = "ns", "TRUE" = "p < .05"),
                     name = "Significance") +
  labs(
    x = "Effect of Prenatal Protective Factors Score (β ± 95% CI)",
    y = "Brain Region (ROI)",
    title = "Associations Between Prenatal Protection Scores and ROI Volumes"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 15),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_blank()
  )


##Run again
#Add multiple correction adjustment (Benjamini-Hochberg)

results <- results %>%
  mutate(p_adj = p.adjust(p.value, method = "BH"))

#Flag significant results after FDR
sig_results_adj <- results %>%
  filter(p_adj < 0.05) %>%
  arrange(p_adj)

#Seeing output
results
sig_results_adj




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
  geom_point(aes(color = p_adj < 0.05), size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Cerebellum & Cortical ROIs",
       x = "Estimate (B)",
       y = "Brain Region (ROI)") +
  scale_color_manual(
    values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"),
    labels = c("FALSE" = "ns", "TRUE" = "p_adj < .05"),
    name = "Significance"
  ) +
  theme_minimal(base_size = 14)

p_other <- ggplot(
  results %>% filter(roi_group == "Other ROIs"),
  aes(x = estimate, y = reorder(roi, estimate))
) +
  geom_point(aes(color = p_adj < 0.05), size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_continuous(limits = c(-10, 10)) +
  labs(title = "Subcortical ROIs",
       x = "Estimate (B)",
       y = "Brain Region (ROI)") +
  scale_color_manual(
    values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"),
    labels = c("FALSE" = "ns", "TRUE" = "p_adj < .05"),
    name = "Significance"
  ) +
  theme_minimal(base_size = 14)


##Combine plots

p_cereb + p_other +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

#######################################################################################

####All CORRECTED ROIs in one plot
ggplot(results, aes(x = estimate, y = reorder(roi, estimate))) +
  geom_point(aes(color = p_adj < 0.05), size = 3) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c("TRUE" = "#de2d26", "FALSE" = "#2b8cbe"),
    labels = c("FALSE" = "ns", "TRUE" = "p_adj < .05"),
    name = "Significance"
  ) +
  labs(
    x = "Effect of Prenatal Protective Factors Score (B ± 95% CI)",
    y = "Brain Region (ROI)",
    title = "Associations Between Prenatal Protection Scores and ROI Volumes (FDR-corrected)"
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
    y = "β (Depression → ROI Volume)",
    x = "Brain Region",
    title = "Regions Associated with Prenatal Protection Scores"
  ) +
  theme_classic(base_size = 14)






##################################################################################
##
##                    Corrected Output - For Supplementals
##
##################################################################################


#Function: Fit LMEM & return full fixed-effect table


fit_lmem_full <- function(outcome, data) {
  
  fml <- as.formula(
    paste0(outcome,
           " ~ PACES + V2_T2_vol_adjusted_age + maternal_age_delivery +
              child_sex + ICV_z + (1|site)")
  )
  
  m <- lmer(fml, data = data)
  
  broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) %>%
    mutate(outcome = outcome)
}


#Apply the function across all ROI outcomes (one loop)

roi_outcomes <- colnames(df_cov)[4:26]   # your ROI variables

all_fx <- map_dfr(roi_outcomes, ~fit_lmem_full(.x, df_cov))

#all_fx now contains:
# term | estimate | SE | df | p.value | conf.low | conf.high | outcome


#Apply FDR correction PER predictor across ROIs


all_fx <- all_fx %>%
  group_by(term) %>% 
  mutate(p_adj = p.adjust(p.value, method = "BH")) %>%
  ungroup()


#Write one corrected results file per ROI

output_dir <- "output/roi_corrected_results"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

for (roi in roi_outcomes) {
  
  roi_table <- all_fx %>% filter(outcome == roi)
  
  outfile <- file.path(output_dir, paste0(roi, "_corrected_fixed_effects.txt"))
  
  capture.output(print(roi_table), file = outfile)
}


#Also write a master CSV if needed


write_csv(all_fx, "output/PACES/Reduced_Model_Output/COVARIATES_all_ROIs_corrected_fixed_effects.csv")




#Create APA-style tables for every predictor 


#All fixed-effect predictors in your model
predictor_terms <- unique(all_fx$term)

#Loop through each predictor & produce an APA table
for (term_i in predictor_terms) {
  
  message("\n\n==============================")
  message("APA TABLE FOR: ", term_i)
  message("==============================\n")
  
  apa_df <- all_fx %>%
    filter(term == term_i) %>%
    select(
      ROI = outcome,
      Estimate = estimate,
      SE = std.error,
      `CI Low` = conf.low,
      `CI High` = conf.high,
      p = p.value,
      `p (FDR)` = p_adj
    )
  
  #Print APA-style table for copy/paste into manuscript
  print(
    apa_table(
      apa_df,
      caption = paste0("Fixed Effects of ", term_i,
                       " Across ROIs With FDR Correction")
    )
  )
}

write_csv(all_fx, "output/PACES/PACES_APA_Table_all_ROIs_corrected_fixed_effects.csv")











