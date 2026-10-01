#################################################################################
##                                                                            ##
##                      Protective Factors Project                            ##
##                       DEPRESSION LMEM - Supplmentals                       ##
##                                                                            ##
#################################################################################

################Hannah Morgan 15SEP2026#########################################
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
    paste0(outcome, " ~ pex_bm_apa_apa2_depr_promisrawscore + V2_T2_vol_adjusted_age + child_sex + mat_ed_cat  + maternal_age_delivery + ICV_z + (1|site)") 
  )
  
  ##Covariates = mat_ed_5cat   PACES  pex_bm_apa_apa2_depr_promisrawscore maternal_age_delivery V2_T2_vol_adjusted_age
  ##APA gestational age = pex_bm_apa_gestational_age
  ###Other infant age = V2_T2_vol_candidate_age sed_basic_demographics_gestational_age_delivery
  
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




###################################################################################
##
##              Standardizing
##
###################################################################################

# Standardize each ROI
df_cov_z <- df_cov %>%
  mutate(across(all_of(roi_outcomes), ~ as.numeric(scale(.x))))

results_z <- map_dfr(roi_outcomes, fit_lmem, data = df_cov_z)

results_z <- results_z %>%
  mutate(p_adj = p.adjust(p.value, method = "BH"))

results_compare <- results %>%
  select(outcome, estimate, p.value, p_adj, conf.low, conf.high) %>%
  rename(
    estimate_raw = estimate,
    p_raw = p.value,
    p_adj_raw = p_adj,
    conf_low_raw = conf.low,
    conf_high_raw = conf.high
  ) %>%
  left_join(
    results_z %>%
      select(outcome, estimate, p.value, p_adj, conf.low, conf.high) %>%
      rename(
        estimate_z = estimate,
        p_z = p.value,
        p_adj_z = p_adj,
        conf_low_z = conf.low,
        conf_high_z = conf.high
      ),
    by = "outcome"
  )

results_compare

write.csv(results_compare, "output/StandardizedandMain_results_ROIs_25SEP2026.csv", row.names = FALSE)


########################################################################################

# Make ROI names nicer
results_z <- results_z %>%
  mutate(
    roi = gsub("V2_T2_vol_", "", outcome),
    roi = gsub("_", " ", roi)
  )


## Forest plot with standardized estimates
ggplot(results_z, aes(x = estimate, y = reorder(roi, estimate))) +
  geom_point(aes(color = p_adj < 0.05), size = 3) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"), 
    labels = c("FALSE" = "ns", "TRUE" = "p < .05"),
    name = "Significance"
  ) +
  labs(
    x = "Standardized estimate (β)",
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

##################Breaking up plot
results_z <- results_z %>%
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
  results_z %>% filter(roi_group == "Cerebellum"),
  aes(x = estimate, y = reorder(roi, estimate))
) +
  geom_point(aes(color = p_adj < 0.05), size = 3, show.legend = FALSE) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Cerebellum & Cortical ROIs",
    x = "Standardized estimate (β)",
    y = "Brain Region (ROI)"
  ) +
  scale_color_manual(
    values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"),
    labels = c("FALSE" = "ns", "TRUE" = "p_adj < .05"),
    name = "Significance"
  ) +
  theme_minimal(base_size = 16)


p_other <- ggplot(
  results_z %>% filter(roi_group == "Other ROIs"),
  aes(x = estimate, y = reorder(roi, estimate))
) +
  geom_point(aes(color = p_adj < 0.05), size = 3) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Subcortical ROIs",
    x = "Standardized estimate (β)",
    y = "Brain Region (ROI)"
  ) +
  scale_color_manual(
    values = c("TRUE" = "#2b8cbe", "FALSE" = "#de2d26"),
    labels = c("FALSE" = "ns", "TRUE" = "p < .05"),
    name = "Significance"
  ) +
  theme_minimal(base_size = 16)

p_other + p_cereb +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")




ggsave(
  filename = "output/Depression/Standardized_ROIs_Depression_forest_plot_17SEP2026.png",   # file name (can be .png, .pdf, .jpeg, etc.)
  width = 14,                      # width in inches
  height = 8,                     # height in inches
  dpi = 300                        # resolution (good for publications)
)














########################################################################################
##
##        Supplemental Tables
##
#########################################################################################

fit_lmem_full <- function(outcome, data, outdir = "output") {
  
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  fml <- as.formula(
    paste0(
      outcome,
      " ~ pex_bm_apa_apa2_depr_promisrawscore +
         V2_T2_vol_adjusted_age +
         child_sex +
         mat_ed_cat +
         maternal_age_delivery +
         ICV_z +
         (1|site)"
    )
  )
  
  m <- lmerTest::lmer(fml, data = data)
  
  broom.mixed::tidy(
    m,
    effects = "fixed",
    conf.int = TRUE,
    p.value = TRUE
  ) %>%
    mutate(outcome = outcome)
}


results_full <- map_dfr(
  roi_outcomes,
  fit_lmem_full,
  data = df_cov
)

results_full <- results_full %>%
  left_join(
    results %>%
      select(outcome, p_adj),
    by = "outcome"
  )




results_full <- results_full %>%
  mutate(
    p_FDR = ifelse(
      term == "pex_bm_apa_apa2_depr_promisrawscore",
      p_adj,
      NA_real_
    )
  ) %>%
  select(
    outcome,
    term,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value,
    p_FDR, 
    df
  )



saveRDS(results_full, "output/Main_results_full_17SEP2026.rds")

write.csv(results_full, "output/Main_results_full_17SEP2026.csv", row.names = FALSE)








#####################################################################################
##          Correcting across everything as a triple check, all 7 ROIs are significant



fit_lmem_full <- function(outcome, data, outdir = "output") {
  
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  fml <- as.formula(
    paste0(
      outcome,
      " ~ pex_bm_apa_apa2_depr_promisrawscore +
         V2_T2_vol_adjusted_age +
         child_sex +
         mat_ed_cat +
         maternal_age_delivery +
         ICV_z +
         (1|site)"
    )
  )
  
  m <- lmerTest::lmer(fml, data = data)
  
  broom.mixed::tidy(
    m,
    effects = "fixed",
    conf.int = TRUE,
    p.value = TRUE
  ) %>%
    mutate(outcome = outcome)
}


results_full <- map_dfr(
  roi_outcomes,
  fit_lmem_full,
  data = df_cov
)

results_full <- results_full %>%
  left_join(
    results %>%
      select(outcome, p_adj),
    by = "outcome"
  )

results_full <- results_full %>%
  mutate(
    p_FDR_everything = p.adjust(p.value, method = "BH")
  )

results_everything_corr <- results_full %>%
  filter(p_FDR_everything < 0.05) %>%
  arrange(p_FDR_everything)


write.csv(
  results_full, "output/main_PMDS_full_results_17SEP2026.csv", row.names = FALSE)
