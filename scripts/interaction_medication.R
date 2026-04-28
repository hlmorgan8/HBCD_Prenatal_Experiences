#################################################################################
##                                                                            ##
##                      Protective Factors Project                            ##
##                       Interaction x Meds LMEM                              ##
##                                                                            ##
#################################################################################

################Hannah Morgan 25FEB2026#########################################
##                                                                            ##
################################################################################




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
library(interactions)
library(emmeans)




#Read the RDS file and assign it to a variable - this should have variables saved properly
df_cov <- readRDS("data/processed/Protective_Factors_Data_Medication_Cleaned_2_19MAR2026.Rds")





###pex_bm_health_preg__meds_001 - should be a factor, "have you used prescription meds during pregnancy, Yes (1) or No"
####pex_bm_health_preg__meds_002 - should be numeric and is asking how many prescribed meds have you taken during pregnancy








####################################################################################
##
##               Medicaiton Breakdown Check
##
#####################################################################################

##Proper N which accounts for NAs (LME below already does this)
df_cov %>%
  filter(!is.na(pex_bm_apa_apa2_depr_promisrawscore)) %>%
  count(pex_bm_health_preg__meds_001) %>%  ##depressed_meds
  mutate(percent = n / sum(n) * 100)










####################################################################################
##
##               Split up DF
##
#####################################################################################

df_sex <- df_cov %>%
  filter(child_sex == 1) #0 is female, 1 is male

df_meds <- df_cov %>%
  filter(pex_bm_health_preg__meds_001 == 0) #0 is people that reported no medication usage (N should be 677; note note automatically excluding NAs)

df_meds_depr <- df_cov %>%
  filter(depressed_meds == 0) #0 is people that reported no depression medication usage (N should be 1784; but note this is not automatically excluding NAs)


####################################################################################
##
##                Prenatal Medication Interaction
##
#####################################################################################



#outcome <- ("V2_T2_vol_Right_Amygdala")

outcome <- c("V2_T2_vol_Right_Amygdala", "V2_T2_vol_Right_Pallidum", "V2_T2_vol_Left_Pallidum", "V2_T2_vol_Right_Accumbens_area",
             "V2_T2_vol_Left_Accumbens_area", "V2_T2_vol_Right_Putamen", "V2_T2_vol_Left_Putamen")

#outcome <- colnames(df_cov)[4:24] 


fit_lmem_mod <- function(outcome, data) {
  
  # Build formula
  fml <- as.formula(
    paste0(
      outcome,
      " ~  pex_bm_apa_apa2_depr_promisrawscore*pex_bm_health_preg__meds_001 + ",
      "mat_ed_cat + child_sex + V2_T2_vol_adjusted_age + maternal_age_delivery + ",
      "ICV_z + (1|site)"
    )
  )
  
  #Fit model
  m <- lmer(fml, data = data)
  
  #Get all fixed effects with confidence intervals
  tidy_table <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) %>%
    mutate(
      outcome = outcome,
      interaction = str_detect(term, "pex_bm_apa_apa2_depr_promisrawscore:pex_bm_health_preg__meds_001")
    ) %>%
    # Put interaction term first
    arrange(desc(interaction))
  tidy_table <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) %>%
    mutate(
      outcome = outcome,
      interaction = str_detect(term, "pex_bm_apa_apa2_depr_promisrawscore:pex_bm_health_preg__meds_001"),
      sig = case_when(
        p.value < .001 ~ "***",
        p.value < .01  ~ "**",
        p.value < .05  ~ "*",
        TRUE ~ ""
      )
    ) %>%
    arrange(desc(interaction))
  
  return(tidy_table)
}



#interaction result
#interaction_result <- fit_lmem_mod(outcome, df_cov) - for one outcome
results <- map_dfr(outcome, fit_lmem_mod, data = df_cov) 

#View all fixed effects
#interaction_result - for one outcome
results

#Flag significant effects - UNCORRECTED
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



################################Interaction plot#########################################
m_full <- lmer(
  V2_T2_vol_Right_Amygdala ~ pex_bm_apa_apa2_depr_promisrawscore * pex_bm_health_preg__meds_001 +
    mat_ed_cat + child_sex + V2_T2_vol_adjusted_age + maternal_age_delivery + ICV_z + (1|site),
  data = df_cov
)


interact_plot(m_full, pred = pex_bm_apa_apa2_depr_promisrawscore, modx = pex_bm_health_preg__meds_001, plot.points = FALSE, interval = TRUE,
              int.width = 0.8) #all other variables are centered



ggsave(
  filename = "output/Depression/Meds_Continuous_Interaction_plot_06APR2026.png",   #file name (can be .png, .pdf, .jpeg, etc.)
  width = 8,                      #width in inches
  height = 6,                     #height in inches
  dpi = 300                        #resolution (good for publications)
)

sim_slopes(
  m_full,
  pred = pex_bm_apa_apa2_depr_promisrawscore,
  modx = pex_bm_health_preg__meds_001
)











####################################################################################
##
##                Prenatal Medication Interaction - Depressed Medication
##
#####################################################################################


outcome <- "V2_T2_vol_Right_Amygdala"
fit_lmem_mod <- function(outcome, data) {
  
  # Build formula
  fml <- as.formula(
    paste0(
      outcome,
      " ~  pex_bm_apa_apa2_depr_promisrawscore*depressed_meds + ",
      "mat_ed_cat + child_sex + V2_T2_vol_adjusted_age + maternal_age_delivery + ",
      "ICV_z + (1|site)"
    )
  )
  
  # Fit model
  m <- lmer(fml, data = data)
  
  # Get all fixed effects with confidence intervals
  tidy_table <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) %>%
    mutate(
      outcome = outcome,
      interaction = str_detect(term, "pex_bm_apa_apa2_depr_promisrawscore:depressed_meds")
    ) %>%
    # Put interaction term first
    arrange(desc(interaction))
  tidy_table <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) %>%
    mutate(
      outcome = outcome,
      interaction = str_detect(term, "pex_bm_apa_apa2_depr_promisrawscore:depressed_meds"),
      sig = case_when(
        p.value < .001 ~ "***",
        p.value < .01  ~ "**",
        p.value < .05  ~ "*",
        TRUE ~ ""
      )
    ) %>%
    arrange(desc(interaction))
  
  return(tidy_table)
}



#Model here
interaction_result <- fit_lmem_mod("V2_T2_vol_Right_Amygdala", df_cov)

# View all fixed effects
interaction_result



################################Interaction plot#########################################
m_full <- lmer(
  V2_T2_vol_Right_Amygdala ~ pex_bm_apa_apa2_depr_promisrawscore * depressed_meds +
    mat_ed_cat + child_sex + V2_T2_vol_adjusted_age + maternal_age_delivery + ICV_z + (1|site),
  data = df_cov
)


interact_plot(m_full, pred = pex_bm_apa_apa2_depr_promisrawscore, modx = depressed_meds, plot.points = FALSE, interval = TRUE,
              int.width = 0.8) #all other variables are centered


ggsave(
  filename = "output/Depression/Depressed_Meds_Group_Interaction_plot_06APR2026.png",   # file name (can be .png, .pdf, .jpeg, etc.)
  width = 8,                      # width in inches
  height = 6,                     # height in inches
  dpi = 300                        # resolution (good for publications)
)




























####################################################################################
##
##                Prenatal Medication Interaction - Removing Meds/Sensitivity Analyses
##
#####################################################################################

#outcome <- ("V2_T2_vol_Right_Amygdala")
outcome <- c("V2_T2_vol_Right_Amygdala", "V2_T2_vol_Right_Pallidum", "V2_T2_vol_Left_Pallidum", "V2_T2_vol_Right_Accumbens_area",
             "V2_T2_vol_Left_Accumbens_area", "V2_T2_vol_Right_Putamen", "V2_T2_vol_Left_Putamen")

fit_lmem_mod <- function(outcome, data) {
  
  # Build formula
  fml <- as.formula(
    paste0(
      outcome,
      " ~  pex_bm_apa_apa2_depr_promisrawscore + ",
      "mat_ed_cat + child_sex + V2_T2_vol_adjusted_age + maternal_age_delivery + ",
      "ICV_z + (1|site)"
    )
  )
  
  #Fit model
  m <- lmer(fml, data = data)
  
  #Get all fixed effects with confidence intervals
  tidy_table <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) %>%
    mutate(
      outcome = outcome,
      interaction = str_detect(term, "pex_bm_apa_apa2_depr_promisrawscore")
    ) %>%
    # Put interaction term first
    arrange(desc(interaction))
  tidy_table <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) %>%
    mutate(
      outcome = outcome,
      interaction = str_detect(term, "pex_bm_apa_apa2_depr_promisrawscore"),
      sig = case_when(
        p.value < .001 ~ "***",
        p.value < .01  ~ "**",
        p.value < .05  ~ "*",
        TRUE ~ ""
      )
    ) %>%
    arrange(desc(interaction))
  
  return(tidy_table)
}



#interaction result
#interaction_result <- fit_lmem_mod(outcome, df_meds) #change the df depending on your sensitivity test
#Run across all ROIs
interaction_result <- map_dfr(outcome, fit_lmem_mod, data = df_meds_depr) #for multiple ROIs

#View all fixed effects
interaction_result

#Add multiple correction adjustment (BH)
interaction_result <- interaction_result %>%
  mutate(p_adj = p.adjust(p.value, method = "BH"))

#Flag significant results after FDR
sig_results_adj <- interaction_result %>%
  filter(p_adj < 0.05) %>%
  arrange(p_adj)

#Seeing output
interaction_result
sig_results_adj



################################Main Effect plot#########################################
ggplot(df_meds_depr, aes(
  x = pex_bm_apa_apa2_depr_promisrawscore,    
  y = V2_T2_vol_Right_Amygdala
)) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Depression (v02)",
    y = "Right Amygdala Volume",
    title = "Right Amygdala vs Depression (Medications for Depression Symptoms Removed)"
  ) +
  theme_minimal()


ggsave(
  filename = "output/Depression/DeprMeds_Depression_Slope_Right_Amygdala_plot_13APR2026.png",   # file name (can be .png, .pdf, .jpeg, etc.)
  width = 10,                      # width in inches
  height = 10,                     # height in inches
  dpi = 300                        # resolution (good for publications)
)




#################Binding the three plots together
df_cov$group <- "Full sample"
df_meds$group <- "Prescription Meds removed"
df_meds_depr$group <- "Depression meds removed"

df_all <- bind_rows(df_cov, df_meds, df_meds_depr)

ggplot(df_all, aes(
  x = pex_bm_apa_apa2_depr_promisrawscore,
  y = V2_T2_vol_Right_Amygdala,
  color = group
)) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Depression (v01)",
    y = "Right Amygdala Volume",
    title = "Right Amygdala vs Depression Across Samples"
  ) +
  theme_minimal()

ggsave(
  filename = "output/Depression/AllMeds_Slope_Right_Amygdala_plot_20APR2026.png",   # file name (can be .png, .pdf, .jpeg, etc.)
  width = 10,                      # width in inches
  height = 10,                     # height in inches
  dpi = 300                        # resolution (good for publications)
)


















####################################################################################
##
##                Prenatal Medication Interaction - Follow up Tests
##
#####################################################################################
glm_depr <- glm(depressed_meds ~ pex_bm_apa_apa2_depr_promisrawscore +
      mat_ed_cat + child_sex + V2_T2_vol_adjusted_age + maternal_age_delivery, 
    data = df_cov,
    family = binomial)
summary(glm_depr)



t.test(
  pex_bm_apa_apa2_depr_promisrawscore ~ depressed_meds,
  data = df_cov
)

df_cov %>%
  group_by(depressed_meds) %>%
  summarise(
    mean_dep = mean(pex_bm_apa_apa2_depr_promisrawscore, na.rm = TRUE),
    sd_dep = sd(pex_bm_apa_apa2_depr_promisrawscore, na.rm = TRUE),
    n = n()
  )

