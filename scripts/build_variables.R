#################################################################################
##                                                                            ##
##                      Protective Factors Project                            ##
##                            Building Factors                                ##
##                                                                            ##
#################################################################################

################Hannah Morgan 19JAN2026#########################################
##                                                                            ##
################################################################################

#install.packages("ggplot2")
#install.packages("stringr")

library(arrow)
library(ggplot2)
library(stringr)
library(dplyr)

df_cov <- read_parquet("data/processed/Protective_Factors_Data_Medication_2_18MAR2026.parquet") #This is incorporating prenatal medication
#df_cov <- read_parquet("data/processed/Protective_Factors_Data_APA_Quality_Control_2_08APR2026.parquet") #This is looking at QC measures
#df_cov <- read_parquet("data/processed/Protective_Factors_Data_Family_HX_2_16MAR2026.parquet") #This is incorporating family hx
#df_cov <- read_parquet("data/processed/Protective_Factors_Data_APA_Pre_Postnatal_2_12MAR2026.parquet")
#df_cov <- read_parquet("data/processed/Protective_Factors_Data_APA-Postnatal_2_12MAR2026.parquet")  #Postnatal (V02) APA scores
#df_cov <- read_parquet("data/processed/Protective_Factors_Data_MAPS_2_09MAR2026.parquet") ##MAPS and PACES
#df_cov <- read_parquet("data/processed/Protective_Factors_Data_IBQ_2_04MAR2026.parquet") ##IBQ and PACES
#df_cov <- read_parquet("data/processed/Protective_Factors_Data_PROMIS_2_20Feb2026.parquet")
##df without paces is Protective_Factors_Data_2_17Feb2026.parquet

#Edinburgh
#df_cov <- read_parquet("data/processed/Protective_Factors_Data_EDINBURGH_Session2_2_11MAR2026.parquet") #Postnatal
#df_cov <- read_parquet("data/processed/Protective_Factors_Data_EDINBURGH_2_25Feb2026.parquet")

#####################################Site Variable###############################

df_cov <- df_cov %>%
  rename(site = sed_basic_demographics_recruitment_site)

###Ensure everything is correct 
df_cov$site <- factor(df_cov$site)
summary(df_cov$site)

###Removing any row that is in a site with 5 or less people in it
df_cov <- df_cov %>%
  group_by(site) %>%                     #group by site
  filter(n() >= 10) %>%                   #keep only groups with >= 5 rows
  ungroup()                              #remove grouping afterwards
df_cov <- droplevels(df_cov)




#Bar plot with counts labeled
ggplot(df_cov, aes(x = site)) +
  geom_bar(fill = "steelblue") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5
  ) +
  theme_minimal() +
  labs(
    title = "Participant Distribution by Study Site",
    x = "Site",
    y = "Count"
  )







###########################Sex Variable###########################################
df_cov <- df_cov %>%
  rename(child_sex = sed_basic_demographics_sex)

#Set "1" (Male) as reference for child_sex
df_cov$child_sex <- factor(df_cov$child_sex)
df_cov$child_sex <- relevel(df_cov$child_sex, ref = "1")
summary(df_cov$child_sex)





##############################Maternal Education#####################################
df_cov <- df_cov %>%
  rename(mat_ed_cat = sed_basic_demographics_rc_mother_education)

##Convert as factors
df_cov$mat_ed_cat <- factor(df_cov$mat_ed_cat)




##Summarize what we have
summary(df_cov$mat_ed_cat)
##1 = High School, 2 = Some College or Associates Degree, 3 = Bachelor's Degree, 4 = Graduate or Professional

#Set reference group
df_cov$mat_ed_cat <- relevel(df_cov$mat_ed_cat, ref = "3")

###Plot results
#Bar plot with counts labeled
ggplot(df_cov, aes(x = mat_ed_cat)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Participant Distribution by Maternal Education",
       x = "Maternal Education",
       y = "Count")








####################Depression Score#################################################



df_cov <- df_cov %>%
  mutate(
    across(pex_bm_apa_2_depr_001:pex_bm_apa_2_depr_008, ~as.numeric(.)),
    apa_2_depr_sum_score = rowSums(across(pex_bm_apa_2_depr_001:pex_bm_apa_2_depr_008),
                                   na.rm = FALSE)   
  )


df_cov$pex_bm_apa_apa2_depr_promisrawscore <- as.numeric(as.character(df_cov$pex_bm_apa_apa2_depr_promisrawscore))
##IF you get a NAs introduced by coercian score, that is because one participant was given a "not enough responses" instead of NA

df_cov$pex_bm_apa_apa2_depr_promistscore <- as.numeric(as.character(df_cov$pex_bm_apa_apa2_depr_promistscore))


#########use for Edinburgh only################
#df_cov$pex_bm_epds_total_score <- as.numeric(as.character(df_cov$pex_bm_epds_total_score))

#use for combined pre and postnatal only
#df_cov$pex_bm_apa_apa2_depr_promisrawscore_prenatal <- as.numeric(as.character(df_cov$pex_bm_apa_apa2_depr_promisrawscore_prenatal))
#df_cov$pex_bm_apa_apa2_depr_promisrawscore_postnatal <- as.numeric(as.character(df_cov$pex_bm_apa_apa2_depr_promisrawscore_postnatal))



####################Maternal Age Score#################################################

##Rename
df_cov <- df_cov %>%
  rename(maternal_age_delivery = sed_basic_demographics_mother_age_delivery)

#Change to numeric
df_cov$maternal_age_delivery <- as.numeric(df_cov$maternal_age_delivery)









############################Brain Volume/ICV####################################

##We will need to change variable names so they aren't so long
###Then we will need to create ICV (total brain volume)
####And then will need to rescale


###This will rename to "V2_T2_XXXX"
df_cov <- df_cov %>%
  #First rename the BIBSNet columns
  rename_with(~ str_replace(.x,
                            "^img_bibsnet_space-T2w_desc-aseg_volumes_(.*)$",
                            "V2_T2_vol_\\1")) %>%
  #Then replace any "-" with "_"
  rename_with(~ str_replace_all(.x, "-", "_"))


##Note that we need to remove 3rd and 4th ventricles and brain stem from the dataset but FIRST WE NEED TO CALCULATE ICV!
df_cov <- df_cov %>%
  mutate(ICV_raw = rowSums(.[, 4:29], na.rm = FALSE))

##[4, 31] for 1.1


###Rescale
#Histogram + density
ggplot(df_cov, aes(ICV_raw)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30,
                 fill = "steelblue",
                 alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal()


###Show sex differences in ICV
ggplot(df_cov, aes(ICV_raw)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30,
                 alpha = 0.4,
                 position = "identity") +
  geom_density(aes(color = child_sex), linewidth = 1) +
  theme_minimal()



range(df_cov$ICV_raw, na.rm = TRUE)


##ICV is magnitudes different, we should rescale
df_cov <- df_cov %>%
  mutate(ICV_z = scale(ICV_raw))


#Remove columns by name to avoid confusion
df_cov <- df_cov %>%
  dplyr::select(
    -V2_T2_vol_3rd_Ventricle,
    -V2_T2_vol_4th_Ventricle,
    -V2_T2_vol_Brain_Stem,
    -V2_T2_vol_CSF,
    -V2_T2_vol_Right_Lateral_Ventricle,
    -V2_T2_vol_Left_Lateral_Ventricle,
    -V2_T2_vol_Unknown
  )








#######################THE FOLLOWING ARE ONLY RELEVANT DF DEPENDING################

######################################################################################
##Family history of depression 005 is depression (Yes, No) 
### 1 = YES

#df_cov <- df_cov %>%
#  mutate(across(c(pex_bm_psych_bm_005, pex_bm_psych_bf_005), as.factor))






#################################################################################
####Medication during prenatal period 
#####There are two outputs for now:
###pex_bm_health_preg__meds_001 - should be a factor, "have you used prescription meds during pregnancy, Yes (1) or No"
####pex_bm_health_preg__meds_002 - should be numeric and is asking how many prescribed meds have you taken during pregnancy

##Convert as factors
#df_cov$pex_bm_health_preg__meds_001 <- factor(df_cov$pex_bm_health_preg__meds_001)
#df_cov$pex_bm_health_preg__meds_001 <- relevel(df_cov$pex_bm_health_preg__meds_001, ref = "0")
#summary(df_cov$pex_bm_health_preg__meds_001)


#Change to numeric
#df_cov$pex_bm_health_preg__meds_002 <- as.numeric(df_cov$pex_bm_health_preg__meds_002)
#summary(df_cov$pex_bm_health_preg__meds_002)


##Make a new column of those with depression and are medicated for it
#med_cols <- sprintf("pex_bm_health_preg__meds_%03d__02", 3:12)


# Pattern covering all your codes
#dep_pattern <- "^F32|^F33|^F34|^F39|^F43.2|^F43.23"

#df_cov <- df_cov %>%
#  mutate(
#    depressed_meds = if_else(
#      rowSums(across(all_of(med_cols), ~ str_detect(., dep_pattern)), na.rm = TRUE) > 0,
#      1, 0
#    )
#  )
#table(df_cov$depressed_meds)

#df_cov$depressed_meds <- factor(df_cov$depressed_meds)
#df_cov$depressed_meds <- relevel(df_cov$depressed_meds, ref = "0")
#summary(df_cov$depressed_meds)



#############################Save Data#############################################


df_cov %>%
  write.csv("data/processed/Protective_Factors_Data_MRI_QC_Cleaned_2_08APR2026.csv")

#This will save information concerning variable type
saveRDS(df_cov, "data/processed/Protective_Factors_Data_MRI_QC_Cleaned_2_08APR2026.Rds")
##with PACES only Protective_Factors_Data_Cleaned_2_20FEB2026.Rds
###Protective_Factors_Data_PROMIS_Cleaned_2_24FEB2026.Rds - With PROMIS, APA Depression, PACES, sites removed
####Protective_Factors_Data_PROMIS_Cleaned_2_04MAR2026.Rds - With PROMIS, APA Depression, PACES, sites removed and levels dropped







































###############################Code relevant for other data/releases


###################PACES Score#####################################################

##Rename
df_cov <- df_cov %>%
  rename(PACES = sed_bm_paces_summary_score)

#Numeric
df_cov$PACES <- as.numeric(df_cov$PACES)


####Outlier Check
###Run this to remove outlier#####

#Flag extreme values using z-scores
df_cov <- df_cov %>%
  mutate(PACES_z = scale(PACES),
         PACES_outlier = abs(PACES_z) > 4)

table(df_cov$PACES_outlier)


####Let's remove that one person (in 1.1) and 4 people in 2.0
df_cov <- df_cov %>%
  filter(!PACES_outlier | is.na(PACES_outlier))






###########THIS IS RELEVANT FOR 1.0 OR 1.1 ONLY#############################
##Now this will need to be converted into categories:
#Recode numeric education codes into descriptive labels
# df_cov <- df_cov %>%
#   mutate(mat_ed_cat = recode(mat_ed_cat,
#                              `0` = "Never attended / Kindergarten only",
#                              `1` = "1st grade",
#                              `2` = "2nd grade",
#                              `3` = "3rd grade",
#                              `4` = "4th grade",
#                              `5` = "5th grade",
#                              `6` = "6th grade",
#                              `7` = "7th grade",
#                              `8` = "8th grade",
#                              `9` = "9th grade",
#                              `10` = "10th grade",
#                              `11` = "11th grade",
#                              `12` = "12th grade, no diploma",
#                              `13` = "High school graduate",
#                              `14` = "GED or equivalent",
#                              `15` = "Some college, no degree",
#                              `16` = "Occupational, Vocational, or Technical program",
#                              `17` = "Associate degree",
#                              `18` = "Bachelor's degree",
#                              `19` = "Master's degree",
#                              `20` = "Professional school degree",
#                              `21` = "Doctoral degree",
#                              `999` = "Don't know",
#                              `777` = "Decline to answer"
#   ))
# summary(df_cov$mat_ed_cat)
# 
# ###Now we want to group
# df_cov <- df_cov %>%
#   mutate(mat_ed_6cat = case_when(
#     mat_ed_cat %in% c("Never attended / Kindergarten only",
#                       "1st grade","2nd grade","3rd grade","4th grade",
#                       "5th grade","6th grade","7th grade","8th grade",
#                       "9th grade","10th grade","11th grade") ~ "less than HS",
#     mat_ed_cat %in% c("12th grade, no diploma","High school graduate","GED or equivalent") ~ "HS or GED",
#     mat_ed_cat %in% c("Some college, no degree",
#                       "Occupational, Vocational, or Technical program",
#                       "Associate degree") ~ "Some College or Associate",
#     mat_ed_cat == "Bachelor's degree" ~ "Bachelor's degree",
#     mat_ed_cat == "Master's degree" ~ "Master's degree",
#     mat_ed_cat %in% c("Professional school degree","Doctoral degree") ~ "Professional or Doctoral degree"
#   ))
# 
# #Check the result
# table(df_cov$mat_ed_6cat)
# ##Convert as factors
# df_cov$mat_ed_6cat <- factor(df_cov$mat_ed_6cat)

#Set reference group
#df_cov$mat_ed_6cat <- relevel(df_cov$mat_ed_6cat, ref = "Bachelor's degree")
######################################################################################



# #####Now also make a 5 category variable for maternal edu ONLY FOR 1.0 or 1.1 RELEASE########
# 
# df_cov <- df_cov %>%
#   mutate(mat_ed_5cat = case_when(
#     mat_ed_6cat %in% c("less than HS", "HS or GED") ~ "HS or Less",
#     TRUE ~ mat_ed_6cat  #keep all other categories the same
#   ))
# 
# 
# 
# #Check the result
# table(df_cov$mat_ed_5cat)
# ##Convert as factors
# df_cov$mat_ed_5cat <- factor(df_cov$mat_ed_5cat)
# #Set reference group (e.g., Bachelors)
# df_cov$mat_ed_5cat <- relevel(df_cov$mat_ed_5cat, ref = "Bachelor's degree")
# 
# ###Plot results
# #Bar plot with counts labeled
# ggplot(df_cov, aes(x = mat_ed_5cat)) +
#   geom_bar(fill = "steelblue") +
#   geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
#   theme_minimal() +
#   labs(title = "Participant Distribution by Maternal Education",
#        x = "Maternal Education",
#        y = "Count")



