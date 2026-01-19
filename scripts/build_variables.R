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


df_cov <- read_parquet("data/processed/Protective_Factors_Data_08DEC2025.parquet")


#####################################Site Variable###############################

df_cov <- df_cov %>%
  rename(site = sed_basic_demographics_recruitment_site)

###Ensure everything is correct 
df_cov$site <- factor(df_cov$site)
summary(df_cov$site)

###Removing any row that is in a site with 5 or less people in it
df_cov <- df_cov %>%
  group_by(site) %>%                     #group by site
  filter(n() >= 5) %>%                   #keep only groups with >= 5 rows
  ungroup()                              #remove grouping afterwards


##Be careful, it seems like R is still counting some sites as factors in the model.
df_cov <- df_cov %>%
  filter(site %in% names(which(table(site) >= 5))) %>%
  droplevels()



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

##Now this will need to be converted into categories:
#Recode numeric education codes into descriptive labels
df_cov <- df_cov %>%
  mutate(mat_ed_cat = recode(mat_ed_cat,
                             `0` = "Never attended / Kindergarten only",
                             `1` = "1st grade",
                             `2` = "2nd grade",
                             `3` = "3rd grade",
                             `4` = "4th grade",
                             `5` = "5th grade",
                             `6` = "6th grade",
                             `7` = "7th grade",
                             `8` = "8th grade",
                             `9` = "9th grade",
                             `10` = "10th grade",
                             `11` = "11th grade",
                             `12` = "12th grade, no diploma",
                             `13` = "High school graduate",
                             `14` = "GED or equivalent",
                             `15` = "Some college, no degree",
                             `16` = "Occupational, Vocational, or Technical program",
                             `17` = "Associate degree",
                             `18` = "Bachelor's degree",
                             `19` = "Master's degree",
                             `20` = "Professional school degree",
                             `21` = "Doctoral degree",
                             `999` = "Don't know",
                             `777` = "Decline to answer"
  ))
summary(df_cov$mat_ed_cat)

###Now we want to group
df_cov <- df_cov %>%
  mutate(mat_ed_6cat = case_when(
    mat_ed_cat %in% c("Never attended / Kindergarten only",
                      "1st grade","2nd grade","3rd grade","4th grade",
                      "5th grade","6th grade","7th grade","8th grade",
                      "9th grade","10th grade","11th grade") ~ "less than HS",
    mat_ed_cat %in% c("12th grade, no diploma","High school graduate","GED or equivalent") ~ "HS or GED",
    mat_ed_cat %in% c("Some college, no degree",
                      "Occupational, Vocational, or Technical program",
                      "Associate degree") ~ "Some College or Associate",
    mat_ed_cat == "Bachelor's degree" ~ "Bachelor's degree",
    mat_ed_cat == "Master's degree" ~ "Master's degree",
    mat_ed_cat %in% c("Professional school degree","Doctoral degree") ~ "Professional or Doctoral degree"
  ))

#Check the result
table(df_cov$mat_ed_6cat)
##Convert as factors
df_cov$mat_ed_6cat <- factor(df_cov$mat_ed_6cat)

#Set reference group
df_cov$mat_ed_6cat <- relevel(df_cov$mat_ed_6cat, ref = "Bachelor's degree")

###Plot results
#Bar plot with counts labeled
ggplot(df_cov, aes(x = mat_ed_6cat)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Participant Distribution by Maternal Education",
       x = "Maternal Education",
       y = "Count")


#####Now also make a 5 category variable for maternal edu

df_cov <- df_cov %>%
  mutate(mat_ed_5cat = case_when(
    mat_ed_6cat %in% c("less than HS", "HS or GED") ~ "HS or Less",
    TRUE ~ mat_ed_6cat  #keep all other categories the same
  ))



#Check the result
table(df_cov$mat_ed_5cat)
##Convert as factors
df_cov$mat_ed_5cat <- factor(df_cov$mat_ed_5cat)
#Set reference group (e.g., Bachelors)
df_cov$mat_ed_5cat <- relevel(df_cov$mat_ed_5cat, ref = "Bachelor's degree")

###Plot results
#Bar plot with counts labeled
ggplot(df_cov, aes(x = mat_ed_5cat)) +
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
                                   na.rm = FALSE)   # <-- critical
  )


df_cov$pex_bm_apa_apa2_depr_promisrawscore <- as.numeric(as.character(df_cov$pex_bm_apa_apa2_depr_promisrawscore))

df_cov$pex_bm_apa_apa2_depr_promistscore <- as.numeric(as.character(df_cov$pex_bm_apa_apa2_depr_promistscore))




####################Maternal Age Score#################################################

##Rename
df_cov <- df_cov %>%
  rename(maternal_age_delivery = sed_basic_demographics_mother_age_delivery)

#Change to numeric
df_cov$maternal_age_delivery <- as.numeric(df_cov$maternal_age_delivery)





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


####Let's remove that one person
df_cov <- df_cov %>%
  filter(!PACES_outlier | is.na(PACES_outlier))












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
  mutate(ICV_raw = rowSums(.[, 4:31], na.rm = TRUE))




###Rescale
#Histogram + density
ggplot(df_cov, aes(ICV_raw)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", linewidth = 1) +
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
    -V2_T2_vol_Left_Lateral_Ventricle
  )

















#############################Save Data#############################################


df_cov %>%
  write.csv("data/processed/Protective_Factors_Data_Cleaned_08DEC2025.csv")

#This will save information concerning variable type
saveRDS(df_cov, "data/processed/Protective_Factors_Data_Cleaned_08DEC2025.Rds")


