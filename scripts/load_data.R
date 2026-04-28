#################################################################################
##                                                                            ##
##                      Protective Factors Project                            ##
##                            Loading Data                                    ##
##                                                                            ##
#################################################################################

################Hannah Morgan 19JAN2026#########################################
##                                                                            ##
################################################################################


#install.packages("dplyr")
#install.packages("arrow")
library(dplyr)
library(arrow)

##This will load into the data directory to grab the data but assumes you have a symbolic link - the below will help you set that up
###to create this, go to Terminal and enter and log into the in VACC (HPC specific to UVM) 
#####cd into your project's raw data folder
######  cd /XX/XX/XX/data/raw


##This is loading in data from your data dictionary
###If you are in scratch space use this:  "data/raw/phenotype/"

load_data <- function(fname, base_path = "data/raw/2.0/rawdata/phenotype/"){
  fpath <- paste0(base_path, fname)
  arrow::read_parquet(fpath)
}


#This pulls in your separate measures of interest
structure_t2 <- load_data("img_bibsnet_space-T2w_desc-aseg_volumes.parquet")
paces <- load_data("sed_bm_paces.parquet")
static_dem <- load_data("sed_basic_demographics.parquet")
apa_depression <- load_data("pex_bm_apa.parquet")  
promis <- load_data("sed_bm_strsup.parquet")
edinburgh_depression <- load_data("pex_bm_epds.parquet")
temperament <- load_data("mh_cg_ibqr.parquet")
irritability <- load_data("mh_cg_mapdb__inf.parquet")
family_hx <- load_data("pex_bm_psych.parquet")
medication <- load_data("pex_bm_health_preg__meds.parquet")
quality_control_mri <- load_data("img_mriqc_T2w.parquet")





####################################################################################
#This gathers the measures into one based on variables that should match up
gather_paces_data <- function(structure_t2, static_dem, apa_depression, promis) {
  structure_t2 %>%
    filter(session_id == 'ses-V02') %>%
    select(-session_id) %>%
    left_join(
      static_dem %>%
        filter(session_id == 'ses-V02') %>%
        select(-session_id)
    ) %>%
    left_join(
      apa_depression %>%
      filter(session_id == 'ses-V01') %>%  #change depending on post or prenatal depression
        select(-session_id)
    ) %>%
    left_join(
      promis %>%
        filter(session_id == 'ses-V01') %>%
        select(-session_id)
    ) %>%
    left_join(
      paces %>%     #change to paces
      filter(session_id == 'ses-V01') %>%
        select(-session_id)
    )
}



#This makes the final data frame
paces_data <- gather_paces_data(
  structure_t2,
  static_dem,
  apa_depression,
  promis
)


#Save the combined data frame
##PACES Data
#arrow::write_parquet(paces_data, "data/processed/Protective_Factors_Data_2_17Feb2026.parquet")

#PROMIS Data
arrow::write_parquet(paces_data, "data/processed/Protective_Factors_Data_APA-Postnatal_2_12MAR2026.parquet")







###################################################################################################
#This gathers the measures into one based on variables that should match up
gather_paces_data <- function(structure_t2, static_dem, edinburgh_depression, promis) {
  structure_t2 %>%
    filter(session_id == 'ses-V02') %>%
    select(-session_id) %>%
    left_join(
      static_dem %>%
        filter(session_id == 'ses-V02') %>%
        select(-session_id)
    ) %>%
    left_join(
      edinburgh_depression %>%
        filter(session_id == 'ses-V02') %>%  #change session depending
        select(-session_id)
    ) %>%
    left_join(
      promis %>%
        filter(session_id == 'ses-V01') %>%
        select(-session_id)
    ) %>%
    left_join(
      paces %>%     #change to paces
        filter(session_id == 'ses-V01') %>%
        select(-session_id)
    )
}



#This makes the final data frame
edinburgh_data <- gather_paces_data(
  structure_t2,
  static_dem,
  edinburgh_depression,
  promis
)


#Save the combined data frame

#PROMIS Data
arrow::write_parquet(edinburgh_data, "data/processed/Protective_Factors_Data_EDINBURGH_Session2_2_11MAR2026.parquet")










#################################################################################
##IBQ Data


#This gathers the measures into one based on variables that should match up
gather_paces_data <- function(structure_t2, static_dem, apa_depression, temperament) {
  structure_t2 %>%
    filter(session_id == 'ses-V02') %>%
    select(-session_id) %>%
    left_join(
      static_dem %>%
        filter(session_id == 'ses-V02') %>%
        select(-session_id)
    ) %>%
    left_join(
      apa_depression %>%
        filter(session_id == 'ses-V01') %>%
        select(-session_id)
    ) %>%
    left_join(
      temperament %>%
        filter(session_id == 'ses-V03') %>%
        select(-session_id)
    ) %>%
    left_join(
      paces %>%     #change to paces
        filter(session_id == 'ses-V01') %>%
        select(-session_id)
    )
}



#This makes the final data frame
temperament_data <- gather_paces_data(
  structure_t2,
  static_dem,
  apa_depression,
  temperament
)


#Save the combined data frame

#IBQ Data
arrow::write_parquet(temperament_data, "data/processed/Protective_Factors_Data_IBQ_2_04MAR2026.parquet")































#################################################################################
#MAPS Data


#This gathers the measures into one based on variables that should match up
gather_paces_data <- function(structure_t2, static_dem, apa_depression, irritability) {
  structure_t2 %>%
    filter(session_id == 'ses-V02') %>%
    select(-session_id) %>%
    left_join(
      static_dem %>%
        filter(session_id == 'ses-V02') %>%
        select(-session_id)
    ) %>%
    left_join(
      apa_depression %>%
        filter(session_id == 'ses-V01') %>%
        select(-session_id)
    ) %>%
    left_join(
      irritability %>%
        filter(session_id == 'ses-V03') %>%
        select(-session_id)
    ) %>%
    left_join(
      paces %>%     #change to paces
        filter(session_id == 'ses-V01') %>%
        select(-session_id)
    )
}



#This makes the final data frame
irritability_data <- gather_paces_data(
  structure_t2,
  static_dem,
  apa_depression,
  irritability
)


#Save the combined data frame

#MAPS Data
arrow::write_parquet(irritability_data, "data/processed/Protective_Factors_Data_MAPS_2_09MAR2026.parquet")

































#########################################################################################

##trying to add in APA2 sessions 1 and 2 into the same df
##be careful here since structure_t2 isn't first so will impact the ICV calculation

gather_paces_data <- function(structure_t2, static_dem, apa_depression, promis) {
  
  # Prenatal depression (V01)
  apa_prenatal <- apa_depression %>%
    filter(session_id == "ses-V01") %>%
    select(-session_id) %>%
    rename_with(~ paste0(., "_prenatal"), -participant_id)
  
  # Postnatal depression (V02)
  apa_postnatal <- apa_depression %>%
    filter(session_id == "ses-V02") %>%
    select(-session_id) %>%
    rename_with(~ paste0(., "_postnatal"), -participant_id)
  
  structure_t2 %>%
    filter(session_id == "ses-V02") %>%
    select(-session_id) %>%
    left_join(
      static_dem %>%
        filter(session_id == "ses-V02") %>%
        select(-session_id),
      by = "participant_id"
    ) %>%
    left_join(apa_prenatal, by = "participant_id") %>%
    left_join(apa_postnatal, by = "participant_id") %>%
    left_join(
      promis %>%
        filter(session_id == "ses-V01") %>%
        select(-session_id),
      by = "participant_id"
    ) %>%
    left_join(
      paces %>%
        filter(session_id == "ses-V01") %>%
        select(-session_id),
      by = "participant_id"
    )
}


#This makes the final data frame
paces_data <- gather_paces_data(
  structure_t2,
  static_dem,
  apa_depression,
  promis
)


#Save the combined data frame
##PACES Data
#arrow::write_parquet(paces_data, "data/processed/Protective_Factors_Data_2_17Feb2026.parquet")

#PROMIS Data
arrow::write_parquet(paces_data, "data/processed/Protective_Factors_Data_APA_Pre_Postnatal_2_12MAR2026.parquet")



































#########################################################################################

##Family History Data

#This gathers the measures into one based on variables that should match up
gather_paces_data <- function(structure_t2, static_dem, apa_depression, family_hx) {
  structure_t2 %>%
    filter(session_id == 'ses-V02') %>%
    select(-session_id) %>%
    left_join(
      static_dem %>%
        filter(session_id == 'ses-V02') %>%
        select(-session_id)
    ) %>%
    left_join(
      apa_depression %>%
        filter(session_id == 'ses-V01') %>%
        select(-session_id)
    ) %>%
    left_join(
      family_hx %>%
        filter(session_id == 'ses-V01') %>%
        select(-session_id)
    ) %>%
    left_join(
      paces %>%     #change to paces
        filter(session_id == 'ses-V01') %>%
        select(-session_id)
    )
}



#This makes the final data frame
family_hx_data <- gather_paces_data(
  structure_t2,
  static_dem,
  apa_depression,
  family_hx
)


#Save the combined data frame

#Fanily HX Data
arrow::write_parquet(family_hx_data, "data/processed/Protective_Factors_Data_Family_HX_2_16MAR2026.parquet")

































#########################################################################################

##Medication History Data

#This gathers the measures into one based on variables that should match up
gather_paces_data <- function(structure_t2, static_dem, apa_depression, medication) {
  structure_t2 %>%
    filter(session_id == 'ses-V02') %>%
    select(-session_id) %>%
    left_join(
      static_dem %>%
        filter(session_id == 'ses-V02') %>%
        select(-session_id)
    ) %>%
    left_join(
      apa_depression %>%
        filter(session_id == 'ses-V01') %>%
        select(-session_id)
    ) %>%
    left_join(
      medication %>%
        filter(session_id == 'ses-V01') %>%
        select(-session_id)
    ) %>%
    left_join(
      paces %>%     #change to paces
        filter(session_id == 'ses-V01') %>%
        select(-session_id)
    )
}



#This makes the final data frame
medication_data <- gather_paces_data(
  structure_t2,
  static_dem,
  apa_depression,
  medication
)


#Save the combined data frame

#Medication Data
arrow::write_parquet(medication_data, "data/processed/Protective_Factors_Data_Medication_2_18MAR2026.parquet")


















####################################################################################
#This gathers the measures into one based on variables that should match up
gather_paces_data <- function(structure_t2, static_dem, apa_depression, quality_control_mri) {
  structure_t2 %>%
    filter(session_id == 'ses-V02') %>%
    select(-session_id) %>%
    left_join(
      static_dem %>%
        filter(session_id == 'ses-V02') %>%
        select(-session_id)
    ) %>%
    left_join(
      apa_depression %>%
        filter(session_id == 'ses-V01') %>%  #change depending on post or prenatal depression
        select(-session_id)
    ) %>%
    left_join(
      quality_control_mri %>%
        filter(session_id == 'ses-V02') %>%
        select(-session_id)
    ) %>%
    left_join(
      paces %>%     #change to paces
        filter(session_id == 'ses-V01') %>%
        select(-session_id)
    )
}



#This makes the final data frame
paces_data <- gather_paces_data(
  structure_t2,
  static_dem,
  apa_depression,
  quality_control_mri
)


#Save the combined data frame
##PACES Data
#arrow::write_parquet(paces_data, "data/processed/Protective_Factors_Data_2_17Feb2026.parquet")

#PROMIS Data
arrow::write_parquet(paces_data, "data/processed/Protective_Factors_Data_APA_Quality_Control_2_08APR2026.parquet")





