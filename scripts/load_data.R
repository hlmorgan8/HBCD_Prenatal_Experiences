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



##This is loading in data from your data dictionary
###If you are in scratch space use this:  "#/#/phenotype/"

load_data <- function(fname, base_path = "#/#/2.0/#/#/"){
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
pregnancy_history <- load_data("pex_bm_health_preg__healthhx.parquet")
infant_health <- load_data("pex_bm_healthv2_inf.parquet")
child_dem <- load_data("sed_bm_demo_child.parquet")



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
#arrow::write_parquet(paces_data, "folder/here/###.parquet")

#PROMIS Data
#arrow::write_parquet(paces_data, "folder/here/###.parquet")







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
arrow::write_parquet(edinburgh_data, "folder/here/###.parquet")











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
#arrow::write_parquet(paces_data, "folder/here/###.parquet")

#PROMIS Data
arrow::write_parquet(paces_data, "folder/here/###.parquet")



































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
arrow::write_parquet(family_hx_data, "folder/here/###.parquet")

































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
arrow::write_parquet(medication_data, "folder/here/###.parquet")


















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
#arrow::write_parquet(paces_data, "folder/here/###.parquet")

#PROMIS Data
arrow::write_parquet(paces_data, "folder/here/###.parquet")



























######################################################################################
##
##                    DATA FRAMES FOR SENSITIVITY ANALYSES
##
######################################################################################






#########################################################################################

##Parity data

#This gathers the measures into one based on variables that should match up
gather_paces_data <- function(structure_t2, static_dem, apa_depression, pregnancy_history) {
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
      pregnancy_history %>%
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
  pregnancy_history
)


#Save the combined data frame

#Parity Data
arrow::write_parquet(family_hx_data, "folder/here/###.parquet")


























#########################################################################################

##Birthweight from Health Inf V2 data

#This gathers the measures into one based on variables that should match up
gather_paces_data <- function(structure_t2, static_dem, apa_depression, pregnancy_history, infant_health) {
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
      pregnancy_history %>%
        filter(session_id == 'ses-V01') %>%
        select(-session_id)
    ) %>%
    left_join(
      infant_health %>%
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
infant_health_data <- gather_paces_data(
  structure_t2,
  static_dem,
  apa_depression,
  pregnancy_history,
  infant_health
)


#Save the combined data frame

#Parity Data
arrow::write_parquet(infant_health_data, "folder/here/###.parquet")





















####Check child demographics for birth parent%
table(child_dem$sed_bm_demo_child__relat_001)


