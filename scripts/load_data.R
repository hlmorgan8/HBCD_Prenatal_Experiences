#################################################################################
##                                                                            ##
##                      Protective Factors Project                            ##
##                            Loading Data                                    ##
##                                                                            ##
#################################################################################

################Hannah Morgan 23OCT2025#########################################
##                                                                            ##
################################################################################


#install.packages("dplyr")
#install.packages("arrow")
library(dplyr)
library(arrow)

##This will load into the data directory to grab the data but assumes you have a symbolic link - the below will help you set that up
###to create this, go to Terminal and enter in VACC with your netID
#####cd into your project's raw data folder
######  cd /users/h/m/hmorgan4/HBCD/Protective_Factors_Project/data/raw


##This is loading in data from your data dictionary
###If you are in scratch space use this:  "data/raw/phenotype/"
load_data <- function(fname, base_path = "data/raw/phenotype/"){
  fpath <- paste0(base_path, fname)
  arrow::read_parquet(fpath)
}


#This pulls in your separate measures of interest
structure_t2 <- load_data("img_bibsnet_space-T2w_desc-aseg_volumes.parquet")
paces <- load_data("sed_bm_paces.parquet")
static_dem <- load_data("sed_basic_demographics.parquet")
apa_depression <- load_data("pex_bm_apa.parquet")  


#This gathers the measures into one based on variables that should match up
gather_paces_data <- function(structure_t2, static_dem, apa_depression) {
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
      paces %>%
      filter(session_id == 'ses-V01') %>%
        select(-session_id)
    )
}



#This makes the final data frame
paces_data <- gather_paces_data(
  structure_t2,
  static_dem,
  apa_depression
)


#Save the combined data frame
arrow::write_parquet(paces_data, "data/processed/Protective_Factors_Data_08DEC2025.parquet")




















