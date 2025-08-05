###############################################
### Read census data and write to GeoJSON file
##############################################


library(cancensus)
library(dplyr)
library(geojsonsf)
library(here)
library(magrittr)
library(remotes)
library(reticulate)
library(rsconnect)
library(sf)
library(tidyverse)


cache_dir <- here("cancensus_cache")
# Create the directory if it doesn't exist
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
  message(paste("Created cancensus cache directory at:", cache_dir))
}
# Set the cache path option for cancensus for THIS project
options(cancensus.cache_path = cache_dir)
options(cancensus.api_key = "CensusMapper_4157198e649c5a47d5759a6f6be9214a")


# Defines vector and assings names variables of interest
census_vars <- c(
  pop_Tot ="v_CA16_401",	#Total	Population, 2016
  dwell_Tot = "v_CA16_404",	#Total private dwellings
  land_area = "v_CA16_407",	#Total	Land area in square kilometres
  dwell_type_Smpl = "v_CA16_408", #	Total private dwellings	Number
  dwell_type_SingDet = "v_CA16_409",	#Total	Single-detached house
  hh_inc_MedIncHHBefTax = "v_CA16_2397",	#Total	Median total income of households in 2015 ($)
  hh_inc_MedIncHHAftTax = "v_CA16_2398",	#Total	Median after-tax income of households in 2015 ($)
  dwell_char_AvgRoomsSmpl = "v_CA16_4855",	#Total	Average number of rooms per dwelling
  dwell_fin_NumbHHSmpl = "v_CA16_4890",	#Total	Total - Owner households in non-farm, non-reserve private dwellings - 25% sample data
  dwell_fin_MedValueOfDwellingSmpl = "v_CA16_4895", #Total	Median value of dwellings ($)
  dwell_fin_AvgValueOfDwellingSmpl = "v_CA16_4896", #Total	Average value of dwellings ($)
  hh_educ_TotHighestDegreeSmpl = "v_CA16_5051",	#Total	Total - Highest certificate, diploma or degree for
  hh_educ_Bach = "v_CA16_5081",	#Total	Bachelor's degree
  hh_educ_AboveBach = "v_CA16_5084",	#Total	University certificate or diploma above bachelor level
  travel_dist_TotDestSmpl = "v_CA16_5777",	#Total	Total - Commuting destination for the employed labour force aged 15 years and over in private households with a usual place of work - 25% sample data
  traveL_dist_WithinCSDSmpl = "v_CA16_5780",	#Total	Commute within census subdivision (CSD) of residence
  traveL_dist_DiffCSDSameCDSmpl = "v_CA16_5783",	#Total	Commute to a different census subdivision (CSD) within census division (CD) of residence
  traveL_dist_DiffCSDDiffCDSmpl = "v_CA16_5786",	#otal	Commute to a different census subdivision (CSD) and census division (CD) within province or territory of residence
  traveL_dist_DiffProvSmpl = "v_CA16_5789",	#Total	Commute to a different province or territory
  traveL_mode_TotModeSmpl = "v_CA16_5792",	#Total	Total - Main mode of commuting for the employed labour force aged 15 years
  #and over in private households with a usual place of work or no fixed workplace address - 25% sample data
  traveL_mode_CarEtcDriverSmpl = "v_CA16_5795",	#Total	Car, truck, van - as a driver
  traveL_mode_CarEtcPassSmpl = "v_CA16_5798",	#Total	Car, truck, van - as a passenger
  traveL_mode_PublicTransSmpl = "v_CA16_5801",	#Total	Public transit
  traveL_time_TotCommuteTimeSmpl = "v_CA16_5813",	#Total	Total - Commuting duration for the employed labour force aged 15 years and over in
  #private households with a usual place of work or no fixed workplace address - 25% sample data
  traveL_time_Less15Smpl = "v_CA16_5816",	#Total	Less than 15 minutes
  traveL_time_15to29Smpl = "v_CA16_5819",	#Total	15 to 29 minutes
  traveL_time_30to44Smpl= "v_CA16_5822",	#Total	30 to 44 minutes
  traveL_time_45to59Smpl = "v_CA16_5825",	#Total	45 to 59 minutes
  travel_time_60PlusSmpl = "v_CA16_5828"	#Total	60 minutes and over
)

# Retrieves census data and writes to file
name_prov <- c("BC", "ON")
num_prov <- c(59, 35)
prov_df <- data.frame(cbind(name_prov, num_prov)) %>%
  setNames(c("prov_name", "prov_num"))

select_prov <- "ON"
select_name <- prov_df$prov_name[which(prov_df$prov_name == select_prov)]
select_num <- prov_df$prov_num[which(prov_df$prov_name == select_prov)]
census_df <- get_census(dataset='CA16', regions=list(PR=select_num),
                        vectors=census_vars, level='CT', quiet = TRUE, geo_format = 'sf', labels = 'short')
census_df$CTUID <- as.character(census_df$GeoUID)


file_name <- here("data", paste0("census_df_", select_prov, ".geojson"))

# Create the 'data' directory if it doesn't exist
if (!dir.exists(here("data"))) {
  dir.create(here("data"), recursive = TRUE)
  message(paste("Created data directory at:", here("data")))
}

sf::st_write(census_df, file_name, driver = "GeoJSON")
