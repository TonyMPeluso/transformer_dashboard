###########################################################################
### Reads and write census data and population projections to geoJSON file
###########################################################################

# --- Libraries ---
library(cancensus)
library(dplyr)
library(geojsonsf)
library(here)
library(readxl)
library(sf)
library(tidyverse)

# --- Directories ---
data_dir <- here("data")
cache_dir <- here("cancensus_cache")

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# Set cancensus options
options(cancensus.cache_path = cache_dir)
options(cancensus.api_key = "CensusMapper_4157198e649c5a47d5759a6f6be9214a")

# --- 1) CENSUS DATA ---
# Variables of interest
census_vars <- c(
  pop_Tot ="v_CA16_401", dwell_Tot = "v_CA16_404", land_area = "v_CA16_407",
  dwell_type_Smpl = "v_CA16_408", dwell_type_SingDet = "v_CA16_409",
  hh_inc_MedIncHHBefTax = "v_CA16_2397", hh_inc_MedIncHHAftTax = "v_CA16_2398",
  dwell_char_AvgRoomsSmpl = "v_CA16_4855", dwell_fin_NumbHHSmpl = "v_CA16_4890",
  dwell_fin_MedValueOfDwellingSmpl = "v_CA16_4895", dwell_fin_AvgValueOfDwellingSmpl = "v_CA16_4896",
  hh_educ_TotHighestDegreeSmpl = "v_CA16_5051", hh_educ_Bach = "v_CA16_5081",
  hh_educ_AboveBach = "v_CA16_5084", travel_dist_TotDestSmpl = "v_CA16_5777",
  traveL_dist_WithinCSDSmpl = "v_CA16_5780", traveL_dist_DiffCSDSameCDSmpl = "v_CA16_5783",
  traveL_dist_DiffCSDDiffCDSmpl = "v_CA16_5786", traveL_dist_DiffProvSmpl = "v_CA16_5789",
  traveL_mode_TotModeSmpl = "v_CA16_5792", traveL_mode_CarEtcDriverSmpl = "v_CA16_5795",
  traveL_mode_CarEtcPassSmpl = "v_CA16_5798", traveL_mode_PublicTransSmpl = "v_CA16_5801",
  traveL_time_TotCommuteTimeSmpl = "v_CA16_5813", traveL_time_Less15Smpl = "v_CA16_5816",
  traveL_time_15to29Smpl = "v_CA16_5819", traveL_time_30to44Smpl= "v_CA16_5822",
  traveL_time_45to59Smpl = "v_CA16_5825", travel_time_60PlusSmpl = "v_CA16_5828"
)

# Province info
prov_df <- tibble(prov_name = c("BC", "ON"), prov_num = c(59, 35))
select_prov <- "ON"
select_num <- prov_df$prov_num[prov_df$prov_name == select_prov]

# Retrieve census data
census_df <- get_census(
  dataset = "CA16", regions = list(PR = select_num),
  vectors = census_vars, level = "CT", quiet = TRUE,
  geo_format = "sf", labels = "short"
) %>%
  mutate(CTUID = as.character(GeoUID))

# Save census data as GeoJSON
geojson_file <- here(data_dir, paste0("census_df_", select_prov, ".geojson"))
st_write(census_df, geojson_file, driver = "GeoJSON", quiet = TRUE)

message("Census data written to: ", geojson_file)

# --- 2) POPULATION PROJECTIONS ---
# Column names
var_series <- c("YEAR", "REGION NAME", "TOTAL")
region_series <- c("YEAR", "TOR", "OTTAWA", "HAMILTON", "WATERLOO",
                   "MIDDLESEX", "NIAGARA", "ESSEX", "DURHAM")
city_names <- c("year", "Tor","Ott", "Ham", "KCW", "Lon", "SCN", "Win", "Osh")

# Read and process projections
proj_file <- here(data_dir, "mof_population_projections_2021-2046.xlsx")
pop_data <- read_excel(proj_file) %>%
  select(all_of(var_series)) %>%
  spread(`REGION NAME`, TOTAL) %>%
  mutate(TOR = TORONTO + PEEL + HALTON + YORK) %>%
  select(all_of(region_series)) %>%
  setNames(city_names)

# Save as CSV
pop_csv <- here(data_dir, "ON_pop_projections.csv")
write_csv(pop_data, pop_csv)

message("Population projections written to: ", pop_csv)
