#####################################################################
### Retrieves census data and writes to file
#####################################################################


library(here)
library(readxl)
library(tidyverse)

var_series <- c("YEAR", "REGION NAME", "TOTAL")
region_series <- c("YEAR", "TOR", "OTTAWA", "HAMILTON", "WATERLOO",
                   "MIDDLESEX", "NIAGARA", "ESSEX", "DURHAM" )
# Middlesex = London; Essex = Windsor; Durham = Oshawa
city_names <- c("year", "Tor","Ott", "Ham", "KCW", "Lon", "SCN", "Win", "Osh")

# Create the 'data' directory if it doesn't exist, using here()
if (!dir.exists(here("data"))) {
  dir.create(here("data"), recursive = TRUE)
  message(paste("Created data directory at:", here("data")))
}


temp <- read_excel(here("data", "mof_population_projections_2021-2046.xlsx"))
temp <- temp[var_series] %>%
  spread('REGION NAME', TOTAL) %>%
  mutate(TOR = TORONTO + PEEL + HALTON + YORK)
temp <- subset(temp, select = region_series) %>%
  setNames(city_names)

write.csv(temp, file = here("data", "ON_pop_projections.csv"))
