# install.packages("dplyr")
# install.packages("geos")
# install.packages("geojsonio")
# install.packages("geojsonsf")
# install.packages("ggplot2")
# install.packages("htmltools")
# install.packages("magrittr")
# install.packages("purrr")
# install.packages("Rcpp")
# install.packages("readxl")
# install.packages("rsconnect")
# install.packages("sf")
# install.packages("terra")
# install.packages("tidyverse")

library(dplyr)
library(geos)
library(geojsonio)
library(geojsonsf)
library(ggplot2)
library(htmltools)
library(magrittr)
library(purrr)
# library(Rcpp)
# library(readxl)
library(rsconnect)
library(sf)
library(terra)
library(tidyverse)

# Creates dataframe of Ontario cities and their codes with which
# to select the city whose forecast is being generated
names <- append(c("Van","Vic"),
  c("Tor","Ott", "Ham", "KCW", "Lon", "SCN", "Win", "Osh"))
census <- append(c(59933, 59935),
  c(35535, 505, 35537, 35541, 35555, 35539, 35559, 35532))
ZEV <- append(c(933, 935),
  c(535, 505, 537, 541, 555, 539, 559, 532))
city_pop_2021 <- append(c(2642.8, 397.237),
  c(6202, 1017, 785.2 , 575.8, 543.6, 433.6, 422.6, 415.3))
prov <- append(rep(c("BC"), each = 2), rep(c("ON"), each = 8))
prov_pop_2021 <- append(rep(c(5241.8), each = 2), rep(c(14225), each = 8))
prov_LDV_2019 <- append(rep(c(216031), each = 2), rep(c(796079), each = 8))
# Vehicle sales  Stats Can 20-10-0001-01, also 20-10-0021-01

last_year <- as.integer(2041)

cities_df <- data.frame(cbind(names, census, ZEV, city_pop_2021, prov,
    prov_pop_2021, prov_LDV_2019)) %>%
  setNames(c("city_name", "CMA_UID", "SAC_code", "city_pop_2021", "Province",
    "prov_pop_2021", "prov_LDV_2019"))

# Choice of city and its corresponding CMA_UID / SAC code
select_city <- "Van"
select_CMA_UID <- cities_df$CMA_UID[which(cities_df$city_name == select_city)]
select_SAC_code <- cities_df$SAC_code[which(cities_df$city_name == select_city)]
select_prov <- cities_df$Province[which(cities_df$city_name == select_city)]
# select_city_v <- as.name(select_city)

# Retrieves census data and filters data for selected city
census_name <- paste("data/census_df_", select_prov, ".geojson", sep = "")
census_df <- st_read(census_name) %>%
  filter(CMA_UID == select_CMA_UID) %>%
  na.omit()

# Creates CTUID-to-geography correspondence table
CTUID_geo_map <- subset(census_df, select = c("GeoUID", "geometry")) %>%
  as.data.frame() %>%
  setNames(c("CTUID", "geometry"))

# Reads in population forecast
frcst_name <- paste("data/pop_projections_", select_prov, ".csv", sep = "")
pop_frcst <- read.csv(frcst_name) %>%
  magrittr::extract(c("year", select_city)) %>%
  as.data.frame()
pop_frcst[,2] <- (gsub(",","",pop_frcst[,2])) %>% # Removes comma from number
  as.numeric()


# Reads in dataset of EVs
fleet_name <- paste("data/CT_ZEV_Fleet ", select_prov, ".csv", sep = "")
fleet_df <- read_csv(fleet_name) %>%
  as.data.frame() %>%
  na.omit()
fleet_df$CTUID <- as.character(sprintf(fleet_df$CTUID, fmt = '%#.2f'))

# Selects city in EV dataset and selects total ZEVs (BEV+PHEV)
ZEV_df <- fleet_df %>%
  filter(SAC_code == select_SAC_code & fuel_type == "ZEV") %>%
  subset(select =c(year, CTUID, value))

# Produces spread form of dataset with years as columns containing values
# first by identifying missing year-CTUID rows and assigning values 0 since pre 2021
temp <- anti_join(expand(ZEV_df, year, CTUID), ZEV_df)
missing_df <- as.data.frame(temp, col_number = 2)
if (count(missing_df) != 0) {
  missing_df$value <- 0
}
ZEV_stock <- rbind(ZEV_df, missing_df) %>%
  spread(year, value)

ZEV_stock <- left_join(CTUID_geo_map, ZEV_stock)
# Adds CTs with no ZEV data and assigns stock of 1 in year 2021, 0 for other years
if (count(missing_df) != 0) {
  ZEV_stock <- left_join(CTUID_geo_map, ZEV_stock) %>%
  mutate_at(7, ~replace_na(.,1)) %>%
  mutate_at(3:6, ~replace_na(.,0))
}

# Check on previous when missing year-CTUID pairs
# i <- which(colnames(ZEV_stock) == "2017")
# j <- which(colnames(ZEV_stock) == "2021")
# colSums(ZEV_stock[,i:j])

# Also calculates CT's share of ZEV stock and population,for ZEV stock in 2021
census_pop <- subset(census_df, select = c("GeoUID", "Population")) %>%
  setNames(c("CTUID", "Population", "geometry"))
ZEV_stock <- left_join(ZEV_stock, census_pop, by = "CTUID" )
ZEV_stock$Population <- as.numeric(ZEV_stock$Population)
i <- which(colnames(ZEV_stock) == "2021")
ZEV_stock$share_pop <- ZEV_stock$Population / sum(ZEV_stock$Population)
ZEV_stock$share_ZEV <- ZEV_stock[,i] / sum(ZEV_stock[,i])

# Check previous
sum(ZEV_stock$share_pop)
sum(ZEV_stock$share_ZEV)

# Short name for future use
name_CTUID <- ZEV_stock$CTUID
number_CTUID <- nrow(ZEV_stock)

# Generates historical annual ZEV city sales from differences in stocks,
# except for first year, which is that year's stock
ZEV_city_sales <- as.data.frame(matrix(0.0, nrow=1, ncol=length(2009:last_year)))
names(ZEV_city_sales) <- c(2009:last_year)
i <- which(colnames(ZEV_stock) == "2018") # Equals 4
j <- which(colnames(ZEV_stock) == "2021") # Equals 7
k <- which(colnames(ZEV_city_sales) == "2018") # Equals 4
l <- which(colnames(ZEV_city_sales) == "2021") # Equals 7
ZEV_city_sales[k-1] <- sum(ZEV_stock[,i-1])
ZEV_city_sales[k:l] <- colSums(ZEV_stock[,i:j]) - colSums(ZEV_stock[,(i-1):(j-1)])

# Creates CT-level ZEV sales dataframe and allocates 2017-2021 city-wide
# sales to CTs based on their share of stock (to avoid strange last-year
# sales as a starting point, and to maintain stock sums per CT)
ZEV_CT_sales_init <- as.data.frame(name_CTUID) %>%
  cbind(matrix(0, length(number_CTUID), length(2009:last_year))) %>%
  setNames(c("CTUID", 2009:last_year))
j <- which(colnames(ZEV_CT_sales_init) == "2017") # Equals 10
k <- which(colnames(ZEV_CT_sales_init) == "2021") # Equals 14
l <- which(names(ZEV_city_sales) == "2017")
for (i in j:k) {
  ZEV_CT_sales_init[,i] <- ZEV_stock$share_ZEV * as.double(ZEV_city_sales[l-j+i])
}

# Check on previous
l <- which(names(ZEV_city_sales) == "2017")
m <- which(names(ZEV_city_sales) == "2021")
colSums(ZEV_CT_sales_init[,j:k]) - ZEV_city_sales[l:m]

# Forecast total LDV sales to last_year using population projection
# using Ottawa's share of 2021 of provincial LDV new registrations (1/15)
# correcting for 2% population growth from 2019 to 2021 (1.02)
# population Ont 14,225k in 2021, LDV new registrations 796,079
LDV_city_sales <- as.data.frame(matrix(0, nrow=1, ncol=length(2021:last_year))) %>%
  setNames(2021:last_year)
LDV_city_sales$'2021' <-
  as.numeric(cities_df$prov_LDV_2019[which(cities_df$city_name == select_city)]) *
  as.numeric(cities_df$city_pop_2021[which(cities_df$city_name == select_city)]) /
  as.numeric(cities_df$prov_pop_2021[which(cities_df$city_name == select_city)])
city <- which(colnames(pop_frcst) == select_city)
j <- which(names(LDV_city_sales) == "2022") # Equals 4
k <- which(names(LDV_city_sales) == last_year) # Equals 4
for (i in j:k) {
  LDV_city_sales[,i] <- LDV_city_sales[,i-1] * as.numeric(pop_frcst[i, city]) /
    as.numeric(pop_frcst[i - 1, city])
}

# Plot results
plot_LDV_city_sales <- select(LDV_city_sales, c(j:k)) %>%
  gather(year, value) %>%
  ggplot(aes(x = year, y = value)) + geom_point()

# Assigns city-wide LDV sales to CTs based on population share
LDV_CT_sales <- as.data.frame(name_CTUID) %>%
  cbind(matrix(0, number_CTUID, length(2021:last_year))) %>%
  setNames(c("CTUID", 2021:last_year))
j <- which(colnames(LDV_CT_sales) == "2022") # Equals 4
k <- which(colnames(LDV_CT_sales) == last_year) # Equals 4
l <- which(names(LDV_city_sales) == "2022") # Equals 2
m <- which(names(LDV_city_sales) == last_year) # Equals 2
n <- which(colnames(ZEV_stock) == "share_pop")
LDV_CT_sales[,j:k] <- as.matrix(ZEV_stock[,n]) %*%
  as.matrix(LDV_city_sales[l:m])

# Check on previous
colSums(LDV_CT_sales[,j:k]) - LDV_city_sales[l:m]

# Plot previous using log scale
o <- which(colnames(LDV_CT_sales) == "2022")
p <- which(colnames(LDV_CT_sales) == last_year)
plot_LDV_CT_sales <- select(LDV_CT_sales, c(o:p)) %>%
  gather(year, value) %>%
  ggplot(aes(x = year, y = log(value))) + geom_point()

# ZEV annual sales growth rate is calculated from ZEV sales in 2021
# to LDV sales in 2040
i <- which(names(ZEV_city_sales) == "2021")
j <- which(names(LDV_city_sales) == "2040")
ZEV_growth <- as.double((LDV_city_sales[j] / ZEV_city_sales[i])^(1/(2040-2021)))

# Calculates annual city ZEV sales to 2040, then to last_year,
# when it's equal to LDV growth
j <- which(names(ZEV_city_sales) == "2022")
k <- which(names(ZEV_city_sales) == "2040")
for (i in j:k){
  ZEV_city_sales[,i] <- ZEV_city_sales[i-1]*ZEV_growth
}
i <- which(names(ZEV_city_sales) == "2041")
j <- which(names(ZEV_city_sales) == last_year)
k <- which(names(LDV_city_sales) == "2041")
l <- which(names(LDV_city_sales) == last_year)
ZEV_city_sales[i:j] <- LDV_city_sales[k:l]

# Plot previous
plot_ZEV_city_sales <- ZEV_city_sales %>%
  gather(year, value) %>%
  ggplot(aes(x = year, y = value)) + geom_point()

# Initializes ZEV sales at the CT level for 20017-2021, using CT ZEV shares
# to avoid unusual 2021 sales numbers
i <- which(colnames(ZEV_CT_sales_init) == "2017")
j <- which(colnames(ZEV_CT_sales_init) == "2021")
k <- which(names(ZEV_city_sales) == "2017")
l <- which(names(ZEV_city_sales) == "2021")
m <- which(colnames(ZEV_stock) == "share_ZEV")
ZEV_CT_sales_init[,i:j] <- as.matrix(ZEV_stock[,m]) %*%
  as.matrix(ZEV_city_sales[k:l])

# Check on previous by column, 2017-2021
colSums(as.matrix(ZEV_stock[,m]))
round(colSums(ZEV_CT_sales_init[,i:j]) - ZEV_city_sales[k:l])
# Check on previous, but by row
round(rowSums(ZEV_CT_sales_init[1:number_CTUID,i:j]) - ZEV_stock$`2021`)

# Calculates initial ZEV sales forecast at the CT level
j <- which(colnames(ZEV_CT_sales_init) == "2022") # Equals 15
k <- which(colnames(ZEV_CT_sales_init) == "2040") # Equals 39
for (i in j:k) {
  ZEV_CT_sales_init[,i] <- ZEV_CT_sales_init[,i-1] * ZEV_growth
}
i <- which(names(ZEV_city_sales) == "2041")
j <- which(names(ZEV_city_sales) == last_year)
k <- which(names(LDV_city_sales) == "2041")
l <- which(names(LDV_city_sales) == last_year)
ZEV_city_sales[i:j] <- LDV_city_sales[k:l]

# Initializes TRUE and FALSE matrices to identify CT / year
# combinations for which ZEV sales exceed LDV limit
excess_true <- as.data.frame(name_CTUID) %>%
  cbind(matrix(data = TRUE, number_CTUID, length(2022:last_year))) %>%
  setNames(c("CTUID", 2022:last_year))
excess_false <- as.data.frame(name_CTUID) %>%
  cbind(matrix(data = FALSE, number_CTUID, length(2022:last_year))) %>%
  setNames(c("CTUID", 2022:last_year))

# Initializes final ZEV sales dataframe
ZEV_CT_sales_final <- as.data.frame(name_CTUID) %>%
  cbind(replicate(length(2009:last_year), numeric(number_CTUID))) %>%
  setNames(c("CTUID", 2009:last_year))

# Prepares iteration over true (excess sales) and false matrices
# depending on whether ZEV sales exceed LDV sales at CT level
ZEV_CT_sales_temp <- ZEV_CT_sales_init
counter <- 1
while (counter <= 6) {
  i <- which(colnames(ZEV_CT_sales_temp) == "2022") # Equals 4
  j <- which(colnames(ZEV_CT_sales_temp) == last_year) # Equals 4
  k <- which(colnames(LDV_CT_sales) == "2022") # Equals 2
  l <- which(colnames(LDV_CT_sales) == last_year) # Equals 2
  excess_true <- ifelse(ZEV_CT_sales_temp[,i:j] >= LDV_CT_sales[,k:l], 1, 0)
  excess_false <- ifelse(ZEV_CT_sales_temp[,i:j] >= LDV_CT_sales[,k:l], 0, 1)

# Initializes matrix of excess sums
  excess_sum <-as.data.frame(matrix(data = 0, nrow = 1, ncol = length(2022:last_year))) %>%
    setNames(c(2022:last_year))

# Sets exceeded values to their maximum, and column-sums excess over maximum
  i <- which(colnames(ZEV_CT_sales_final) == "2022")
  j <- which(colnames(ZEV_CT_sales_final) == last_year)
  k <- which(colnames(LDV_CT_sales) == "2022")
  l <- which(colnames(LDV_CT_sales) == last_year)
  m <- which(colnames(ZEV_CT_sales_temp) == "2022")
  n <- which(colnames(ZEV_CT_sales_temp) == last_year)
  ZEV_CT_sales_final[,i:j] <- excess_true * LDV_CT_sales[,k:l] +
    excess_false * ZEV_CT_sales_temp[,m:n]
  excess_sum <- matrix(colSums(excess_true * (ZEV_CT_sales_temp[,m:n] -
    ZEV_CT_sales_final[,i:j])))

# Sums up population for each year of those who are below max sales
# Then finds share of that pop for each CT to which excess is assigned
  pop_inverse <-  1/colSums(excess_false*ZEV_stock$share_pop)
  excess_share <- (ZEV_stock$share_pop %*%  t(pop_inverse)) *
    excess_false

  # Allocates excess sales to CTs below max sales, "excess_sales" dataframe
  excess_sum_matrix <- matrix(rep(excess_sum, each=number_CTUID), nrow=number_CTUID)
  excess_sales <- as.data.frame(name_CTUID) %>%
    cbind(matrix(0, number_CTUID,length(2022:last_year))) %>%
    setNames(c("CTUID", 2022:last_year))
  i <- which(colnames(excess_sales) == "2022")
  j <- which(colnames(excess_sales) == last_year)
  k <- which(colnames(ZEV_CT_sales_final) == "2022")
  l <- which(colnames(ZEV_CT_sales_final) == last_year)
  m <- which(colnames(ZEV_CT_sales_temp) == "2022")
  n <- which(colnames(ZEV_CT_sales_temp) == last_year)
  excess_sales[,i:j] <- excess_share * excess_sum_matrix
  ZEV_CT_sales_final[,k:l] <- excess_true * ZEV_CT_sales_final[,k:l] +
    excess_false*(ZEV_CT_sales_temp[,m:n] + excess_sales[,i:j])

  ZEV_CT_sales_temp <- ZEV_CT_sales_final

  counter <- counter + 1
}

# Checks calculation of excess
i <- which(colnames(ZEV_CT_sales_init) == "2022")
j <- which(colnames(ZEV_CT_sales_init) == last_year)
k <- which(colnames(ZEV_CT_sales_final) == "2022")
l <- which(colnames(ZEV_CT_sales_final) == last_year)
m <- which(colnames(ZEV_CT_sales_temp) == "2022")
n <- which(colnames(ZEV_CT_sales_temp) == last_year)
round(colSums(ZEV_CT_sales_temp[,k:l]) - colSums(ZEV_CT_sales_init[,i:j]))

# Checks, but PROBLEM since ZEV_city sales don't taper off post 2040
o <- which(colnames(ZEV_city_sales) == "2022")
p <- which(colnames(ZEV_city_sales) == last_year)
round(colSums(ZEV_CT_sales_temp[m:n]) - ZEV_city_sales[o:p])

# Checks which CT - year combinations max out
i <- which(colnames(LDV_CT_sales) == "2022")
j <- which(colnames(LDV_CT_sales) == last_year)
k <- which(colnames(ZEV_CT_sales_temp) == "2022")
l <- which(colnames(ZEV_CT_sales_temp) == last_year)
ratio <- round(ZEV_CT_sales_temp[,k:l] / LDV_CT_sales[,i:j], digits = 4)

# Appends LDV (=ZEV) sales post 2040
i <- which(colnames(LDV_city_sales) == "2041")
j <- which(colnames(LDV_city_sales) == last_year)
k <- which(colnames(ZEV_CT_sales_temp) == "2041")
l <- which(colnames(ZEV_CT_sales_temp) == last_year)
ZEV_CT_sales_temp[order(ZEV_CT_sales_temp$CTUID),]
ZEV_stock[order(ZEV_stock$share_pop),]
ZEV_CT_sales_temp[,k:l] <-  as.matrix(ZEV_stock$share_pop) %*%
  as.matrix(LDV_city_sales[i:j])

# Plot individual CT sales
i <- which(colnames(ZEV_CT_sales_temp) == "2022")
i <- which(colnames(ZEV_CT_sales_temp) == last_year)
plot_ZEV_CT_sales_temp <- select(ZEV_CT_sales_temp, c(i:j)) %>%
  gather(year, value) %>%
  ggplot(aes(x = year, y = log(value))) + geom_point()

# Tests break point at 2040 - 2041
i <- which(colnames(ZEV_CT_sales_temp) == "2040")
j <- which(colnames(ZEV_CT_sales_temp) == "2041")
temp <- round(ZEV_CT_sales_temp[,j] / ZEV_CT_sales_temp[,i], digits = 4)

# Plot aggregate CT sales
i <- which(colnames(ZEV_CT_sales_temp) == "2022")
j <- which(colnames(ZEV_CT_sales_temp) == last_year)
ZEV_city_final <- data.frame(ZEV_sales=colSums(ZEV_CT_sales_temp[,i:j])) %>%
  rownames_to_column(var = "year")
ggplot(ZEV_city_final, aes(x=year, y=ZEV_sales))  + geom_point()

# Generates CT stock numbers, summing annual sales over 12 years,
# first initializing matrix
ZEV_CT_stock_for <- as.data.frame(name_CTUID) %>%
  cbind(replicate(length(2022:last_year), numeric(number_CTUID))) %>%
  setNames(c("CTUID", 2022:last_year))
j <- which(colnames(ZEV_CT_stock_for) == "2022") #Equals 2
k <- which(colnames(ZEV_CT_stock_for) == last_year) #Equals 26
l <- which(colnames(ZEV_CT_sales_temp) == "2022") # Equals 15
for (i in j:k) {
  m <- l - j + i - 12
  n <- l - j + i
  ZEV_CT_stock_for[,i] <- rowSums(ZEV_CT_sales_temp[m:n])
}

# Calculation of city-wide LDV stock
LDV_city_stock_for <- as.data.frame(matrix(0, nrow=1, ncol=length(2030:last_year)))
colnames(LDV_city_stock_for) <- c(2030:last_year)
j <- which(colnames(LDV_city_stock_for) == "2035") #Equals 2
k <- which(colnames(LDV_city_stock_for) == last_year) #Equals 26
l <- which(colnames(LDV_city_sales) == "2035") # Equals 15
for (i in j:k) {
  m <- l-j+i-12
  n <- l-j+i
  LDV_city_stock_for[i] <- sum(LDV_city_sales[m:n])
}

# Plot previous
ZEV_city_stock_for <- data.frame(ZEV_stock=colSums(ZEV_CT_stock_for[j:k])) %>%
  rownames_to_column(var = "year")
ggplot(ZEV_city_stock_for, aes(x=year, y=ZEV_stock))  + geom_point()

# Gather data frame before joining
i <- which(colnames(ZEV_CT_stock_for) == "2022")
j <- which(colnames(ZEV_CT_stock_for) == last_year)
ZEV_CT_stock_for <- gather(ZEV_CT_stock_for, key="year", value="ZEV_stock", i:j)

# Binds geography to ZEV df,
keepvars <- c("CTUID", "year", "ZEV_stock", "Population", "geometry")
ZEV_stock_for <- inner_join(ZEV_CT_stock_for, census_df, by = c("CTUID" = "GeoUID")) %>%
    st_as_sf() %>%
    subset(select=keepvars)

filename <- paste("data/ZEV_stock_for_", select_city, ".geojson", sep = "")
geojson_write(ZEV_stock_for, file = filename)



