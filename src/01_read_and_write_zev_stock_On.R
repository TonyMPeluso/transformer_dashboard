###################################################################################
### Produces GeoJSON file of ZEV stocks and associated geography for Ontario cities
###################################################################################

library(fs)
library(geojsonio)
library(here)
library(sf)
library(tidyverse)

# --- Configuration (for Toronto) ---
config <- list(
  selected_city = "Tor",
  ont_pop_2021k = 14225,
  ont_ldv_2021 = 796079,
  lifetime = 12
)
data_dir <- dir_create(here("data"))
output_dir <- dir_create(here("output"))

# --- Load Data ---
cities <- tribble(
  ~city, ~CMA_UID, ~SAC, ~pop2021,
  "Tor",35535,535,6202,"Ott",505,505,1017,"Ham",35537,537,785.2,
  "KCW",35541,541,575.8,"Lon",35555,555,543.6,"SCN",35539,539,433.6,
  "Win",35559,559,422.6,"Osh",35532,532,415.3
)
sel <- filter(cities, city == config$selected_city)
stopifnot(nrow(sel) == 1)

census <- read_sf(here(data_dir, "census_df_ON.geojson")) %>% filter(CMA_UID == sel$CMA_UID)
pop_proj <- read_csv(here(data_dir, "ON_pop_projections.csv"))
fleet <- read_csv(here(data_dir, "CT_ZEV_Fleet_ON.csv")) %>% filter(SAC_code == sel$SAC & fuel_type == "ZEV")

# --- Population and LDV Sales ---
city_pop <- pop_proj %>%
  mutate(year = as.numeric(year)) %>%
  right_join(tibble(year = 2017:2046), by="year") %>%
  mutate(!!config$selected_city := replace_na(.data[[config$selected_city]], sel$pop2021))

ldv_sales <- city_pop %>%
  mutate(sales = 0,
         sales = ifelse(year==2021,
                        config$ont_ldv_2021*(sel$pop2021/config$ont_pop_2021k),
                        NA)) %>%
  fill(sales, .direction="down") %>%
  mutate(sales = ifelse(year>2021, lag(sales)*( .data[[config$selected_city]]/lag(.data[[config$selected_city]])), sales))

# --- ZEV Sales ---
zev_hist <- fleet %>% group_by(year) %>% summarise(stock=sum(value), .groups="drop")
zev_sales <- zev_hist %>%
  mutate(sales = stock - lag(stock, default=0)) %>%
  bind_rows(tibble(year=2022:2046, sales=NA)) %>% arrange(year)

growth <- (ldv_sales$sales[ldv_sales$year==2040] / zev_sales$sales[zev_sales$year==2021])^(1/19)
zev_sales <- zev_sales %>%
  mutate(sales = ifelse(year>2021 & year<=2040,
                        zev_sales$sales[zev_sales$year==2021]*growth^(year-2021),
                        sales),
         sales = ifelse(year>2040, ldv_sales$sales[match(year,ldv_sales$year)], sales))

# --- Stock Calculations ---
rolling_sum <- function(x, n) map_dbl(seq_along(x), ~sum(x[max(1,.x-n+1):.x], na.rm=TRUE))
zev_stock <- zev_sales %>% mutate(stock = rolling_sum(sales, config$lifetime))
ldv_stock <- ldv_sales %>% mutate(stock = rolling_sum(sales, config$lifetime))

# --- Plots ---
p_ldv <- ggplot(ldv_sales, aes(year, sales)) + geom_line() + labs(title="LDV Sales")
ggsave(here(output_dir, paste0("LDV_sales_", config$selected_city, ".png")), plot = p_ldv)

p_zev <- ggplot(zev_sales, aes(year, sales)) + geom_line() + labs(title="ZEV Sales")
ggsave(here(output_dir, paste0("ZEV_sales_", config$selected_city, ".png")), plot = p_zev)

p_zev_stock <- ggplot(zev_stock, aes(year, stock)) + geom_line() + labs(title="ZEV Stock")
ggsave(here(output_dir, paste0("ZEV_stock_", config$selected_city, ".png")), plot = p_zev_stock)


# --- GeoJSON Output ---
geojson_write(census, file = here(data_dir, paste0("ZEV_stock_", config$selected_city, ".geojson")))

message("--- Forecast complete ---")

