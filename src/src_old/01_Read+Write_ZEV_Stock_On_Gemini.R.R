# --- 1. Package Management and Loading ---

# It's good practice to separate package installation from loading.
# Only run `install.packages()` if a package is not already installed.
# Using pacman::p_load is a highly recommended approach for
# efficient package loading and installation if needed.
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(
  dplyr, geos, geojsonio, geojsonsf, ggplot2, here, htmltools,
  magrittr, purrr, Rcpp, readxl, rsconnect, sf, terra, tidyverse
)

# --- 2. Configuration and Setup ---

# Define city data clearly
cities_data <- tribble(
  ~city_name, ~CMA_UID, ~SAC_code, ~pop_2021,
  "Tor",      35535,    535,       6202,
  "Ott",      505,      505,       1017,
  "Ham",      35537,    537,       785.2,
  "KCW",      35541,    541,       575.8,
  "Lon",      35555,    555,       543.6,
  "SCN",      35539,    539,       433.6,
  "Win",      35559,    559,       422.6,
  "Osh",      35532,    532,       415.3
) %>%
  mutate(across(c(CMA_UID, SAC_code, pop_2021), as.numeric)) # Ensure numeric types

# Choice of city - use a named list for clear configuration
config <- list(
  selected_city_name = "Tor",
  ontario_pop_2021_thousands = 14225, # Ontario population in thousands for 2021
  ontario_ldv_registrations_2021 = 796079, # Ontario LDV new registrations 2021
  stock_lifetime_years = 12 # Years for ZEV stock calculation
)

# Derive selected city's codes
selected_city <- cities_data %>%
  filter(city_name == config$selected_city_name)

if (nrow(selected_city) == 0) {
  stop(paste("Selected city '", config$selected_city_name, "' not found in cities_data."))
}

select_CMA_UID <- selected_city$CMA_UID
select_SAC_code <- selected_city$SAC_code

# Create the 'data' directory if it doesn't exist
data_dir <- here("data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  message(paste("Created data directory at:", data_dir))
}

# --- 3. Data Loading and Initial Processing ---

# Retrieves census data and filters data for selected city
# Use `read_sf` for geojson files directly.
census_df <- read_sf(file.path(data_dir, "census_df_ON.geojson")) %>%
  filter(CMA_UID == select_CMA_UID) %>%
  na.omit() # Consider if `na.omit` is always appropriate here

# Creates CTUID-to-geography correspondence table
# Select and rename in one step for clarity
CTUID_geo_map <- census_df %>%
  select(CTUID = GeoUID, geometry) %>%
  st_drop_geometry() # If geometry is not needed for mapping, drop it early

# Reads in population forecast
pop_frcst <- read_csv(file.path(data_dir, "ON_pop_projections.csv"))

# Reads in dataset of EVs
fleet_df <- read_csv(file.path(data_dir, "CT_ZEV_Fleet_ON.csv")) %>%
  na.omit() %>% # Consider if `na.omit` is appropriate here too
  mutate(CTUID = sprintf(CTUID, fmt = '%#.2f')) # Consistent formatting for CTUID

# Selects city in EV dataset and selects total ZEVs (BEV+PHEV)
ZEV_df <- fleet_df %>%
  filter(SAC_code == select_SAC_code, fuel_type == "ZEV") %>%
  select(year, CTUID, value)

# --- 4. ZEV Stock Calculation (Historical) ---

# Produces spread form of dataset with years as columns containing values
# Use `complete` from tidyr to handle missing year-CTUID rows efficiently.
# Then `pivot_wider` for spreading.
ZEV_stock_wide <- ZEV_df %>%
  complete(year, CTUID, fill = list(value = 0)) %>% # Fill missing with 0
  pivot_wider(names_from = year, values_from = value, names_prefix = "y")

# Join with geography map
ZEV_stock <- left_join(CTUID_geo_map, ZEV_stock_wide, by = "CTUID")

# Add CTs with no ZEV data and assign stock of 1 in year 2021, 0 for other years
# This logic might need re-evaluation. If a CT has *no* ZEV data, `complete` might
# not add it unless `CTUID_geo_map` is used as the base for `complete`.
# Assuming `ZEV_stock` now contains all CTUIDs from `CTUID_geo_map`.
# This section looks like it's trying to fill `NA`s in specific year columns
# after the join. It's better to ensure completeness *before* the join or
# handle NAs more explicitly.
# Let's simplify and rely on `complete` and then `mutate(across(...))` for NAs.

# Extract year columns and convert to numeric for calculations
year_cols_ZEV_stock <- paste0("y", 2017:2021) # Define the range of years for operation

# Handle NAs after joining if they represent missing historical data
# Ensure CTUIDs from CTUID_geo_map are all present.
# It's safer to ensure all CTUIDs from CTUID_geo_map are in ZEV_stock_wide *before* spreading
# or filling NAs, then merge.
all_ct_years <- expand_grid(CTUID = CTUID_geo_map$CTUID, year = min(ZEV_df$year):max(ZEV_df$year))

ZEV_stock_processed <- ZEV_df %>%
  full_join(all_ct_years, by = c("CTUID", "year")) %>%
  mutate(value = replace_na(value, 0)) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "y") %>%
  left_join(CTUID_geo_map, ., by = "CTUID") # Ensure CTUID_geo_map is the base

# Re-evaluating the '1 for 2021, 0 for others' rule:
# If a CT had no ZEVs prior to 2021, and this is truly the rule:
ZEV_stock <- ZEV_stock_processed %>%
  mutate(
    y2021 = ifelse(is.na(y2021), 1, y2021), # If 2021 is NA, set to 1
    across(paste0("y", 2017:2020), ~ ifelse(is.na(.), 0, .)) # If earlier years are NA, set to 0
  )

# Calculate CT's share of ZEV stock and population
census_pop <- census_df %>%
  select(CTUID = GeoUID, Population) %>%
  st_drop_geometry() %>%
  mutate(Population = as.numeric(Population)) # Ensure numeric type

ZEV_stock <- left_join(ZEV_stock, census_pop, by = "CTUID") %>%
  mutate(
    share_pop = Population / sum(Population, na.rm = TRUE),
    share_ZEV = .data[[paste0("y", 2021)]] / sum(.data[[paste0("y", 2021)]], na.rm = TRUE)
  )

# Short name for future use
name_CTUID <- ZEV_stock$CTUID
number_CTUID <- nrow(ZEV_stock)

# --- 5. ZEV City Sales (Historical) ---

# Generates historical annual ZEV city sales from differences in stocks,
# except for first year, which is that year's stock
# Use `rowwise` or `mutate` with `lag` for more readable calculations
# Define years more robustly
historical_sales_years <- 2017:2021
forecast_sales_years <- 2022:2046
all_sales_years <- min(historical_sales_years):max(forecast_sales_years)

# Create ZEV_city_sales as a tibble
ZEV_city_sales <- tibble(year = all_sales_years) %>%
  mutate(sales = 0)

# Calculate initial ZEV city sales based on ZEV_stock
# Aggregate ZEV_stock by year
city_historical_stock <- ZEV_stock %>%
  select(CTUID, starts_with("y")) %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year_str",
    names_prefix = "y",
    values_to = "stock"
  ) %>%
  mutate(year = as.numeric(year_str)) %>%
  group_by(year) %>%
  summarise(total_stock = sum(stock, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(year)

# Calculate sales as difference
city_historical_sales <- city_historical_stock %>%
  mutate(sales = total_stock - lag(total_stock, default = 0)) %>%
  filter(year %in% historical_sales_years) %>% # Filter to relevant historical years
  select(year, sales)

# Update ZEV_city_sales with historical data
ZEV_city_sales <- ZEV_city_sales %>%
  left_join(city_historical_sales, by = "year", suffix = c(".orig", "")) %>%
  mutate(sales = coalesce(sales, sales.orig)) %>% # Use the calculated sales
  select(-sales.orig)

# Set the 2017 sales to the 2017 stock sum
ZEV_city_sales <- ZEV_city_sales %>%
  mutate(sales = ifelse(year == 2017, city_historical_stock$total_stock[city_historical_stock$year == 2017], sales))

# Creates CT-level ZEV sales dataframe (historical initialization)
ZEV_CT_sales_init <- tibble(CTUID = name_CTUID)

for (yr in historical_sales_years) {
  col_name <- paste0("y", yr)
  city_sales_for_year <- ZEV_city_sales$sales[ZEV_city_sales$year == yr]
  ZEV_CT_sales_init[[col_name]] <- ZEV_stock$share_ZEV * city_sales_for_year
}

# --- 6. LDV Sales Forecast ---

# Forecast total LDV sales to 2046 using population projection

# Ensure pop_frcst is properly loaded and contains the selected city's population.
# Also, ensure 'year' column exists and is numeric.
pop_frcst <- read_csv(file.path(data_dir, "ON_pop_projections.csv")) %>%
  mutate(year = as.numeric(year)) # Ensure year is numeric for joining/filtering

# Population column for selected city, joined with all forecast years
# It's better to ensure this dataframe contains all necessary years before looping.
selected_city_pop_col <- pop_frcst %>%
  select(year, selected_city_pop = !!sym(config$selected_city_name)) %>%
  # Ensure all forecast years are present, filling missing population if necessary (though a warning is better)
  right_join(tibble(year = all_sales_years), by = "year") %>% # Join with all relevant years
  arrange(year)

# Check for missing population data early
if (any(is.na(selected_city_pop_col$selected_city_pop))) {
  missing_pop_years <- selected_city_pop_col$year[is.na(selected_city_pop_col$selected_city_pop)]
  stop(paste("Missing population data for selected city in years:",
             paste(missing_pop_years, collapse = ", "),
             ". Please check ON_pop_projections.csv"))
}


LDV_city_sales_df <- tibble(year = all_sales_years) %>% # Use all_sales_years for consistency
  mutate(sales = 0)

# Calculate 2021 LDV city sales
ldv_2021_city_sales <- config$ontario_ldv_registrations_2021 *
  (selected_city$pop_2021 / config$ontario_pop_2021_thousands)

LDV_city_sales_df$sales[LDV_city_sales_df$year == 2021] <- ldv_2021_city_sales


# Calculate future LDV sales based on population growth
# Use a dplyr approach with `lag` for cleaner calculation
LDV_city_sales_df <- LDV_city_sales_df %>%
  left_join(selected_city_pop_col, by = "year") %>%
  arrange(year) %>%
  mutate(
    prev_year_pop = lag(selected_city_pop, order_by = year),
    prev_year_sales = lag(sales, order_by = year)
  ) %>%
  # Now, apply the growth factor only for years > 2021
  rowwise() %>% # Apply row-by-row
  mutate(
    sales = ifelse(year > 2021, prev_year_sales * (selected_city_pop / prev_year_pop), sales)
  ) %>%
  ungroup() %>%
  select(year, sales) # Keep only the necessary columns


# Plot results
plot_LDV_city_sales <- LDV_city_sales_df %>%
  ggplot(aes(x = as.factor(year), y = sales)) +
  geom_point() +
  labs(title = "Forecasted LDV City Sales", x = "Year", y = "Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Assigns city-wide LDV sales to CTs based on population share
LDV_CT_sales <- tibble(CTUID = name_CTUID)

# Create columns for all forecast years
for (yr in forecast_sales_years) {
  LDV_CT_sales[[paste0("y", yr)]] <- ZEV_stock$share_pop * LDV_city_sales_df$sales[LDV_city_sales_df$year == yr]
}

# Plot previous using log scale
plot_LDV_CT_sales <- LDV_CT_sales %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year_str",
    names_prefix = "y",
    values_to = "value"
  ) %>%
  mutate(year = as.numeric(year_str)) %>%
  filter(year %in% forecast_sales_years) %>%
  ggplot(aes(x = as.factor(year), y = log(value))) +
  geom_point() +
  labs(title = "Forecasted LDV CT Sales (Log Scale)", x = "Year", y = "Log(Sales)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 7. ZEV Sales Forecast (Iterative Allocation) ---

# ZEV annual sales growth rate is calculated from ZEV sales in 2021
# to LDV sales in 2040
zev_2021_sales <- ZEV_city_sales$sales[ZEV_city_sales$year == 2021]
ldv_2040_sales <- LDV_city_sales_df$sales[LDV_city_sales_df$year == 2040]
years_diff_growth <- 2040 - 2021

if (zev_2021_sales <= 0) {
  warning("ZEV city sales in 2021 are zero or negative. ZEV growth rate calculation skipped.")
  ZEV_growth <- 1 # Default to no growth if base is zero
} else {
  ZEV_growth <- (ldv_2040_sales / zev_2021_sales)^(1 / years_diff_growth)
}

# Calculates annual city ZEV sales to 2040, then to 2046,
# when it's equal to LDV growth
for (yr in forecast_sales_years) {
  if (yr <= 2040) {
    if (yr == 2021) {
      # 2021 sales already set from historical
    } else {
      ZEV_city_sales$sales[ZEV_city_sales$year == yr] <-
        ZEV_city_sales$sales[ZEV_city_sales$year == (yr - 1)] * ZEV_growth
    }
  } else { # Post 2040, ZEV sales equal LDV sales
    ZEV_city_sales$sales[ZEV_city_sales$year == yr] <-
      LDV_city_sales_df$sales[LDV_city_sales_df$year == yr]
  }
}

# Plot previous
plot_ZEV_city_sales <- ZEV_city_sales %>%
  ggplot(aes(x = as.factor(year), y = sales)) +
  geom_point() +
  labs(title = "Forecasted ZEV City Sales", x = "Year", y = "Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Initializes ZEV sales at the CT level for 2017-2021 (already done, just reconfirming)
# ZEV_CT_sales_init already contains these values based on historical calculation.

# Calculates initial ZEV sales forecast at the CT level for 2022-2046
# Extend ZEV_CT_sales_init to include forecast years
ZEV_CT_sales_initial_forecast <- ZEV_CT_sales_init
for (yr in forecast_sales_years) {
  if (yr <= 2040) {
    if (yr == 2021) {
      # 2021 sales are from historical init
    } else {
      prev_col_name <- paste0("y", yr - 1)
      current_col_name <- paste0("y", yr)
      ZEV_CT_sales_initial_forecast[[current_col_name]] <-
        ZEV_CT_sales_initial_forecast[[prev_col_name]] * ZEV_growth
    }
  } else {
    current_col_name <- paste0("y", yr)
    LDV_sales_for_year <- LDV_city_sales_df$sales[LDV_city_sales_df$year == yr]
    ZEV_CT_sales_initial_forecast[[current_col_name]] <- ZEV_stock$share_pop * LDV_sales_for_year
  }
}

# ZEV sales distribution algorithm
ZEV_CT_sales_final <- ZEV_CT_sales_initial_forecast # Start with the initial forecast

# Identify relevant year columns for the loop
forecast_col_names <- paste0("y", forecast_sales_years)

for (iteration_count in 1:6) { # Loop for allocation adjustment
  # Determine where ZEV sales exceed LDV sales
  excess_mask <- ZEV_CT_sales_final[, forecast_col_names] >= LDV_CT_sales[, forecast_col_names]

  # Calculate excess amount and set sales to LDV limit where exceeded
  excess_values <- ifelse(excess_mask, ZEV_CT_sales_final[, forecast_col_names] - LDV_CT_sales[, forecast_col_names], 0)
  ZEV_CT_sales_final[, forecast_col_names] <- ifelse(excess_mask, LDV_CT_sales[, forecast_col_names], ZEV_CT_sales_final[, forecast_col_names])

  # Sum the total excess for each year
  total_excess_per_year <- colSums(excess_values, na.rm = TRUE)

  # Calculate shares for redistribution
  # Only consider CTs that *did not* exceed their LDV limit
  non_exceeding_mask <- !excess_mask

  # Population share of non-exceeding CTs
  pop_non_exceeding <- ZEV_stock$share_pop * non_exceeding_mask

  # Sum of population shares for non-exceeding CTs per year
  sum_pop_non_exceeding_per_year <- colSums(pop_non_exceeding, na.rm = TRUE)

  # Avoid division by zero if no non-exceeding CTs for a year
  sum_pop_non_exceeding_per_year[sum_pop_non_exceeding_per_year == 0] <- 1e-9 # Small non-zero to prevent NaN

  # Calculate redistribution shares
  redistribution_shares <- sweep(pop_non_exceeding, 2, sum_pop_non_exceeding_per_year, "/")

  # Distribute excess sales
  allocated_excess <- sweep(redistribution_shares, 2, total_excess_per_year, "*")

  # Add allocated excess to ZEV sales for non-exceeding CTs
  ZEV_CT_sales_final[, forecast_col_names] <- ZEV_CT_sales_final[, forecast_col_names] + allocated_excess
}

# Checks:
# Compare aggregate city sales before and after adjustment (should be similar)
# ZEV_city_initial_forecast_total <- colSums(ZEV_CT_sales_initial_forecast[, forecast_col_names], na.rm = TRUE)
# ZEV_city_final_total <- colSums(ZEV_CT_sales_final[, forecast_col_names], na.rm = TRUE)
# cbind(ZEV_city_initial_forecast_total, ZEV_city_final_total)

# Check which CT - year combinations max out
# This check should be run *after* the iterative allocation is complete
ratio_max_out <- ZEV_CT_sales_final[, forecast_col_names] / LDV_CT_sales[, forecast_col_names]
ratio_max_out <- round(ratio_max_out, digits = 4)

# Appends LDV (=ZEV) sales post 2040 - this was handled in the forecast loop already
# ZEV_CT_sales_final should already have the correct values for 2041-2046 based on LDV sales
# However, if the iterative loop *reduced* them below the LDV share, this step
# ensures they are brought back up to the population-based LDV share.
for (yr in 2041:2046) {
  col_name <- paste0("y", yr)
  LDV_sales_for_year <- LDV_city_sales_df$sales[LDV_city_sales_df$year == yr]
  ZEV_CT_sales_final[[col_name]] <- ZEV_stock$share_pop * LDV_sales_for_year
}


# Plot individual CT sales (example for a few CTs)
plot_ZEV_CT_sales_final_sample <- ZEV_CT_sales_final %>%
  filter(CTUID %in% head(name_CTUID, 5)) %>% # Plot first 5 CTs
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year_str",
    names_prefix = "y",
    values_to = "value"
  ) %>%
  mutate(year = as.numeric(year_str)) %>%
  filter(year %in% all_sales_years) %>%
  ggplot(aes(x = as.factor(year), y = log(value), color = CTUID, group = CTUID)) +
  geom_line() +
  labs(title = "Forecasted ZEV CT Sales (Log Scale, Sample)", x = "Year", y = "Log(Sales)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot aggregate CT sales
ZEV_city_final_agg <- ZEV_CT_sales_final %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year_str",
    names_prefix = "y",
    values_to = "sales"
  ) %>%
  mutate(year = as.numeric(year_str)) %>%
  group_by(year) %>%
  summarise(ZEV_sales = sum(sales, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year %in% all_sales_years)

ggplot(ZEV_city_final_agg, aes(x = as.factor(year), y = ZEV_sales)) +
  geom_point() +
  geom_line() +
  labs(title = "Aggregate Final ZEV City Sales", x = "Year", y = "ZEV Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 8. ZEV Stock Forecast ---

# Generates CT stock numbers, summing annual sales over 'stock_lifetime_years'
ZEV_CT_stock_for <- tibble(CTUID = name_CTUID)

# For each forecast year, calculate the rolling sum of sales
for (yr in forecast_sales_years) {
  start_year_for_stock <- max(min(all_sales_years), yr - config$stock_lifetime_years + 1)
  years_to_sum <- paste0("y", start_year_for_stock:yr)

  # Check if all columns exist; if not, fill missing with 0 for the sum
  # This makes the sum more robust, assuming sales are 0 for non-existent historical years
  sales_data_for_stock <- ZEV_CT_sales_final %>%
    select(CTUID, any_of(years_to_sum))

  # Add missing year columns with 0 values if they don't exist
  missing_cols <- setdiff(years_to_sum, names(sales_data_for_stock))
  if (length(missing_cols) > 0) {
    sales_data_for_stock[missing_cols] <- 0
  }

  ZEV_CT_stock_for[[paste0("y", yr)]] <- rowSums(sales_data_for_stock %>% select(all_of(years_to_sum)), na.rm = TRUE)
}

# Calculation of city-wide LDV stock
LDV_city_stock_for <- tibble(year = forecast_sales_years) %>%
  mutate(stock = 0)

for (yr in forecast_sales_years) {
  start_year_for_stock <- max(min(all_sales_years), yr - config$stock_lifetime_years + 1)
  years_to_sum <- start_year_for_stock:yr

  # Ensure LDV_city_sales_df covers these years
  ldv_sales_slice <- LDV_city_sales_df %>%
    filter(year %in% years_to_sum)

  LDV_city_stock_for$stock[LDV_city_stock_for$year == yr] <- sum(ldv_sales_slice$sales, na.rm = TRUE)
}

# Plot ZEV city stock forecast
ZEV_city_stock_for_agg <- ZEV_CT_stock_for %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year_str",
    names_prefix = "y",
    values_to = "stock"
  ) %>%
  mutate(year = as.numeric(year_str)) %>%
  group_by(year) %>%
  summarise(ZEV_stock = sum(stock, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year %in% forecast_sales_years) # Only forecast years

ggplot(ZEV_city_stock_for_agg, aes(x = as.factor(year), y = ZEV_stock)) +
  geom_point() +
  geom_line() +
  labs(title = "Aggregate Forecasted ZEV City Stock", x = "Year", y = "ZEV Stock") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 9. Final Output ---

# Gather data frame before joining
ZEV_CT_stock_for_long <- ZEV_CT_stock_for %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year_str",
    names_prefix = "y",
    values_to = "ZEV_stock"
  ) %>%
  mutate(year = as.numeric(year_str)) %>%
  filter(year %in% forecast_sales_years) # Only forecast years

# Binds geography to ZEV df
# Ensure `census_df` has the 'geometry' column for `st_as_sf()`
keepvars <- c("CTUID", "year", "ZEV_stock", "Population", "geometry")

ZEV_stock_for_output <- ZEV_CT_stock_for_long %>%
  inner_join(census_df %>% select(GeoUID, Population, geometry), by = c("CTUID" = "GeoUID")) %>%
  st_as_sf() %>%
  select(all_of(keepvars)) # Use all_of to ensure variables exist

filename <- file.path(data_dir, paste0("ZEV_stock_for_", config$selected_city_name, ".geojson"))
geojson_write(ZEV_stock_for_output, file = filename)

message(paste("ZEV stock forecast saved to:", filename))
