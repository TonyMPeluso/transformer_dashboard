###############################################################
### Writes ZEV Stock and LDV Sales Forecast for Ontario Cities
##############################################################


if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(
  dplyr, geos, geojsonio, geojsonsf, ggplot2, here, htmltools,
  magrittr, purrr, Rcpp, readxl, rsconnect, sf, terra, tidyverse,
  readr # Ensure tidyverse is loaded for dplyr, ggplot2, etc.
)

message("--- Packages loaded ---")

# Define city data
cities_data <- tribble(
  ~city_name, ~CMA_UID, ~SAC_code, ~pop_2021,
  "Tor",       35535,      535,        6202,
  "Ott",       505,        505,        1017,
  "Ham",       35537,      537,        785.2,
  "KCW",       35541,      541,        575.8,
  "Lon",       35555,      555,        543.6,
  "SCN",       35539,      539,        433.6,
  "Win",       35559,      559,        422.6,
  "Osh",       35532,      532,        415.3
) %>%
  mutate(across(c(CMA_UID, SAC_code, pop_2021), as.numeric)) # Ensure numeric types

message("City metadata defined.")
print(str(cities_data))

# Choice of city - use a named list for clear configuration
config <- list(
  selected_city_name = "Tor",
  ontario_pop_2021_thousands = 14225, # Ontario population in thousands for 2021
  ontario_ldv_registrations_2021 = 796079, # Ontario LDV new registrations 2021
  stock_lifetime_years = 12 # Years for ZEV stock calculation
)

message("Configuration set.")
print(str(config))

# Derive selected city's codes
selected_city <- cities_data %>%
  filter(city_name == config$selected_city_name)

if (nrow(selected_city) == 0) {
  stop(paste("Selected city '", config$selected_city_name, "' not found in cities_data."))
}

select_CMA_UID <- selected_city$CMA_UID
select_SAC_code <- selected_city$SAC_code

message(paste("Selected city details: CMA_UID =", select_CMA_UID, ", SAC_code =", select_SAC_code))

# Create the 'data' directory if it doesn't exist
data_dir <- here("data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  message(paste("Created data directory at:", data_dir))
}

# Retrieves census data and filters data for selected city
# Use `read_sf` for geojson files directly.
message(paste("Loading census data from:", file.path(data_dir, "census_df_ON.geojson")))
census_df <- read_sf(file.path(data_dir, "census_df_ON.geojson"), quiet = TRUE) %>%
  filter(CMA_UID == select_CMA_UID) %>%
  na.omit() # Consider if `na.omit` is always appropriate here

message("Census data loaded and filtered.")
print(str(census_df))

# Creates CTUID-to-geography correspondence table
# Select and rename in one step for clarity
CTUID_geo_map <- census_df %>%
  select(CTUID = GeoUID) %>%
  st_drop_geometry() # Drop geometry if not needed for the map, keep it in census_df for final output

message("CTUID-to-geography map created (no geometry).")
print(str(CTUID_geo_map))

# Reads in population forecast
message(paste("Loading population forecast from:", file.path(data_dir, "ON_pop_projections.csv")))
pop_frcst <- read_csv(file.path(data_dir, "ON_pop_projections.csv"), show_col_types = FALSE) %>%
  mutate(year = as.numeric(year)) # Ensure year is numeric for joining/filtering

message("Raw population forecast loaded.")
print(str(pop_frcst))

# Get 2021 population for the selected city from selected_city (derived from cities_data)
city_2021_pop_k <- selected_city$pop_2021

# Create a tibble for years 2017-2020 with the 2021 population data
# Use !!sym() to correctly set the column name dynamically
missing_pop_years_data <- tibble(
  year = 2017:2020,
  !!sym(config$selected_city_name) := city_2021_pop_k
)

# Bind rows to pop_frcst. This adds the missing historical years.
pop_frcst <- bind_rows(missing_pop_years_data, pop_frcst) %>%
  arrange(year) # Ensure years are in order

message("Population forecast padded with 2017-2020 data.")
print(str(pop_frcst))

# Reads in dataset of EVs
message(paste("Loading ZEV fleet data from:", file.path(data_dir, "CT_ZEV_Fleet_ON.csv")))
fleet_df <- read_csv(file.path(data_dir, "CT_ZEV_Fleet_ON.csv"), show_col_types = FALSE) %>%
  na.omit() %>% # Consider if `na.omit` is appropriate here too
  mutate(CTUID = sprintf(CTUID, fmt = '%#.2f')) # Consistent formatting for CTUID

message("ZEV fleet data loaded.")
print(str(fleet_df))


# Selects city in EV dataset and selects total ZEVs (BEV+PHEV)
ZEV_df <- fleet_df %>%
  filter(SAC_code == select_SAC_code, fuel_type == "ZEV") %>%
  select(year, CTUID, value)

message("Filtered ZEV data for selected city.")
print(str(ZEV_df))

# ZEV Stock Calculation (Historical) ---
#
# Define all relevant years for historical and forecast data
min_zev_data_year <- min(ZEV_df$year, na.rm = TRUE) # Should be 2017 based on data
historical_sales_years <- min_zev_data_year:2021
forecast_sales_years <- 2022:2046
all_sales_years <- min(historical_sales_years):max(forecast_sales_years)

message(paste("Defined years: historical_sales_years =", paste(historical_sales_years, collapse = ", ")))
message(paste("Defined years: forecast_sales_years =", paste(forecast_sales_years, collapse = ", ")))
message(paste("Defined years: all_sales_years =", paste(all_sales_years, collapse = ", ")))


# Create a complete grid of all CTUIDs and all relevant years
all_ct_years_grid <- expand_grid(CTUID = CTUID_geo_map$CTUID, year = all_sales_years)

# Combine ZEV_df with the full grid, fill missing values with 0
ZEV_stock_processed <- ZEV_df %>%
  full_join(all_ct_years_grid, by = c("CTUID", "year")) %>%
  mutate(value = replace_na(value, 0)) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "y")

message("ZEV stock data processed to wide format.")
print(str(ZEV_stock_processed))

# Join with CTUID_geo_map (which has no geometry, just CTUIDs to ensure all are present)
# Then apply specific rules for ZEV stock (1 for 2021 if NA, 0 for 2017-2020 if NA)
# This assumes that CTUID_geo_map contains all CTUIDs for the selected city.
ZEV_stock <- ZEV_stock_processed %>%
  left_join(CTUID_geo_map, ., by = "CTUID") %>%
  mutate(
    # Rule: If y2021 is NA (meaning no ZEV data for this CTUID in 2021), set to 1.
    y2021 = ifelse(is.na(y2021), 1, y2021),
    # Rule: If earlier years (y2017-y2020) are NA, set to 0.
    # This `across` applies to all columns starting with 'y' in the specified range.
    across(starts_with("y") & matches(paste0("^(", paste(2017:2020, collapse = "|"), ")$")),
           ~ ifelse(is.na(.), 0, .))
  )

message("ZEV stock joined with map and rules applied.")
print(str(ZEV_stock))


# Calculate CT's share of ZEV stock and population
census_pop <- census_df %>%
  select(CTUID = GeoUID, Population) %>%
  st_drop_geometry() %>%
  mutate(Population = as.numeric(Population)) # Ensure numeric type

message("Census population extracted.")
print(str(census_pop))

ZEV_stock <- left_join(ZEV_stock, census_pop, by = "CTUID") %>%
  mutate(
    share_pop = Population / sum(Population, na.rm = TRUE),
    share_ZEV = .data[[paste0("y", 2021)]] / sum(.data[[paste0("y", 2021)]], na.rm = TRUE)
  )

message("ZEV stock with population and ZEV shares calculated.")
print(str(ZEV_stock))

# Short name for future use
name_CTUID <- ZEV_stock$CTUID
number_CTUID <- nrow(ZEV_stock)

message(paste("Number of CTUIDs:", number_CTUID))
# --- NEW: Early check for zero CTUIDs ---
if (number_CTUID == 0) {
  stop(paste("No CTUIDs found for selected CMA_UID:", select_CMA_UID, ". Please check census_df_ON.geojson and your selected CMA_UID."))
}


# ZEV City Sales (Historical) ---
#
# Aggregate ZEV_stock by year to get city-wide historical stock
city_historical_stock <- ZEV_stock %>%
  select(CTUID, starts_with("y")) %>% # Select CTUID and all year columns
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

message("City-wide historical ZEV stock aggregated.")
print(str(city_historical_stock))

# Create ZEV_city_sales tibble for all years (historical and forecast)
ZEV_city_sales <- tibble(year = all_sales_years) %>%
  mutate(sales = 0)

# Calculate historical sales as difference from stock
city_historical_sales_calc <- city_historical_stock %>%
  mutate(sales = total_stock - lag(total_stock, default = 0)) %>%
  filter(year %in% historical_sales_years) %>% # Filter to relevant historical years
  select(year, sales)

# Update ZEV_city_sales with historical data
ZEV_city_sales <- ZEV_city_sales %>%
  left_join(city_historical_sales_calc, by = "year", suffix = c(".orig", "")) %>%
  mutate(sales = coalesce(sales, sales.orig)) %>% # Use the calculated sales
  select(-sales.orig)

# Set the 2017 sales to the 2017 stock sum as per provided logic
# Ensure 2017 stock is available
stock_2017_val <- city_historical_stock$total_stock[city_historical_stock$year == 2017]
if (length(stock_2017_val) == 1) {
  ZEV_city_sales <- ZEV_city_sales %>%
    mutate(sales = ifelse(year == 2017, stock_2017_val, sales))
} else {
  warning("2017 stock data not found for initial ZEV city sales adjustment.")
}


message("ZEV city historical sales calculated.")
print(str(ZEV_city_sales))

# Creates CT-level ZEV sales dataframe (historical initialization)
ZEV_CT_sales_init <- tibble(CTUID = name_CTUID)

for (yr in historical_sales_years) {
  col_name <- paste0("y", yr)
  city_sales_for_year <- ZEV_city_sales$sales[ZEV_city_sales$year == yr]
  ZEV_CT_sales_init[[col_name]] <- ZEV_stock$share_ZEV * city_sales_for_year
}

message("ZEV CT historical sales initialized.")
print(str(ZEV_CT_sales_init))

# LDV Sales Forecast
#
# Population column for selected city, joined with all forecast years
selected_city_pop_col <- pop_frcst %>%
  select(year, selected_city_pop = !!sym(config$selected_city_name)) %>%
  # Ensure all forecast years are present, filling missing population if necessary (though a warning is better)
  right_join(tibble(year = all_sales_years), by = "year") %>% # Join with all relevant years
  arrange(year)

message("Selected city population data prepared.")
print(str(selected_city_pop_col))

# Check for missing population data early
if (any(is.na(selected_city_pop_col$selected_city_pop))) {
  missing_pop_years <- selected_city_pop_col$year[is.na(selected_city_pop_col$selected_city_pop)]
  stop(paste("Missing population data for selected city in years:",
             paste(missing_pop_years, collapse = ", "),
             ". Please check ON_pop_projections.csv or historical population padding logic."))
}

LDV_city_sales_df <- tibble(year = all_sales_years) %>% # Use all_sales_years for consistency
  mutate(sales = 0)

# Calculate 2021 LDV city sales
ldv_2021_city_sales <- config$ontario_ldv_registrations_2021 *
  (selected_city$pop_2021 / config$ontario_pop_2021_thousands)

LDV_city_sales_df$sales[LDV_city_sales_df$year == 2021] <- ldv_2021_city_sales

message(paste("LDV 2021 city sales calculated:", ldv_2021_city_sales))


# Calculate future LDV sales based on population growth
LDV_city_sales_df <- LDV_city_sales_df %>%
  left_join(selected_city_pop_col, by = "year") %>%
  arrange(year) %>%
  mutate(
    prev_year_pop = lag(selected_city_pop, order_by = year),
    prev_year_sales = lag(sales, order_by = year)
  ) %>%
  # Apply the growth factor only for years > 2021
  rowwise() %>%
  mutate(
    sales = ifelse(year > 2021 & !is.na(prev_year_pop) & prev_year_pop != 0,
                   prev_year_sales * (selected_city_pop / prev_year_pop), sales)
  ) %>%
  ungroup() %>%
  select(year, sales) # Keep only the necessary columns

message("LDV city sales forecasted.")
print(str(LDV_city_sales_df))

# Plot results
plot_LDV_city_sales <- LDV_city_sales_df %>%
  ggplot(aes(x = as.factor(year), y = sales)) +
  geom_point() +
  labs(title = paste("Forecasted LDV City Sales for", config$selected_city_name), x = "Year", y = "Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

message("Plotting LDV city sales...")
print(plot_LDV_city_sales) # Explicitly print plot
ggsave(here("output", paste0("LDV_city_sales_", config$selected_city_name, ".png")), plot = plot_LDV_city_sales)
message("LDV city sales plot saved.")


# Assigns city-wide LDV sales to CTs based on population share
LDV_CT_sales <- tibble(CTUID = name_CTUID)

# Create columns for all forecast years
for (yr in all_sales_years) { # Iterate over all relevant years, not just forecast_sales_years
  LDV_CT_sales[[paste0("y", yr)]] <- ZEV_stock$share_pop * LDV_city_sales_df$sales[LDV_city_sales_df$year == yr]
}
message("LDV CT sales assigned.")
print(str(LDV_CT_sales))

# Plot previous using log scale
plot_LDV_CT_sales <- LDV_CT_sales %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year_str",
    names_prefix = "y",
    values_to = "value"
  ) %>%
  mutate(year = as.numeric(year_str)) %>%
  filter(year %in% all_sales_years) %>% # Filter by all_sales_years for consistency
  ggplot(aes(x = as.factor(year), y = log(value))) +
  geom_point() +
  labs(title = paste("Forecasted LDV CT Sales (Log Scale) for", config$selected_city_name), x = "Year", y = "Log(Sales)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

message("Plotting LDV CT sales (log scale)...")
print(plot_LDV_CT_sales)
ggsave(here("output", paste0("LDV_CT_sales_log_", config$selected_city_name, ".png")), plot = plot_LDV_CT_sales)
message("LDV CT sales log plot saved.")

# ZEV Sales Forecast (Iterative Allocation)
# ZEV annual sales growth rate is calculated from ZEV sales in 2021
# to LDV sales in 2040
zev_2021_sales <- ZEV_city_sales$sales[ZEV_city_sales$year == 2021]
ldv_2040_sales <- LDV_city_sales_df$sales[LDV_city_sales_df$year == 2040]
years_diff_growth <- 2040 - 2021

message(paste("ZEV 2021 Sales:", zev_2021_sales))
message(paste("LDV 2040 Sales:", ldv_2040_sales))


if (zev_2021_sales <= 0) {
  warning("ZEV city sales in 2021 are zero or negative. ZEV growth rate calculation skipped.")
  ZEV_growth <- 1 # Default to no growth if base is zero
} else {
  ZEV_growth <- (ldv_2040_sales / zev_2021_sales)^(1 / years_diff_growth)
}
message(paste("Calculated ZEV_growth:", ZEV_growth))

# Calculates annual city ZEV sales to 2040, then to 2046
# This loop updates the 'sales' column in ZEV_city_sales dataframe
for (yr in all_sales_years) {
  if (yr < 2021) {
    # Historical sales already set
  } else if (yr <= 2040) {
    if (yr == 2021) {
      # 2021 sales already set from historical init or explicitly above
    } else {
      # Ensure prev_col_name exists before calculating
      prev_yr_sales <- ZEV_city_sales$sales[ZEV_city_sales$year == (yr - 1)]
      if (length(prev_yr_sales) == 1 && !is.na(prev_yr_sales)) {
        ZEV_city_sales$sales[ZEV_city_sales$year == yr] <- prev_yr_sales * ZEV_growth
      } else {
        warning(paste("Missing or NA previous year sales for ZEV city forecast year:", yr, ". Setting to 0."))
        ZEV_city_sales$sales[ZEV_city_sales$year == yr] <- 0
      }
    }
  } else { # Post 2040, ZEV sales equal LDV sales
    ZEV_city_sales$sales[ZEV_city_sales$year == yr] <-
      LDV_city_sales_df$sales[LDV_city_sales_df$year == yr]
  }
}

message("ZEV city sales forecasted and adjusted post-2040.")
print(str(ZEV_city_sales))

# Plot previous
plot_ZEV_city_sales <- ZEV_city_sales %>%
  ggplot(aes(x = as.factor(year), y = sales)) +
  geom_point() +
  labs(title = paste("Forecasted ZEV City Sales for", config$selected_city_name), x = "Year", y = "Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

message("Plotting ZEV city sales forecast...")
print(plot_ZEV_city_sales)
ggsave(here("output", paste0("ZEV_city_sales_forecast_", config$selected_city_name, ".png")), plot = plot_ZEV_city_sales)
message("ZEV city sales forecast plot saved.")

# Calculates initial ZEV sales forecast at the CT level for 2022-2046
# Extend ZEV_CT_sales_init to include forecast years
ZEV_CT_sales_initial_forecast <- ZEV_CT_sales_init # Already has historical sales from ZEV_stock$share_ZEV

for (yr in forecast_sales_years) {
  current_col_name <- paste0("y", yr)
  prev_col_name <- paste0("y", yr - 1)

  if (yr <= 2040) {
    # This block handles ZEV growth phase up to 2040
    if (prev_col_name %in% names(ZEV_CT_sales_initial_forecast)) {
      ZEV_CT_sales_initial_forecast[[current_col_name]] <-
        ZEV_CT_sales_initial_forecast[[prev_col_name]] * ZEV_growth
    } else {
      warning(paste("Missing previous year column", prev_col_name, "for ZEV_CT_sales_initial_forecast. Setting", current_col_name, "to 0."))
      ZEV_CT_sales_initial_forecast[[current_col_name]] <- 0 # Or handle appropriately
    }
  } else { # Post 2040, CT ZEV sales equal LDV sales based on population share
    LDV_sales_for_year <- LDV_city_sales_df$sales[LDV_city_sales_df$year == yr]
    ZEV_CT_sales_initial_forecast[[current_col_name]] <- ZEV_stock$share_pop * LDV_sales_for_year
  }
}

message("ZEV CT sales initial forecast calculated.")
print(str(ZEV_CT_sales_initial_forecast))

# ZEV sales distribution algorithm (Iterative capping and redistribution)
ZEV_CT_sales_final <- ZEV_CT_sales_initial_forecast # Start with the initial forecast

# Identify relevant year columns for the loop (only forecast years)
forecast_col_names <- paste0("y", forecast_sales_years)

message("Starting iterative ZEV sales distribution algorithm...")
for (iteration_count in 1:6) { # Loop for allocation adjustment
  message(paste("Iteration:", iteration_count))

  # Ensure data frames are aligned (important for direct column access)
  current_zev_sales_matrix <- as.matrix(ZEV_CT_sales_final[, forecast_col_names, drop = FALSE])
  current_ldv_sales_matrix <- as.matrix(LDV_CT_sales[, forecast_col_names, drop = FALSE])

  message("Debug: Dimensions of current_zev_sales_matrix:")
  print(dim(current_zev_sales_matrix))
  message("Debug: Class of current_zev_sales_matrix:")
  print(class(current_zev_sales_matrix))
  message("Debug: Dimensions of current_ldv_sales_matrix:")
  print(dim(current_ldv_sales_matrix))
  message("Debug: Class of current_ldv_sales_matrix:")
  print(class(current_ldv_sales_matrix))

  # Determine where ZEV sales exceed LDV sales
  excess_mask <- current_zev_sales_matrix >= current_ldv_sales_matrix
  message("Debug: Dimensions of excess_mask:")
  print(dim(excess_mask))
  message("Debug: Class of excess_mask:")
  print(class(excess_mask))


  # Calculate excess amount and set sales to LDV limit where exceeded
  excess_values <- ifelse(excess_mask, current_zev_sales_matrix - current_ldv_sales_matrix, 0)
  message("Debug: Dimensions of excess_values (after ifelse):")
  print(dim(excess_values))
  message("Debug: Class of excess_values (after ifelse):")
  print(class(excess_values))

  ZEV_CT_sales_final[, forecast_col_names] <- ifelse(excess_mask, current_ldv_sales_matrix, current_zev_sales_matrix)

  # Sum the total excess for each year
  total_excess_per_year <- colSums(excess_values, na.rm = TRUE)
  message(paste("Total excess for year y2022 (sample):", total_excess_per_year["y2022"]))


  # Calculate shares for redistribution
  # Only consider CTs that *did not* exceed their LDV limit
  non_exceeding_mask <- !excess_mask
  message("Debug: Dimensions of non_exceeding_mask:")
  print(dim(non_exceeding_mask))
  message("Debug: Class of non_exceeding_mask:")
  print(class(non_exceeding_mask))

  # Population share of non-exceeding CTs
  message("Debug: Length of ZEV_stock$share_pop:")
  print(length(ZEV_stock$share_pop))
  message("Debug: Class of ZEV_stock$share_pop:")
  print(class(ZEV_stock$share_pop))
  message("Debug: Checking for NAs, Infs, NaNs in ZEV_stock$share_pop:")
  print(summary(ZEV_stock$share_pop))
  if (any(is.na(ZEV_stock$share_pop)) || any(is.infinite(ZEV_stock$share_pop)) || any(is.nan(ZEV_stock$share_pop))) {
    warning("ZEV_stock$share_pop contains NA, Inf, or NaN values. This might impact calculations.")
  }


  pop_non_exceeding <- ZEV_stock$share_pop * non_exceeding_mask
  message("Debug: Dimensions of pop_non_exceeding:")
  print(dim(pop_non_exceeding))
  message("Debug: Class of pop_non_exceeding:")
  print(class(pop_non_exceeding))
  message("Debug: Checking for NAs, Infs, NaNs in pop_non_exceeding:")
  print(summary(c(pop_non_exceeding))) # Flatten for summary
  if (any(is.na(pop_non_exceeding)) || any(is.infinite(pop_non_exceeding)) || any(is.nan(pop_non_exceeding))) {
    warning("pop_non_exceeding contains NA, Inf, or NaN values. This might impact calculations.")
  }

  # Create a clean, explicit numeric matrix copy of pop_non_exceeding
  # This tries to remove any hidden attributes or state that might be confusing colSums
  pop_non_exceeding_clean <- as.matrix(pop_non_exceeding)
  storage.mode(pop_non_exceeding_clean) <- "numeric" # Ensure numeric storage mode

  message("Debug: Dimensions of pop_non_exceeding_clean:")
  print(dim(pop_non_exceeding_clean))
  message("Debug: Class of pop_non_exceeding_clean:")
  print(class(pop_non_exceeding_clean))
  message("Debug: Is pop_non_exceeding_clean identical to pop_non_exceeding? (attributes only)")
  print(identical(attributes(pop_non_exceeding_clean), attributes(pop_non_exceeding))) # Check if attributes changed

  # Sum of population shares for non-exceeding CTs per year
  sum_pop_non_exceeding_per_year <- colSums(pop_non_exceeding_clean, na.rm = TRUE)
  message("Debug: colSums on pop_non_exceeding_clean successful.") # Debug message after the call
  # --- END CRITICAL FIX ATTEMPT ---


  # Avoid division by zero if no non-exceeding CTs for a year
  sum_pop_non_exceeding_per_year[sum_pop_non_exceeding_per_year == 0] <- 1e-9 # Small non-zero to prevent NaN

  # Calculate redistribution shares
  redistribution_shares <- sweep(pop_non_exceeding, 2, sum_pop_non_exceeding_per_year, "/")

  # Distribute excess sales
  allocated_excess <- sweep(redistribution_shares, 2, total_excess_per_year, "*")

  # Add allocated excess to ZEV sales for non-exceeding CTs
  ZEV_CT_sales_final[, forecast_col_names] <- ZEV_CT_sales_final[, forecast_col_names] + allocated_excess

  # Check if any significant excess still exists
  # Aggregate ZEV city forecast from ZEV_city_sales for comparison
  total_zev_city_forecasted <- ZEV_city_sales %>%
    filter(year %in% forecast_sales_years) %>%
    pull(sales) %>%
    sum(na.rm = TRUE) # Sum of the total city-wide ZEV forecast

  # Sum of all sales in the ZEV_CT_sales_final after this iteration
  current_total_ct_sales <- sum(ZEV_CT_sales_final[, forecast_col_names], na.rm = TRUE)

  # Explicitly convert to matrix for remaining_excess_sum calculation
  zev_sales_for_convergence <- as.matrix(ZEV_CT_sales_final[, forecast_col_names, drop = FALSE])
  ldv_sales_for_convergence <- as.matrix(LDV_CT_sales[, forecast_col_names, drop = FALSE])

  excess_for_convergence <- ifelse(zev_sales_for_convergence >= ldv_sales_for_convergence,
                                   zev_sales_for_convergence - ldv_sales_for_convergence, 0)

  remaining_excess_sum <- sum(colSums(excess_for_convergence, na.rm = TRUE))

  if (remaining_excess_sum < 0.01 * total_zev_city_forecasted) { # if remaining excess is very small compared to total forecast
    message(paste("Converged after", iteration_count, "iterations. Remaining excess sum:", remaining_excess_sum))
    break
  }
}

if (iteration_count >= 6) { # Check if the loop finished without breaking
  warning("Capping iterations reached maximum without full convergence. Results may still contain some excess.")
}

message("ZEV sales distribution algorithm complete.")
print(str(ZEV_CT_sales_final))

# Appends LDV (=ZEV) sales post 2040 - this was handled in the forecast loop already
# However, if the iterative loop *reduced* them below the LDV share, this step
# ensures they are brought back up to the population-based LDV share.
# This should happen *after* the iterative adjustment for years 2041-2046
# to ensure they strictly follow LDV sales.
for (yr in 2041:2046) {
  col_name <- paste0("y", yr)
  LDV_sales_for_year <- LDV_city_sales_df$sales[LDV_city_sales_df$year == yr]
  ZEV_CT_sales_final[[col_name]] <- ZEV_stock$share_pop * LDV_sales_for_year
}
message("ZEV sales post-2040 adjusted to follow LDV shares.")


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
  labs(title = paste("Forecasted ZEV CT Sales (Log Scale, Sample) for", config$selected_city_name), x = "Year", y = "Log(Sales)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

message("Plotting sample ZEV CT sales (log scale)...")
print(plot_ZEV_CT_sales_final_sample)
ggsave(here("output", paste0("ZEV_CT_sales_final_sample_", config$selected_city_name, ".png")), plot = plot_ZEV_CT_sales_final_sample)
message("Sample ZEV CT sales log plot saved.")

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
  labs(title = paste("Aggregate Final ZEV City Sales for", config$selected_city_name), x = "Year", y = "ZEV Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

message("Plotting aggregate final ZEV city sales...")
print(last_plot()) # Print the last generated ggplot
ggsave(here("output", paste0("ZEV_CT_sales_agg_final_", config$selected_city_name, ".png")), plot = last_plot())
message("Aggregate final ZEV city sales plot saved.")

# --- 8. ZEV Stock Forecast ---

# Generates CT stock numbers, summing annual sales over 'stock_lifetime_years'
ZEV_CT_stock_for <- tibble(CTUID = name_CTUID)

message("Generating ZEV CT stock forecast...")

# Pre-process ZEV_CT_sales_final once into a matrix for efficient subsetting
# This ensures all 'yYYYY' columns for 'all_sales_years' are present
# and are numeric.
ZEV_CT_sales_matrix_all_years <- ZEV_CT_sales_final %>%
  select(CTUID, paste0("y", all_sales_years)) %>% # Select all relevant year columns
  column_to_rownames("CTUID") %>%
  as.matrix()

# Ensure the matrix is purely numeric (important for rowSums)
storage.mode(ZEV_CT_sales_matrix_all_years) <- "numeric"

# For each forecast year, calculate the rolling sum of sales
for (yr in all_sales_years) {
  start_year_for_stock <- max(min(all_sales_years), yr - config$stock_lifetime_years + 1)
  years_to_sum_str <- paste0("y", start_year_for_stock:yr)

  # Select relevant sales data directly from the pre-processed matrix
  # This guarantees all necessary columns exist as long as they were in all_sales_years
  # and that they are numeric. 'drop = FALSE' ensures it remains a matrix even if only one column.
  sales_slice_matrix <- ZEV_CT_sales_matrix_all_years[, years_to_sum_str, drop = FALSE]

  # Sum rows for stock calculation
  ZEV_CT_stock_for[[paste0("y", yr)]] <- rowSums(sales_slice_matrix, na.rm = TRUE)
}
message("ZEV CT stock forecast generated.")
print(str(ZEV_CT_stock_for))


# Calculation of city-wide LDV stock
LDV_city_stock_for <- tibble(year = all_sales_years) %>% # Calculate for all_sales_years
  mutate(stock = 0)

message("Generating LDV city stock forecast...")
for (yr in all_sales_years) {
  start_year_for_stock <- max(min(all_sales_years), yr - config$stock_lifetime_years + 1)
  years_to_sum_num <- start_year_for_stock:yr

  # Ensure LDV_city_sales_df covers these years
  ldv_sales_slice <- LDV_city_sales_df %>%
    filter(year %in% years_to_sum_num)

  LDV_city_stock_for$stock[LDV_city_stock_for$year == yr] <- sum(ldv_sales_slice$sales, na.rm = TRUE)
}
message("LDV city stock forecast generated.")
print(str(LDV_city_stock_for))

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
  filter(year %in% all_sales_years) # Only relevant years for plotting

ggplot(ZEV_city_stock_for_agg, aes(x = as.factor(year), y = ZEV_stock)) +
  geom_point() +
  geom_line() +
  labs(title = paste("Aggregate Forecasted ZEV City Stock for", config$selected_city_name), x = "Year", y = "ZEV Stock") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

message("Plotting aggregate forecasted ZEV city stock...")
print(last_plot())
ggsave(here("output", paste0("ZEV_city_stock_forecast_agg_", config$selected_city_name, ".png")), plot = last_plot())
message("Aggregate forecasted ZEV city stock plot saved.")

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
  filter(year %in% all_sales_years) # Ensure only relevant years

message("Preparing final ZEV stock for output...")
print(str(ZEV_CT_stock_for_long))

# Binds geography to ZEV df
# Ensure `census_df` has the 'geometry' column for `st_as_sf()`
# Also need to get population into this final output
ZEV_stock_for_output <- ZEV_CT_stock_for_long %>%
  # Join with census_df to get original geometry and population for the GeoUID (CTUID)
  inner_join(census_df %>% select(GeoUID, Population, geometry), by = c("CTUID" = "GeoUID")) %>%
  mutate(Population = as.numeric(Population)) %>% # Ensure population is numeric
  st_as_sf() # Convert to sf object

keepvars <- c("CTUID", "year", "ZEV_stock", "Population", "geometry") # Define variables to keep

ZEV_stock_for_output <- ZEV_stock_for_output %>%
  select(all_of(keepvars)) # Use all_of to ensure variables exist

message("Final ZEV stock data for output prepared as SF object.")
print(str(ZEV_stock_for_output))

filename <- file.path(data_dir, paste0("ZEV_stock_for_", config$selected_city_name, ".geojson"))
message(paste("Saving ZEV stock forecast to:", filename))
geojson_write(ZEV_stock_for_output, file = filename)

message("--- Script Finished ---")
