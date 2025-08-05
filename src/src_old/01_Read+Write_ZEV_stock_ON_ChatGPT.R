# Package Installation (Run once, or manage with renv)
# install.packages("dplyr")
# install.packages("geos") # Not strictly needed if not doing GEOS-specific ops directly
# install.packages("geojsonio")
# install.packages("geojsonsf") # Not strictly needed if not doing geojsonsf-specific ops directly
# install.packages("ggplot2")
# install.packages("here")
# install.packages("htmltools") # Not strictly needed unless outputting HTML
# install.packages("magrittr") # Generally loaded by tidyverse, but can be explicit
# install.packages("purrr") # Generally loaded by tidyverse, but can be explicit
# install.packages("Rcpp") # Not directly used in this script's logic
# install.packages("readxl")
# install.packages("rsconnect") # Not directly used in this script's logic
# install.packages("sf")
# install.packages("terra") # Not directly used in this script's logic
# install.packages("tidyverse")

# Load necessary libraries
library(dplyr)
library(geojsonio)
library(ggplot2)
library(here)
library(magrittr)
library(readr) # Explicitly load readr for read_csv
library(readxl)
library(sf) # Still needed for st_read and st_as_sf
library(tidyr) # For pivot_wider/longer, included with tidyverse but explicit can be good
library(tibble) # For tibble()


# --- 1. Configuration and Setup ---
# Ensure 'data' directory exists
if (!dir.exists(here("data"))) {
  dir.create(here("data"), recursive = TRUE)
  message(paste("Created data directory at:", here("data")))
}

# Ensure 'output' directory exists for plots
if (!dir.exists(here("output"))) {
  dir.create(here("output"), recursive = TRUE)
  message(paste("Created output directory at:", here("output")))
}

# Define selected city for analysis
SELECTED_CITY_NAME <- "Tor"

# --- 2. Data Loading and Initial Processing ---

# Function to prepare city metadata
prepare_city_metadata <- function() {
  city_names <- c("Tor","Ott", "Ham", "KCW", "Lon", "SCN", "Win", "Osh")
  census_codes <- c(35535, 505, 35537, 35541, 35555, 35539, 35559, 35532)
  zev_codes <- c(535, 505, 537, 541, 555, 539, 559, 532)
  pop_2021_k <- c(6202, 1017, 785.2 , 575.8, 543.6, 433.6, 422.6, 415.3 ) # Population in thousands

  cities_df <- tibble(
    city_name = city_names,
    CMA_UID = census_codes,
    SAC_code = zev_codes,
    city_pop_2021 = pop_2021_k
  )
  return(cities_df)
}

# Function to load and filter census data (returns an sf object)
load_and_filter_census_data <- function(selected_cma_uid) {
  st_read(here("data", "census_df_ON.geojson")) %>%
    filter(CMA_UID == selected_cma_uid) %>%
    na.omit()
}

# Function to prepare ZEV fleet data (returns a tibble)
prepare_zev_fleet_data <- function(selected_sac_code) {
  fleet_df <- read_csv(here("data", "CT_ZEV_Fleet_ON.csv")) %>%
    na.omit() # read_csv returns a tibble by default

  # Ensure CTUID is character, formatted as expected
  fleet_df$CTUID <- as.character(sprintf(fleet_df$CTUID, fmt = '%#.2f'))

  ZEV_df <- fleet_df %>%
    filter(SAC_code == selected_sac_code & fuel_type == "ZEV") %>%
    select(year, CTUID, value)

  # Handle missing year-CTUID rows by expanding and filling with 0
  temp_expand <- expand(ZEV_df, year, CTUID)
  missing_df <- anti_join(temp_expand, ZEV_df, by = c("year", "CTUID"))

  if (nrow(missing_df) > 0) {
    missing_df$value <- 0
  }

  ZEV_stock <- bind_rows(ZEV_df, missing_df) %>%
    pivot_wider(names_from = year, values_from = value, values_fill = 0)

  return(ZEV_stock)
}

# --- 3. Core Calculation Functions ---

# Function to calculate ZEV stock with population shares (returns a tibble)
calculate_zev_stock_shares <- function(zev_stock_data, census_pop_data) {
  # Join ZEV stock data (tibble) with census population data (tibble)
  ZEV_stock_with_pop <- left_join(zev_stock_data, census_pop_data %>% select(CTUID, Population), by = "CTUID") %>%
    mutate(
      Population = as.numeric(Population),
      # Fill NAs for year columns that might not have data for all CTUIDs
      # assuming year columns start with "20"
      across(starts_with("20"), ~replace_na(., 0))
    )

  # Calculate shares
  if ("2021" %in% names(ZEV_stock_with_pop)) {
    ZEV_stock_with_pop <- ZEV_stock_with_pop %>%
      mutate(
        share_pop = Population / sum(Population, na.rm = TRUE),
        share_ZEV = .data[["2021"]] / sum(.data[["2021"]], na.rm = TRUE)
      )
  } else {
    warning("2021 column not found in ZEV_stock_with_pop for share_ZEV calculation. Setting share_ZEV to 0.")
    ZEV_stock_with_pop <- ZEV_stock_with_pop %>%
      mutate(
        share_pop = Population / sum(Population, na.rm = TRUE),
        share_ZEV = 0 # Default if 2021 data is missing
      )
  }

  return(ZEV_stock_with_pop)
}

# Function to generate historical ZEV city sales (returns a tibble)
generate_historical_zev_sales <- function(zev_stock_data) {
  years_stock <- as.numeric(names(zev_stock_data)[grep("^20", names(zev_stock_data))]) # Get year columns
  min_year_stock <- min(years_stock, na.rm = TRUE)
  max_year_stock <- max(years_stock, na.rm = TRUE)

  all_years_sales <- 2009:2046
  ZEV_city_sales <- as_tibble(matrix(0.0, nrow=1, ncol=length(all_years_sales)))
  names(ZEV_city_sales) <- as.character(all_years_sales)

  # Calculate sales from stock differences for available historical years
  if (min_year_stock < max_year_stock) { # Ensure there's a range to calculate diffs
    for (year_idx in (min_year_stock + 1):max_year_stock) {
      prev_year_col <- as.character(year_idx - 1)
      curr_year_col <- as.character(year_idx)

      if (prev_year_col %in% names(zev_stock_data) && curr_year_col %in% names(zev_stock_data)) {
        sales <- sum(zev_stock_data[[curr_year_col]], na.rm = TRUE) -
          sum(zev_stock_data[[prev_year_col]], na.rm = TRUE)
        ZEV_city_sales[[curr_year_col]] <- sales
      }
    }
  }

  # Explicitly set 2017 sales to its stock sum as in the original script if 2017 stock exists
  if ("2017" %in% names(zev_stock_data)) {
    ZEV_city_sales$`2017` <- sum(zev_stock_data$`2017`, na.rm = TRUE)
  }

  return(ZEV_city_sales)
}

# Function to forecast total LDV sales (returns a tibble)
forecast_ldv_city_sales <- function(cities_data, selected_city_name, pop_forecast_data) {
  LDV_city_sales <- as_tibble(matrix(0, nrow=1, ncol=length(2021:2046))) %>%
    setNames(as.character(2021:2046))

  city_pop_2021 <- cities_data$city_pop_2021[cities_data$city_name == selected_city_name]
  # Assuming 796079 is provincial LDV new registrations and 14225 is provincial population in thousands
  LDV_city_sales$'2021' <- 796079 * as.numeric(city_pop_2021) / 14225

  # Ensure pop_forecast_data is correctly structured for joining/indexing
  pop_forecast_data_long <- pop_forecast_data %>%
    pivot_longer(
      cols = -year,
      names_to = "city_name",
      values_to = "population_k"
    )

  for (year_idx in 2022:2046) {
    prev_year_col <- as.character(year_idx - 1)
    curr_year_col <- as.character(year_idx)

    # Get population for the selected city for current and previous year
    pop_curr <- pop_forecast_data_long %>%
      filter(year == as.numeric(curr_year_col), city_name == selected_city_name) %>%
      pull(population_k)

    pop_prev <- pop_forecast_data_long %>%
      filter(year == as.numeric(prev_year_col), city_name == selected_city_name) %>%
      pull(population_k)

    if (length(pop_curr) > 0 && length(pop_prev) > 0 && pop_prev != 0) {
      LDV_city_sales[[curr_year_col]] <- LDV_city_sales[[prev_year_col]] * pop_curr / pop_prev
    } else {
      warning(paste("Population data missing or zero for", selected_city_name, "in year", prev_year_col, "or", curr_year_col))
      LDV_city_sales[[curr_year_col]] <- LDV_city_sales[[prev_year_col]] # Carry forward previous value
    }
  }
  return(LDV_city_sales)
}

# Function to allocate LDV sales to CTs (returns a tibble)
allocate_ldv_ct_sales <- function(ctuid_names, number_ctuid, ldv_city_sales, zev_stock_shares) {
  LDV_CT_sales <- as_tibble(ctuid_names) %>%
    setNames("CTUID") %>%
    bind_cols(as_tibble(matrix(0, number_ctuid, length(2021:2046)))) %>%
    setNames(c("CTUID", as.character(2021:2046)))

  # Ensure `share_pop` is correctly extracted and aligned
  share_pop_vector <- zev_stock_shares %>%
    select(CTUID, share_pop) %>%
    arrange(CTUID) %>% # Ensure order matches LDV_CT_sales CTUIDs
    pull(share_pop)

  # Years for LDV_CT_sales calculation
  years_ldv_ct_sales <- as.character(2021:2046)
  years_ldv_city_sales <- as.character(2021:2046) # Match years for ldv_city_sales subset

  # Use outer product for matrix multiplication where share_pop is a column vector
  # and ldv_city_sales is a row vector
  LDV_CT_sales[, years_ldv_ct_sales] <- outer(share_pop_vector, as.numeric(ldv_city_sales[, years_ldv_city_sales]))

  return(LDV_CT_sales)
}

# Function to forecast ZEV city sales (returns a tibble)
forecast_zev_city_sales <- function(zev_city_sales, ldv_city_sales) {
  # Create a mutable copy
  ZEV_city_sales_forecast <- as_tibble(zev_city_sales)

  # Ensure years are numeric for calculations
  zev_2021 <- ZEV_city_sales_forecast[["2021"]]
  ldv_2040 <- ldv_city_sales[["2040"]]

  # Handle potential division by zero or NA
  if (is.na(zev_2021) || zev_2021 == 0) {
    warning("ZEV sales in 2021 is zero or NA. ZEV growth calculation may be problematic. Setting growth to 1.1.")
    ZEV_growth <- 1.1 # Default growth if base is problematic
  } else {
    ZEV_growth <- as.double((ldv_2040 / zev_2021)^(1/(2040-2021)))
  }

  for (year_idx in 2022:2040){
    col_name <- as.character(year_idx)
    prev_col_name <- as.character(year_idx - 1)
    ZEV_city_sales_forecast[[col_name]] <- ZEV_city_sales_forecast[[prev_col_name]] * ZEV_growth
  }

  # ZEV sales equal LDV sales post 2040
  years_post_2040 <- as.character(2041:2046)
  ZEV_city_sales_forecast[, years_post_2040] <- ldv_city_sales[, years_post_2040]

  return(ZEV_city_sales_forecast)
}

# Function to calculate and refine ZEV CT sales with limits (returns a tibble)
calculate_refined_zev_ct_sales <- function(ctuid_names, num_ctuid, zev_stock_shares, ldv_ct_sales, zev_city_forecasted_sales) {

  # Initialize ZEV_CT_sales_init based on ZEV_stock_shares and city sales
  # This ensures all CTUIDs are present from the start and historical sales are aligned
  ZEV_CT_sales_init <- as_tibble(ctuid_names) %>%
    setNames("CTUID") %>%
    bind_cols(as_tibble(matrix(0, num_ctuid, length(2009:2046)))) %>%
    setNames(c("CTUID", as.character(2009:2046)))

  # Populate historical sales (2017-2021) based on ZEV stock shares
  years_historical_sales <- as.character(2017:2021)

  # Ensure the city sales are properly converted to a matrix for multiplication
  city_sales_historical_matrix <- as.matrix(zev_city_forecasted_sales[, years_historical_sales, drop = FALSE])
  share_ZEV_vector <- zev_stock_shares$share_ZEV # This is already a vector

  # --- MODIFIED LINE BELOW ---
  # CRUCIAL FIX: Convert city_sales_historical_matrix to a simple vector for outer()
  # The previous error occurred because outer() was receiving a 1-row matrix,
  # leading to an unexpected 3D array output. as.numeric() flattens it to a vector.
  ZEV_CT_sales_init[, years_historical_sales] <- outer(share_ZEV_vector, as.numeric(city_sales_historical_matrix))
  # --- END MODIFIED LINE ---


  # Forecast initial ZEV CT sales up to 2040 (before capping)
  initial_forecast_years <- as.character(2022:2040)
  for (year_col in initial_forecast_years) {
    prev_year_col <- as.character(as.numeric(year_col) - 1)
    if (prev_year_col %in% names(ZEV_CT_sales_init) && prev_year_col %in% names(zev_city_forecasted_sales) && as.numeric(zev_city_forecasted_sales[[prev_year_col]]) != 0) {
      # Growth factor from city sales applied to CT sales
      ZEV_CT_sales_init[[year_col]] <- ZEV_CT_sales_init[[prev_year_col]] * (
        zev_city_forecasted_sales[[year_col]] / zev_city_forecasted_sales[[prev_year_col]]
      )
    } else {
      ZEV_CT_sales_init[[year_col]] <- ZEV_CT_sales_init[[prev_year_col]] # Maintain constant if growth cannot be calculated
    }
  }

  ZEV_CT_sales_temp <- ZEV_CT_sales_init # Start iteration with the initialized sales

  # Ensure all CTUID-based dataframes are sorted for consistent row alignment
  # This is good practice before operations that depend on row order
  ZEV_CT_sales_temp <- ZEV_CT_sales_temp %>% arrange(CTUID)
  ldv_ct_sales <- ldv_ct_sales %>% arrange(CTUID)
  zev_stock_shares <- zev_stock_shares %>% arrange(CTUID)

  # Initialize ZEV_CT_sales_final from temp
  ZEV_CT_sales_final <- ZEV_CT_sales_temp

  counter <- 1
  max_iterations <- 10

  while (counter <= max_iterations) {
    comparison_years <- as.character(2022:2046)

    # CRUCIAL FIX: Ensure these subsets are always treated as matrices
    zev_sales_matrix <- as.matrix(ZEV_CT_sales_final[, comparison_years, drop = FALSE])
    ldv_sales_matrix <- as.matrix(ldv_ct_sales[, comparison_years, drop = FALSE])

    # Identify where ZEV sales exceed LDV limits
    excess_mask <- zev_sales_matrix >= ldv_sales_matrix

    # Calculate excess amounts. ifelse will return a matrix because inputs are matrices.
    excess_amounts <- ifelse(excess_mask, zev_sales_matrix - ldv_sales_matrix, 0)

    # Sum of excess sales across all CTs for each year (colSums expects 2D)
    total_excess_per_year <- colSums(excess_amounts, na.rm = TRUE)

    # Set exceeded values to their maximum (LDV limit)
    # Assign back to the tibble.
    ZEV_CT_sales_final[, comparison_years] <- ifelse(excess_mask, ldv_sales_matrix, zev_sales_matrix)

    # Calculate available population share for reallocation
    # `zev_stock_shares$share_pop` is a vector. `!excess_mask` is a logical matrix.
    # The result should be a matrix by recycling the vector.
    non_excess_pop_share <- zev_stock_shares$share_pop * (!excess_mask)

    # Sum of non-excess population shares for each year
    sum_non_excess_pop_share_per_year <- colSums(non_excess_pop_share, na.rm = TRUE)

    # Avoid division by zero
    sum_non_excess_pop_share_per_year[sum_non_excess_pop_share_per_year == 0] <- 1e-9

    # Calculate reallocation factor for each CT and year
    # `sweep` operates on a matrix (which non_excess_pop_share should be) and a vector
    reallocation_factor <- sweep(as.matrix(non_excess_pop_share), MARGIN = 2, FUN = "/", STATS = sum_non_excess_pop_share_per_year)

    # Distribute total excess among non-exceeding CTs
    reallocated_sales <- sweep(reallocation_factor, MARGIN = 2, FUN = "*", STATS = total_excess_per_year)

    # Add reallocated sales to the CTs that were not capped
    # Assign back to tibble
    ZEV_CT_sales_final[, comparison_years] <- ZEV_CT_sales_final[, comparison_years] + reallocated_sales

    # Check if any significant excess still exists
    if (sum(abs(total_excess_per_year)) < 0.01 * sum(ZEV_city_forecasted_sales[, comparison_years], na.rm = TRUE)) {
      message(paste("Converged after", counter, "iterations."))
      break
    }

    counter <- counter + 1
  }

  if (counter > max_iterations) {
    warning("Capping iterations reached maximum without full convergence. Results may still contain some excess.")
  }

  # Ensure no values are negative after adjustments, set to 0 if they are
  ZEV_CT_sales_final[, comparison_years] <- pmax(0, as.matrix(ZEV_CT_sales_final[, comparison_years, drop = FALSE]))

  # For years beyond 2040, explicitly make ZEV sales equal to LDV sales if not already capped
  years_post_2040_explicit <- as.character(2041:2046)
  ZEV_CT_sales_final[, years_post_2040_explicit] <- ldv_ct_sales[, years_post_2040_explicit, drop = FALSE]

  return(ZEV_CT_sales_final)
}
# Function to generate CT stock numbers (returns a tibble)
generate_ct_stock_forecast <- function(ctuid_names, number_ctuid, zev_ct_sales, window_years = 12) {
  ZEV_CT_stock_for <- as_tibble(ctuid_names) %>%
    setNames("CTUID") %>%
    bind_cols(as_tibble(matrix(0, number_ctuid, length(2022:2046)))) %>%
    setNames(c("CTUID", as.character(2022:2046)))

  # Years for stock calculation
  forecast_years_stock <- as.character(2022:2046)

  for (year_str in forecast_years_stock) {
    current_year <- as.numeric(year_str)
    start_year_sales <- current_year - window_years
    end_year_sales <- current_year - 1

    # Ensure sales years are within the available range of zev_ct_sales
    sales_years_to_sum <- as.character(max(min(as.numeric(names(zev_ct_sales)[-1]), na.rm = TRUE), start_year_sales) : min(max(as.numeric(names(zev_ct_sales)[-1]), na.rm = TRUE), end_year_sales))

    # Sum only if there are valid sales years to sum
    if (length(sales_years_to_sum) > 0 && all(sales_years_to_sum %in% names(zev_ct_sales))) {
      ZEV_CT_stock_for[[year_str]] <- rowSums(zev_ct_sales[, sales_years_to_sum, drop = FALSE], na.rm = TRUE)
    } else {
      ZEV_CT_stock_for[[year_str]] <- 0 # No sales data for this window
    }
  }

  return(ZEV_CT_stock_for)
}


# Function to generate LDV city stock numbers (returns a tibble)
generate_ldv_city_stock_forecast <- function(ldv_city_sales, window_years = 12) {
  LDV_city_stock_for <- as_tibble(matrix(0, nrow=1, ncol=length(2030:2046))) %>%
    setNames(as.character(2030:2046))

  forecast_years_ldv_stock <- as.character(2030:2046)

  for (year_str in forecast_years_ldv_stock) {
    current_year <- as.numeric(year_str)
    start_year_sales <- current_year - window_years
    end_year_sales <- current_year - 1

    sales_years_to_sum <- as.character(max(min(as.numeric(names(ldv_city_sales)), na.rm = TRUE), start_year_sales) : min(max(as.numeric(names(ldv_city_sales)), na.rm = TRUE), end_year_sales))

    if (length(sales_years_to_sum) > 0 && all(sales_years_to_sum %in% names(ldv_city_sales))) {
      LDV_city_stock_for[[year_str]] <- sum(ldv_city_sales[, sales_years_to_sum], na.rm = TRUE)
    } else {
      LDV_city_stock_for[[year_str]] <- 0
    }
  }
  return(LDV_city_stock_for)
}


# Function to save results (expects ZEV_CT_stock_forecast as tibble, joins geometry internally)
save_zev_stock_forecast <- function(zev_ct_stock_forecast, zev_stock_shares_data, ctuid_geometry_mapping, selected_city_name) {

  # Gather ZEV_CT_stock_forecast into long format
  zev_ct_stock_long <- zev_ct_stock_forecast %>%
    pivot_longer(
      cols = starts_with("20"),
      names_to = "year",
      values_to = "ZEV_stock"
    ) %>%
    mutate(year = as.numeric(year)) # Ensure year is numeric for consistency

  # Join Population from zev_stock_shares_data (which has CTUID and Population)
  zev_ct_stock_long <- left_join(zev_ct_stock_long, zev_stock_shares_data %>% select(CTUID, Population), by = "CTUID")

  # Join with geometry data (which is a tibble with CTUID and geometry column)
  ZEV_stock_for_final <- inner_join(zev_ct_stock_long, ctuid_geometry_mapping, by = "CTUID") %>%
    st_as_sf() # This converts the combined tibble to an sf object

  # Ensure Population is numeric if it wasn't already
  ZEV_stock_for_final$Population <- as.numeric(ZEV_stock_for_final$Population)

  keepvars <- c("CTUID", "year", "ZEV_stock", "Population", "geometry")
  ZEV_stock_for_final <- ZEV_stock_for_final %>% select(all_of(keepvars))

  filename <- here("data", paste0("ZEV_stock_for_", selected_city_name, ".geojson"))
  geojson_write(ZEV_stock_for_final, file = filename)
  message(paste("Saved ZEV stock forecast to:", filename))
}


# --- 4. Main Execution Flow ---

# Prepare city metadata
cities_df <- prepare_city_metadata()

# Determine selected city codes
selected_city_data <- cities_df %>% filter(city_name == SELECTED_CITY_NAME)
select_CMA_UID <- selected_city_data$CMA_UID
select_SAC_code <- selected_city_data$SAC_code

# Load and process census data (this returns an sf object)
census_df_sf <- load_and_filter_census_data(select_CMA_UID)

# Create CTUID-to-geometry mapping (as a simple tibble with a geometry column)
# This tibble contains the sfc geometry column but is NOT itself an sf object.
ctuid_geometry_map <- census_df_sf %>%
  as_tibble() %>% # Convert to tibble to ensure dplyr operations don't trigger sf checks
  select(GeoUID, geometry) %>%
  rename(CTUID = GeoUID)

# Extract census population data for share calculation (as a simple tibble)
census_pop <- census_df_sf %>%
  as_tibble() %>%
  select(GeoUID, Population) %>%
  rename(CTUID = GeoUID)

# Read in population forecast (as a tibble)
pop_frcst <- read_csv(here("data", "ON_pop_projections.csv"))

# Prepare ZEV fleet data (as a tibble)
ZEV_stock_raw <- prepare_zev_fleet_data(select_SAC_code)

# Calculate ZEV stock shares (operates on tibbles, returns a tibble)
ZEV_stock_with_shares <- calculate_zev_stock_shares(ZEV_stock_raw, census_pop)

# Short name for future use
name_CTUID <- ZEV_stock_with_shares$CTUID # Use the CTUIDs from the processed stock data
number_CTUID <- nrow(ZEV_stock_with_shares)

# Generate historical ZEV city sales (operates on tibble, returns a tibble)
ZEV_city_sales <- generate_historical_zev_sales(ZEV_stock_with_shares)

# Forecast total LDV sales (operates on tibbles, returns a tibble)
LDV_city_sales <- forecast_ldv_city_sales(cities_df, SELECTED_CITY_NAME, pop_frcst)

# Plot LDV city sales (example of visualization)
plot_LDV_city_sales <- LDV_city_sales %>%
  pivot_longer(everything(), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  geom_point() +
  labs(title = paste("Forecasted LDV City Sales for", SELECTED_CITY_NAME),
       x = "Year", y = "Sales") +
  theme_minimal()
print(plot_LDV_city_sales)
ggsave(here("output", paste0("LDV_city_sales_", SELECTED_CITY_NAME, ".png")), plot = plot_LDV_city_sales)

# Assign city-wide LDV sales to CTs based on population share (returns a tibble)
LDV_CT_sales <- allocate_ldv_ct_sales(name_CTUID, number_CTUID, LDV_city_sales, ZEV_stock_with_shares)

# Plot LDV CT sales (example of visualization) - Aggregated to city level for trend
plot_LDV_CT_sales_agg <- LDV_CT_sales %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "sales_value"
  ) %>%
  group_by(year) %>%
  summarise(total_sales = sum(sales_value, na.rm = TRUE)) %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = log(total_sales))) + # Using log scale as in original comment
  geom_line() +
  geom_point() +
  labs(title = paste("Aggregated Log LDV Sales by CT for", SELECTED_CITY_NAME),
       x = "Year", y = "Log of Sales") +
  theme_minimal()
print(plot_LDV_CT_sales_agg)
ggsave(here("output", paste0("LDV_CT_sales_agg_", SELECTED_CITY_NAME, ".png")), plot = plot_LDV_CT_sales_agg)

# Forecast ZEV city sales (operates on tibbles, returns a tibble)
ZEV_city_sales_forecasted <- forecast_zev_city_sales(ZEV_city_sales, LDV_city_sales)

# Plot ZEV city sales
plot_ZEV_city_sales <- ZEV_city_sales_forecasted %>%
  pivot_longer(everything(), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  geom_point() +
  labs(title = paste("Forecasted ZEV City Sales for", SELECTED_CITY_NAME),
       x = "Year", y = "Sales") +
  theme_minimal()
print(plot_ZEV_city_sales)
ggsave(here("output", paste0("ZEV_city_sales_forecasted_", SELECTED_CITY_NAME, ".png")), plot = plot_ZEV_city_sales)

# Calculate and refine ZEV CT sales (operates on tibbles, returns a tibble)
ZEV_CT_sales_final <- calculate_refined_zev_ct_sales(name_CTUID, number_CTUID, ZEV_stock_with_shares, LDV_CT_sales, ZEV_city_sales_forecasted)

# Plot aggregate CT ZEV sales
ZEV_city_final_plot_data <- ZEV_CT_sales_final %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "sales_value"
  ) %>%
  group_by(year) %>%
  summarise(ZEV_sales = sum(sales_value, na.rm = TRUE)) %>%
  mutate(year = as.numeric(year))

ggplot(ZEV_city_final_plot_data, aes(x=year, y=ZEV_sales))  +
  geom_line() +
  geom_point() +
  labs(title = paste("Aggregated ZEV Sales by CT for", SELECTED_CITY_NAME),
       x = "Year", y = "ZEV Sales")  +
  theme_minimal()
ggsave(here("output", paste0("ZEV_CT_sales_agg_final_", SELECTED_CITY_NAME, ".png")), plot = last_plot())


# Generate CT stock numbers (operates on tibbles, returns a tibble)
ZEV_CT_stock_for <- generate_ct_stock_forecast(name_CTUID, number_CTUID, ZEV_CT_sales_final)

# Generate LDV city stock numbers (operates on tibbles, returns a tibble)
LDV_city_stock_for <- generate_ldv_city_stock_forecast(LDV_city_sales)


# Plot ZEV city stock forecast
ZEV_city_stock_for_plot_data <- ZEV_CT_stock_for %>%
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "stock_value"
  ) %>%
  group_by(year) %>%
  summarise(ZEV_stock = sum(stock_value, na.rm = TRUE)) %>%
  mutate(year = as.numeric(year))

ggplot(ZEV_city_stock_for_plot_data, aes(x=year, y=ZEV_stock))  +
  geom_line() +
  geom_point() +
  labs(title = paste("Aggregated ZEV Stock Forecast for", SELECTED_CITY_NAME),
       x = "Year", y = "ZEV Stock")  +
  theme_minimal()
ggsave(here("output", paste0("ZEV_city_stock_forecast_", SELECTED_CITY_NAME, ".png")), plot = last_plot())


# Save final ZEV stock forecast to geojson
# Passes zev_stock_with_shares (which has population), and ctuid_geometry_map (which has geometry)
save_zev_stock_forecast(ZEV_CT_stock_for, ZEV_stock_with_shares, ctuid_geometry_map, SELECTED_CITY_NAME)
