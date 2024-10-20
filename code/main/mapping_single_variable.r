# Load required libraries
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(ggplot2)
library(viridis)

# Enable caching for shapefiles
options(tigris_use_cache = TRUE)

# Set Census API Key (replace with your actual API key)
#census_api_key("your_census_api_key", install = TRUE)

# Function to dynamically plot census data at the tract or place level for state, variable, and optionally county or city
plot_census_data_dynamic <- function(state, variable, county = NULL, city = NULL) {
  
  # Error handling: Check if state and variable are provided
  if (missing(state) | missing(variable)) {
    stop("Please provide both a state and a variable.")
  }
  
  # Step 1: Load tract-level geographic data for the state
  state_tracts <- tracts(state = state, cb = TRUE, year = 2022)  # Load tract boundaries
  
  # Step 2: Fetch ACS data for the specified state using tracts or place for cities
  if (!is.null(city)) {
    # If city is provided, use "place" geography for city-level data
    acs_data <- get_acs(geography = "place",
                        variables = variable,
                        state = state,
                        geometry = TRUE,
                        year = 2022)
    
    # Filter to the specific city within the state
    acs_data <- acs_data %>%
      filter(str_detect(NAME, city))
    
    # Also filter the tract data to the city, so we get tract borders within the city if necessary
    state_tracts <- state_tracts %>%
      filter(str_detect(NAME, city))
  } else {
    # If no city is provided, use "tract" geography for the entire state or county
    acs_data <- get_acs(geography = "tract",
                        variables = variable,
                        state = state,
                        geometry = TRUE,
                        year = 2022)
  }
  
  # Step 3: Filter by county if provided
  if (!is.null(county)) {
    acs_data <- acs_data %>% 
      filter(str_detect(NAME, county))
    
    state_tracts <- state_tracts %>% 
      filter(str_detect(NAME, county))
  }
  
  # Check if the filtered data is empty
  if (nrow(acs_data) == 0) {
    stop("No data found for the specified region. Please check the county or city name.")
  }
  
  # Step 4: Plot the tract or place-level data
  ggplot() +
    geom_sf(data = state_tracts, fill = NA, color = "black", size = 0.5) +   # Tract boundaries with no fill, just borders
    geom_sf(data = acs_data, aes(fill = estimate), color = "white", size = 0.3) +  # Plot ACS data by tract/place with fill
    scale_fill_viridis_c(option = "plasma", na.value = "grey50") +  # Color scale
    labs(title = paste(variable, "in", state, 
                       ifelse(!is.null(county), paste("County:", county), ""), 
                       ifelse(!is.null(city), paste("City:", city), "")),
         fill = "Estimate") +
    theme_minimal()
}

# Example usage for the entire state with tract-level borders
plot_census_data_dynamic(state = "IL", variable = "B01003_001")

# Example usage for a specific county (Cook County in Illinois)
plot_census_data_dynamic(state = "TN", variable = "B19013_001", county = "Davidson")

# Example usage for a specific city (Chicago in Illinois)
plot_census_data_dynamic(state = "ND", variable = "B19013_001", city = "Pine Ridge")
