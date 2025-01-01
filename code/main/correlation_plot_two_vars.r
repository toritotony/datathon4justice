# Load required libraries
library(tidycensus)
library(tidyverse)

# Set Census API Key (replace with your actual API key)
#census_api_key("your_census_api_key", install = TRUE)

# Function to find correlation between two variables in a state, optionally filtering by county or city
plot_correlation_between_vars <- function(state, var1, var2, county = NULL, city = NULL) {
  
  # Fetch data for the first variable
  acs_data_var1 <- get_acs(geography = "tract",
                           variables = var1,
                           state = state,
                           geometry = FALSE,  # No need for geometry, just the data
                           year = 2022)
  
  # Fetch data for the second variable
  acs_data_var2 <- get_acs(geography = "tract",
                           variables = var2,
                           state = state,
                           geometry = FALSE,  # No need for geometry, just the data
                           year = 2022)
  
  # Join the two datasets by GEOID (common geographic ID)
  combined_data <- acs_data_var1 %>%
    select(GEOID, estimate_var1 = estimate) %>%
    inner_join(acs_data_var2 %>%
                 select(GEOID, estimate_var2 = estimate), by = "GEOID")
  
  # Filter by county or city if provided
  if (!is.null(county)) {
    combined_data <- combined_data %>%
      filter(str_detect(NAME, county))
  }
  
  if (!is.null(city)) {
    combined_data <- combined_data %>%
      filter(str_detect(NAME, city))
  }
  
  # Check if the filtered data is empty
  if (nrow(combined_data) == 0) {
    stop("No data found for the specified region. Please check the county or city name.")
  }
  
  # Calculate the correlation between the two variables
  correlation_value <- cor(combined_data$estimate_var1, combined_data$estimate_var2, use = "complete.obs")
  
  # Output the correlation value
  print(paste("Correlation between", var1, "and", var2, "is:", round(correlation_value, 3)))
  
  # Create a scatter plot to visualize the correlation
  ggplot(combined_data, aes(x = estimate_var1, y = estimate_var2)) +
    geom_point(alpha = 0.6) +
    labs(title = paste("Scatter Plot of", var1, "vs.", var2, "in", state,
                       ifelse(!is.null(county), paste("County:", county), ""),
                       ifelse(!is.null(city), paste("City:", city), "")),
         x = var1, y = var2) +
    theme_minimal()
}

# Example usage for finding correlation between two variables (Total Population and Median Income)
plot_correlation_between_vars(state = "IL", var1 = "B01003_001", var2 = "B19013_001")
