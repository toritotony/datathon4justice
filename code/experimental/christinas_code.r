---
title: "CommunitySafety_API"
author: "Christina Segar"
date: "2024-10-18"
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Dataset Information
ACS 5 2022 https://api.census.gov/data/2022/acs/acs5/variables.html 
## Setup
```{r load, message=FALSE, warning=FALSE}
library(tidycensus)
library(tidyverse)
library(purrr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tigris)
library(sf)
library(viridis)
# Replace with your API Key
census_api_key("") 
```
## Define Variables
We'll be pulling data for all census tract state and counties with the specified variables, but here we'll define which ones we want to pass in to our plots.
**NOTE: So far only Total Population "B01003_001" variable seems to be widely available within this dataset for Census Tract. We will need additional investigation to find out where other variables should be pulled from outside of 2022 ACS5 dataset.
```{r vars}
# For API Call
censusVariablesList <- c(     
  "B01003_001"   # Total Pop
      #"B01002_001E",  # Median Age
      #"B19013_001E",  # Median Household Income
      #"DP03_0009PE",  # Unemployment rate (16+)
      #"DP03_0128PE",  # Poverty rate
      #"DP03_0074PE",  # SNAP benefits rate
      #"DP02_0068PE",  # Bachelor's degree rate
      #"DP02_0067PE",  # High school graduation rate
      #"DP03_0099PE",  # Uninsured rate
      #"DP02_0001E",   # Total households
      #"B08006_002E",  # People commuting by car, truck, or van
      #"DP03_0025E"    # Mean travel time to work
  )
# For Plotting
stateNamePlot <- "Illinois"        # State
countyNamePlot <- "Cook"           # County
censusVariablePlot <- "B01003_001" # Variable
```
## API Call
Clearing out old variables
```{r reset, message=FALSE, warning=FALSE, include=FALSE}
# Clear out old datasets
rm(list = c("censusResultShapes", "censusResult"))#, "tracts_data"))
gc() # Clear memory
```
Set up list of states to pass into API call. Saving API call results to locak saved file to make future runs more efficient.
Limiting number of requests per second to try to avoid rate_sleep() error
```{r call}
options(tigris_use_cache = TRUE) # Caching shapefiles
us <- tigris::states()$STUSPS # Listing state abbreviations
# Function to get data with error handling and retries
safe_get_acs <- safely(function(x) {
  get_acs(
    geography = "tract",  # Census Tract level
    year = 2022, 
    state = x,
    geometry = TRUE,  
    variables = censusVariablesList
  )
})
# Add a rate limiter to avoid overloading the API (1 request every second)
slow_get_acs <- slowly(safe_get_acs, rate = rate_delay(1))
# Check if cached file exists, else perform API call and save the result
if (file.exists("census_result.rds")) {
  censusResult <- readRDS("census_result.rds")
} else {
  censusResult <- map_df(us, function(state) {
    result <- slow_get_acs(state)
    
    if (!is.null(result$result)) {
      return(result$result)
    } else {
      message(paste("Failed for state:", state))
      return(NULL)  # Skip the state if it fails
    }
  })
  
# Save the fetched data to a local file for re-use in future runs
  saveRDS(censusResult, "census_result.rds")
}
```
```{r preview_result}
# Preview the API call result
print(head(censusResult))
```
# Plotting Census Results
Note: For counties with many census tracts, this can become a very cluttered graph.
```{r plot_function, echo=FALSE}
#Function to plot county tract data for specific State, County, and Census Variable
plot_census_data <- function(stateNameVar, censusVar, countyNameVar) {
  
  censusResult %>%
    filter(variable == censusVar, 
           str_detect(NAME, stateNameVar),
           if (!is.null(countyNameVar)) str_detect(NAME, countyNameVar) else TRUE) %>%
    ggplot(aes(x = estimate, y = reorder(NAME, estimate))) + 
    geom_point() +
    labs(x = "Estimate", y = "Census Tract", title = paste(censusVar, "by Census Tract in",countyNameVar," County, ",stateNameVar)) +
    theme_minimal()
}
```
To quickly test whether a given state and county has census tract data for a variable, re-run {r vars} chunk and this one to check for empty plots.
```{r plot_call}
plot_census_data(stateNamePlot, censusVariablePlot, countyNamePlot)
```
# Merge Census Data with Shapefiles
Map shapefiles to census data for a single County and State
```{r shape_merge_function}
combine_census_shapefiles <- function(stateNameVar2, countyNameVar2) {
  # Create a file name for caching shapefile data
  cache_file <- paste0("tracts_data_", stateNameVar2, "_", countyNameVar2, ".rds")
  
  # Check if the shapefile data for the state and county exists
  if (file.exists(cache_file)) {
    tracts_data <- readRDS(cache_file)
    message("Loaded cached shapefile data for ", countyNameVar2, " County, ", stateNameVar2)
  } else {
    # If it doesn't exist, fetch the tracts data from the Census API
    tracts_data <- tracts(state = stateNameVar2, county = countyNameVar2, cb = TRUE, year = 2022)
    
    # Save the fetched data to a file for future use
    saveRDS(tracts_data, cache_file)
    message("Saved new shapefile data for ", countyNameVar2, " County, ", stateNameVar2)
  }
  # Make sure the GEOID types match in both datasets
  tracts_data <- tracts_data %>% mutate(GEOID = as.character(GEOID))
  filtered_censusResult <- censusResult %>% 
    filter(str_detect(NAME, stateNameVar2), str_detect(NAME, countyNameVar2)) %>%
    mutate(GEOID = as.character(GEOID))
  
  # Perform a spatial join between the shapefiles and filtered census results
  censusResultShapes <- st_join(tracts_data, filtered_censusResult, join = st_intersects)
  
  return(censusResultShapes)
}
```
Saving the output to a result variable
```{r shape_merge}
censusResultShapes <- combine_census_shapefiles(stateNamePlot, countyNamePlot)
```
# Plotting Spacial Data
```{r plot_spacial}
filtered_data <- censusResultShapes %>%
  filter(variable == censusVariablePlot) 
# Plot the filtered data
ggplot(data = filtered_data) +
  geom_sf(aes(fill = estimate)) + 
  scale_fill_viridis_c(option = "plasma", na.value = "grey50") +  # Use a gradient color scale
  labs(title = paste(censusVariablePlot, " in", countyNamePlot, "County,", stateNamePlot),
       fill = "Population Estimate") +
  theme_minimal()
```