###########################################################################################
###########################################################################################
########################### Re-envisioning Community Safety Data Script ###################
###########################################################################################
###########################################################################################

# Install and load necessary packages
install.packages("dplyr")
install.packages("tidycensus")
install.packages("tidyverse")
install.packages("stringr")
install.packages("readxl")
install.packages("tigris")
library(tidycensus)
library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)
library(tigris)

################################ DATA COLLECTION ##########################################

# enable caching of data for shapefiles
options(tigris_use_cache = TRUE)

# Set API key
census_api_key("8b5837f549e20d8305aed20693c65c7205b50b5a", install = TRUE)

# Load the recent variable dictionaries for ACS
v23year1 <- load_variables(2023, "acs1")
v22year5 <- load_variables(2022, "acs5")

specified_vars <- c("B01003_001", "B01002_001", "B19013_001", "DP03_0009PE", "DP03_0128PE", 
                    "DP03_0074PE", "DP02_0068PE", "DP02_0068E", "DP02_0067PE", "DP03_0099PE",
                    "DP02_0001E", "B08006_002E", "DP03_0025E")

new_vars <- c("B25122", "B25012", "B28002_005", "B28002_004", "B22002_002")

us <- unique(fips_codes$state)[1:51]

# get state variable data nationally
us_state_data <- get_acs(geography = "state", 
                      year = 2022,
                      survey = "acs5", 
                      variables = c(totalpop = "B01003_001", 
                                    medianage = "B01002_001",
                                    medianincome = "B19013_001",
                                    unemployrate16plus = "DP03_0009PE",
                                    povertyratepop = "DP03_0128PE",
                                    percenthousesSNAP = "DP03_0074PE",
                                    percentpeople25bachelors = "DP02_0068PE",
                                    estpeople25plusbachelors = "DP02_0068E",
                                    percent25highschoolormore = "DP02_0067PE",
                                    percentcivilianwohealthinsur = "DP03_0099PE",
                                    totalnumhouseholds = "DP02_0001E",
                                    numpeopletravelbyvehicle = "B08006_002E",
                                    meantraveltimeforwork = "DP03_0025E"))

# get state variable data nationally
us_city_data <- map_df(us, function(x) { 
                         get_acs(geography = "place", 
                         state = x,
                         year = 2022,
                         survey = "acs5", 
                         variables = c(totalpop = "B01003_001", 
                                       medianage = "B01002_001",
                                       medianincome = "B19013_001",
                                       unemployrate16plus = "DP03_0009PE",
                                       povertyratepop = "DP03_0128PE",
                                       percenthousesSNAP = "DP03_0074PE",
                                       percentpeople25bachelors = "DP02_0068PE",
                                       estpeople25plusbachelors = "DP02_0068E",
                                       percent25highschoolormore = "DP02_0067PE",
                                       percentcivilianwohealthinsur = "DP03_0099PE",
                                       totalnumhouseholds = "DP02_0001E",
                                       numpeopletravelbyvehicle = "B08006_002E",
                                       meantraveltimeforwork = "DP03_0025E"))})

# get tract variable data for all states 
us_tract_data <- map_df(us, function(x) {
  get_acs(survey = "acs5", geography = "tract", year = 2022, geometry = TRUE, variables = c(totalpop = "B01003_001", 
                                                                           medianage = "B01002_001",
                                                                           medianincome = "B19013_001",
                                                                           unemployrate16plus = "DP03_0009PE",
                                                                           povertyratepop = "DP03_0128PE",
                                                                           percenthousesSNAP = "DP03_0074PE",
                                                                           percentpeople25bachelors = "DP02_0068PE",
                                                                           estpeople25plusbachelors = "DP02_0068E",
                                                                           percent25highschoolormore = "DP02_0067PE",
                                                                           percentcivilianwohealthinsur = "DP03_0099PE",
                                                                           totalnumhouseholds = "DP02_0001E",
                                                                           numpeopletravelbyvehicle = "B08006_002E",
                                                                           meantraveltimeforwork = "DP03_0025E"), state = x)
})

# get food access variables from USDA

food_access_2019 <- read_excel("C:\\Users\\wolfe\\OneDrive\\Desktop\\datathon4justice\\data-sources\\FoodAccessResearchAtlasData2019.xlsx")
food_access_2019 <- food_access_2019 %>%
  rename(GEOID = CensusTract)

################################### DATA TRANSFORMATION #########################################

# group by GEOID and provide averages for values
us_tract_data_grouped <- us_tract_data %>%
  group_by(GEOID, variable) %>%  # Group by GEOID and variable
  summarize(mean_estimate = mean(estimate, na.rm = TRUE),  # Calculate mean estimate
            mean_moe = mean(moe, na.rm = TRUE)) 

# group by GEOID and provide averages for values
us_state_data_grouped <- us_state_data %>%
  group_by(GEOID, variable) %>%  # Group by GEOID and variable
  summarize(mean_estimate = mean(estimate, na.rm = TRUE),  # Calculate mean estimate
            mean_moe = mean(moe, na.rm = TRUE)) 

# join food access and tract data for Beth 

join_tract_food_access <- full_join(us_tract_data, food_access_2019, by = "GEOID")

# filtered us state data by specified variables (NEED TO FIND NEW CODES OR DATA FOR VARS NOT FOUND)
us_state_data_filtered <- us_state_data %>%
  filter(variable %in% c("totalpop", 
                         "medianage",
                         "medianincome",
                         "unemployrate16plus",
                         "povertyratepop",
                         "percenthousesSNAP",
                         "percentpeople25bachelors",
                         "estpeople25plusbachelors",
                         "percent25highschoolormore",
                         "percentcivilianwohealthinsur",
                         "totalnumhouseholds",
                         "numpeopletravelbyvehicle",
                         "meantraveltimeforwork"))

# filtered us tract data by specified variables 
us_tract_data_filtered <- us_tract_data %>%
  filter(variable %in% c("totalpop", 
                         "medianage",
                         "medianincome",
                         "unemployrate16plus",
                         "povertyratepop",
                         "percenthousesSNAP",
                         "percentpeople25bachelors",
                         "estpeople25plusbachelors",
                         "percent25highschoolormore",
                         "percentcivilianwohealthinsur",
                         "totalnumhouseholds",
                         "numpeopletravelbyvehicle",
                         "meantraveltimeforwork"))

# join city-tract-state tables into one table
us_tract_data_to_join <- us_tract_data %>%
  mutate(GEOID = substr(GEOID, 1, 2))

# Join tract data with state data based on state_fips (first two chars of GEOID)
tract_state_data <- us_tract_data_to_join %>%
  left_join(us_state_data, by = "GEOID")

# function to extract and dynamically name dataset for tracts in question

################################## PLOTTING #########################################

# Function to plot census and food access data at the tract level
plot_census_food_data_tract <- function(stateName, censusVariable, countyName = NULL, 
                                        title = NULL, x_label = NULL, y_label = NULL, 
                                        point_color = "blue", point_size = 3) {
  
  # Validate that the state exists in the data
  if (!any(str_detect(join_tract_food_access$NAME, stateName))) {
    stop("State name not found in the data. Please provide a valid state name.")
  }
  
  # Validate that the variable exists in the data
  if (!censusVariable %in% unique(join_tract_food_access$variable)) {
    stop("Census variable not found in the data. Please provide a valid variable code.")
  }
  
  # Filter the data based on state, county (if provided), and variable
  plot_data <- join_tract_food_access %>%
    filter(variable == censusVariable, 
           str_detect(NAME, stateName),
           if (!is.null(countyName)) str_detect(NAME, countyName) else TRUE)
  
  # Check if filtered data has rows
  if (nrow(plot_data) == 0) {
    stop("No data available for the given combination of state, county, and variable.")
  }
  
  # Set dynamic plot labels if not provided
  if (is.null(x_label)) x_label <- "Population Estimate"
  if (is.null(y_label)) y_label <- "Census Tract"
  if (is.null(title)) title <- paste(censusVariable, "in", stateName, if (!is.null(countyName)) paste("(", countyName, ")"))
  
  # Generate the plot
  p <- ggplot(plot_data, aes(x = estimate, y = reorder(NAME, estimate))) + 
    geom_point(color = point_color, size = point_size) +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal(base_size = 15) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      plot.title = element_text(size = 16, face = "bold")
    )
  
  return(p)
}

plot_census_food_data_tract(state="Illinois", censusVariable = "PovertyRate", )

################################# CORRELATIONS ######################################

# Function to calculate correlation between two variables at the tract level
plot_correlation_food_access_tract <- function(state, var1, var2, county = NULL) {
  
  # Fetch data for the first variable from the tract-food-access data
  data_var1 <- join_tract_food_access %>%
    filter(variable == var1, str_detect(NAME, state))
  
  # Fetch data for the second variable from the tract-food-access data
  data_var2 <- join_tract_food_access %>%
    filter(variable == var2, str_detect(NAME, state))
  
  # Join the two datasets by GEOID (common geographic ID for tracts)
  combined_data <- data_var1 %>%
    select(GEOID, estimate_var1 = estimate) %>%
    inner_join(data_var2 %>%
                 select(GEOID, estimate_var2 = estimate), by = "GEOID")
  
  # Filter by county if provided
  if (!is.null(county)) {
    combined_data <- combined_data %>%
      filter(str_detect(NAME, county))
  }
  
  # Check if the filtered data is empty
  if (nrow(combined_data) == 0) {
    stop("No data found for the specified region. Please check the county or variable names.")
  }
  
  # Calculate the correlation between the two variables
  correlation_value <- cor(combined_data$estimate_var1, combined_data$estimate_var2, use = "complete.obs")
  
  # Output the correlation value
  print(paste("Correlation between", var1, "and", var2, "is:", round(correlation_value, 3)))
  
  # Create a scatter plot to visualize the correlation
  ggplot(combined_data, aes(x = estimate_var1, y = estimate_var2)) +
    geom_point(alpha = 0.6) +
    labs(title = paste("Scatter Plot of", var1, "vs.", var2, "in", state,
                       ifelse(!is.null(county), paste("County:", county), "")),
         x = var1, y = var2) +
    theme_minimal()
}
