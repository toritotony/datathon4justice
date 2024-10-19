# Install and load necessary packages
install.packages("dplyr")
install.packages("tidycensus")
install.packages("tidyverse")
install.packages("stringr")
install.packages("readxl")
library(tidycensus)
library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)

# Set API key
census_api_key("8b5837f549e20d8305aed20693c65c7205b50b5a", install = TRUE)

# Load the variables for the 2023 1-year ACS
v23year1 <- load_variables(2023, "acs1")
v22year5 <- load_variables(2022, "acs5")

specified_vars <- c("B01003_001", "B01002_001", "B19013_001", "DP03_0009PE", "DP03_0128PE", 
                    "DP03_0074PE", "DP02_0068PE", "DP02_0068E", "DP02_0067PE", "DP03_0099PE",
                    "DP02_0001E", "B08006_002E", "DP03_0025E")

# Fetch data from the 2023 1-year ACS
pop23year1 <- get_acs(geography = "state", 
                 year = 2023,
                 survey = "acs1", 
                 variables = specified_vars)

pop22year5 <- get_acs(geography = "state", 
                      year = 2022,
                      survey = "acs5", 
                      variables = specified_vars)

# Filter the data to include only the variables you specified
pop23year1_filtered <- pop23year1 %>%
  filter(variable %in% specified_vars)

pop22year5_filtered <- pop22year5 %>%
  filter(variable %in% specified_vars)






# get tract median income data for all states 
us <- unique(fips_codes$state)[1:51]

totalincome <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B19013_001", state = x)
})

totalincome %>%
  ggplot(aes(x = value, y = reorder(NAME, value))) + 
  geom_point()



# variable changes 2023 1 year for API
excelvars2023 <- read_excel("C:\\Users\\wolfe\\Downloads\\2023-1yr-api-changes.csv")









# get median age by state in 2020 with data drawn from demographic characteristics, then plot ggplot
age2020 <- get_decennial(geography = "state", 
                       variables = "P13_001N", 
                       year = 2020,
                       sumfile = "dhc")

age2020 %>%
  ggplot(aes(x = value, y = reorder(NAME, value))) + 
  geom_point()









# get median household income data from 2017-2021 ACS for Vermont then plot point + uncertainty
vt <- get_acs(geography = "county", 
              variables = c(medincome = "B19013_001"), 
              state = "VT",
              year = 2021)

vt %>%
  mutate(NAME = gsub(" County, Vermont", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by county in Vermont",
       subtitle = "2017-2021 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")
