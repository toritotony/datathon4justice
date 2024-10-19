# Install and load necessary packages
install.packages("tidycensus")
install.packages("tidyverse")
library(tidycensus)
library(tidyverse)

# Set API key
census_api_key("8b5837f549e20d8305aed20693c65c7205b50b5a", install = TRUE)

# Load the variables for the 2023 1-year ACS
v23 <- load_variables(2023, "acs1")

# Fetch data from the 2023 1-year ACS
pop23 <- get_acs(geography = "state", 
                 year = 2023,
                 survey = "acs1", 
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



