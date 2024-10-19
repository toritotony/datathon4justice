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

# Fetch data from the 2023 1-year ACS
pop23year1 <- get_acs(geography = "state", 
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

pop22year5 <- get_acs(geography = "state", 
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
                                    percentcivilianwohealthins = "DP03_0099PE",
                                    totalnumhouseholds = "DP02_0001E",
                                    numpeopletravelbyvehicle = "B08006_002E",
                                    meantraveltimeforwork = "DP03_0025E"))


# get tract population data for all states 
us <- unique(fips_codes$state)[1:51]

totalpop <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = "B01003_001", state = x)
})


# variable changes 2023 1 year for API

excelvars2023 <- read_excel("C:\\Users\\wolfe\\Downloads\\2023-1yr-api-changes.csv")



