################################# TEST CODE FROM 1ST DAY #######################################

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
                                    percentcivilianwohealthinsur = "DP03_0099PE",
                                    totalnumhouseholds = "DP02_0001E",
                                    numpeopletravelbyvehicle = "B08006_002E",
                                    meantraveltimeforwork = "DP03_0025E"))


################################# SAMPLE CODE FROM ARTICLES #####################################

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



# get aging populations in Ramsey County, Minnesota from a 2010-2016 ACS by census tracts
vars <- paste0("B01001_0", c(20:25, 44:49))

ramsey <- get_acs(geography = "tract", 
                  variables = vars, 
                  state = "MN", 
                  county = "Ramsey", 
                  year = 2016)

head(ramsey %>% select(-NAME))

ramsey65 <- ramsey %>%
  group_by(GEOID) %>%
  summarize(sumest = sum(estimate), 
            summoe = moe_sum(moe, estimate))

head(ramsey65)