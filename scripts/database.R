# Load packages
source(here::here("scripts/setup.R"))


# Data collection
database <- read_csv(("https://covid.ourworldindata.org/data/owid-covid-data.csv"))

## Data cleaning

# filter European countries from "continent" 
database_EU <- database %>% 
  filter(continent == "Europe") %>% # 140k obs -> 31k observations remaining
  as_tibble()

# remove variables that have to many NAs
database_EU <- database_EU %>% 
  filter(location != "Russia") %>% 
  relocate(c(date, continent), .before = iso_code) %>%
  select(-c(weekly_icu_admissions                        
            ,weekly_icu_admissions_per_million            
            ,handwashing_facilities                      
            ,weekly_hosp_admissions                     
            ,weekly_hosp_admissions_per_million           
            ,total_boosters                            
            ,total_boosters_per_hundred                  
            ,excess_mortality_cumulative_absolute        
            ,excess_mortality_cumulative                  
            ,excess_mortality                            
            ,excess_mortality_cumulative_per_million
            ,icu_patients
            ,icu_patients_per_million
            ,hosp_patients
            ,hosp_patients_per_million
            ,hospital_beds_per_thousand
            ,extreme_poverty
            ,tests_per_case
            ,tests_units
  )) %>% 
  rename(country_code = iso_code)%>%
  filter(!is.na(total_cases)) %>% 
  as_tibble()



# some negative absurd values in "new_cases", maybe replace them by 0
#database_EU$new_cases[database_EU$new_cases < 0] <- 0


# can be useful later on (map?)
countries <- unique(database_EU$location)

# dygraph
database_EU_dg <- database_EU %>% select(date, location, new_cases_smoothed) #-----------> TO DELETE IF WE DONT USE IT IN SHINY
database_EU_xts <- xts(x = database_EU$new_cases_smoothed, order.by = database_EU$date) # transform into xts to plot dygraph


# MAP SUB-DATASETS
#-------------------------------------------------------------------------------

# DISPLAY SETTINGS
#-----------------

colors <- c("tan4", "turquoise4")
pal <- colorNumeric(palette = "BrBG" ,domain = c(0, 100)) 

# COORDINATE DATA
#----------------
coordinates <- read_csv(here::here("map.data/coordinates.csv")) %>% #COORDINATES
  select(country_code = `Alpha-3 code`, 
         latitude = `Latitude (average)`,
         longitude = `Longitude (average)`) %>%
  filter(country_code != "NA") %>%
  mutate_if(is.character, as.factor)

# LAST DATE GLOBAL SUMMARY
#-------------------------
last_date <- database_EU %>% 
  filter(date == max(date)-7) %>% 
  group_by(date) %>% 
  summarise(total_cases_EU = sum(total_cases), 
            total_deaths_EU = sum(total_deaths, na.rm = TRUE), 
            people_fully_vaccinated_EU = sum(people_fully_vaccinated, na.rm = TRUE))





