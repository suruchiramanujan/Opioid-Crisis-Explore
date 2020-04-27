library(shiny)
library(tidyverse)
library(gt)
library(dplyr)
library(ggplot2)
library(lubridate)
library(janitor)
library(shinyWidgets)
library(shinythemes)
library(ggthemes)
library(maps)
library(leaflet)
library(plotly)
library(DT)
library(broom)

# copied all data and turned into rds format to decrease size

# https://wonder.cdc.gov/controller/datarequest/D77;jsessionid=3F8131C534663BDD3F79E66994FCF12E

death_by_year_and_state <- read_csv("Drug Overdose Deaths by Year and State.csv", 
                                    col_types = "lcdccddddc") %>%
  filter(`Crude Rate` != "Unreliable")

us_state <- map_data("state") %>%
  rename(state_full = region)

death_by_year_and_state <- death_by_year_and_state %>%
  filter(`UCD - Drug/Alcohol Induced Code` == "D") %>%
  rename(state_full = State) %>%
  mutate(state_full = tolower(state_full)) %>%
  left_join(us_state, by = "state_full" ) %>%
  drop_na(`Crude Rate`)

death_by_year_and_state$`Crude Rate` <-as.numeric(death_by_year_and_state$`Crude Rate`)

write_rds(death_by_year_and_state, "death_by_year_and_state.rds")

treatment_locations <- read_csv("Drug Treatment Centers.csv", col_types = "ccccccccc") %>%
  select(`Program Name`, Street, City, State, Zipcode)

write_rds(treatment_locations, "treatment_locations.rds")

# state populations

state_pop <- read_csv("State Population Estimate.csv", col_types = "dccdcdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd") %>%
  rename(state_full = NAME) %>%
  mutate(state_full = tolower(state_full))

write_rds(state_pop, "state_pop.rds")

# https://simplemaps.com/data/us-cities

longlatinfo <- read_csv("uscitieslonglat.csv", col_types = "ccccdcccddddcllcdcd") %>%
  rename(City = city) %>%
  rename(State = state_id)

write_rds(longlatinfo, "longlatinfo.rds")

treatment_locations_map <- treatment_locations %>%
  left_join(longlatinfo, by = c("City", "State")) %>%
  group_by(State) %>%
  mutate(count = n()) %>%
  mutate(state_name = tolower(state_name)) %>%
  rename(state_full = state_name) %>%
  left_join(us_state, by = "state_full") 

treatment_locations_map_per_cap <- treatment_locations_map %>%
  left_join(state_pop, by = "state_full") %>%
  group_by(State) %>%
  mutate(count_per_pop = n()/POPESTIMATE2019)

treatment_locations_map_mas <- treatment_locations %>%
  left_join(longlatinfo, by = c("City", "State")) %>%
  filter(State == "MA") %>%
  drop_na()

write_rds(treatment_locations_map_mas, "treatment_locations_map_mas.rds")

counties <- read_csv("zip_codes_states.csv", col_types = "dddccc") %>%
  na.omit() %>%
  filter(state == "MA") %>%
  rename(County = county) %>%
  rename(Municipality = city)

write_rds(counties, "counties.rds")

madeathbycounty <- read_csv("MAAverageAnnualOpioidRelatedDeathRateper100,000People.csv", 
                            col_types = "cdcdddc") %>%
  na.omit()

write_rds(madeathbycounty, "madeathbycounty.rds")

countypop <- read_csv("countypop.csv", col_types = "cdd") %>%
  na.omit() %>%
  mutate(Pop = Pop/100000) %>%
  rename(subregion = CTYNAME) %>%
  mutate(subregion = tolower(subregion)) %>%
  select(subregion, Pop)

write_rds(countypop, "countypop.rds")

madeathbycountywlonglat <- madeathbycounty %>%
  left_join(counties, by = "Municipality")

madeathbycountywlonglat <- madeathbycountywlonglat %>%
  select(County,
         `Confirmed Opioid Related Death Count 2001-2005`,
         `Confirmed Opioid Related Death Count 2006-2010`,
         `Confirmed Opioid Related Death Count 2011-2015`,
         `latitude`,
         `longitude`,
         `Municipality`) %>%
  distinct(Municipality, .keep_all= TRUE) %>%
  na.omit() %>%
  rename(subregion = County) %>%
  mutate(subregion = tolower(subregion)) %>%
  group_by(subregion) %>%
  mutate(total_deaths_2001.5 = sum(`Confirmed Opioid Related Death Count 2001-2005`)) %>%
  mutate(total_deaths_2006.10 = sum(`Confirmed Opioid Related Death Count 2006-2010`)) %>%
  mutate(total_deaths_2011.15 = sum(`Confirmed Opioid Related Death Count 2011-2015`)) %>%
  distinct(subregion, .keep_all= TRUE)

write_rds(madeathbycountywlonglat, "madeathbycountywlonglat.rds")

us_county <- map_data("county") %>%
  filter(region == "massachusetts")

# https://www.indexmundi.com/facts/united-states/quick-facts/massachusetts/percent-of-people-of-all-ages-in-poverty#table

povertybycounty <- read_csv("PovertyByCounty.csv", col_types = "cd") %>%
  na.omit() %>%
  rename(subregion = County) %>%
  mutate(subregion = tolower(subregion))

write_rds(povertybycounty, "povertybycounty.rds")


full_data <- us_county %>%
  left_join(madeathbycountywlonglat, by = "subregion") %>%
  left_join(countypop, by = "subregion") %>%
  left_join(povertybycounty, by = "subregion") %>%
  mutate(percap_2001.5 = total_deaths_2001.5/Pop) %>%
  mutate(percap_2006.10 = total_deaths_2006.10/Pop) %>%
  mutate(percap_2011.15 = total_deaths_2011.15/Pop) 

ageopioidmodel <- read_csv("U.S. age vs. opioid deaths.csv", col_types = "dcc") %>%
  clean_names() %>%
  filter(number_of_deaths != "N/A") %>%
  filter(age_range != "Total") %>%
  mutate(age_range = as.factor(age_range)) %>%
  mutate(number_of_deaths = as.numeric(number_of_deaths))

write_rds(ageopioidmodel, "ageopioidmodel.rds")

raceopioidmodel <- read_csv("U.S. race vs. opioid deaths.csv", col_types = "dcc") %>%
  clean_names() %>%
  na.omit() %>%
  mutate(race = as.factor(race)) %>%
  mutate(opioid_deaths = as.numeric(opioid_deaths))

write_rds(raceopioidmodel, "raceopioidmodel.rds")

ageandracemodel <- ageopioidmodel %>%
  na.omit() %>%
  full_join(raceopioidmodel, by = "year") %>%
  rename(deathsbyage = number_of_deaths) %>%
  rename(deathsbyrace = opioid_deaths)

deathsbycommondiseases <- read_csv("deathsbycommondiseases.csv", col_types = "ldddddd") %>%
  select(Year, `Deaths by Cancer`, `Deaths by Drug Overdose`, `Deaths by CVD`) %>%
  mutate(`Proportion of 1999 Deaths by Cancer` = `Deaths by Cancer`/549829) %>%
  mutate(`Proportion of 1999 Deaths by Drugs` = `Deaths by Drug Overdose`/19122) %>%
  mutate(`Proportion of 1999 Deaths by CVD` = `Deaths by CVD`/949900)

write_rds(deathsbycommondiseases, "deathsbycommondiseases.rds")

totaldeathsbycommondiseases <- deathsbycommondiseases %>%
  select(Year, `Deaths by Cancer`, `Deaths by Drug Overdose`, `Deaths by CVD`) %>%
  pivot_longer(.,
               cols = starts_with("Deaths by"),
               names_prefix = "Deaths by",
               values_to = "Deaths") %>%
  rename(`Cause of Death` = name)

propdeathsbycommondiseases <- deathsbycommondiseases %>%
  select(Year, `Proportion of 1999 Deaths by Cancer`, 
         `Proportion of 1999 Deaths by Drugs`, 
         `Proportion of 1999 Deaths by CVD`) %>%
  pivot_longer(., cols = starts_with("Proportion of"), names_prefix = "Proportion of 1999 Deaths by", values_to = "Prop.Deaths") %>%
  rename(`Cause of Death` = name)

mavsus_death <- read_csv("MAAgeAdjustedOpioidRelatedDeathRateByYear.csv", col_types = "cdc") %>%
  na.omit()

write_rds(mavsus_death, "mavsus_death.rds")

mavsus_death <- mavsus_death %>%
  group_by(Geography) %>%
  filter (Year != "2015") %>%
  filter (Year != "1999") %>%
  rename(deathperhundredthousand =`Age-Adjusted Opioid-Related Death Rate per 100,000 People`)