# script to create backup stores of the data used in the Silicon Valley COVID-19 Dashboard
# processes the data to create the data frames that go directly into the plots in the dashboard

library(tidyverse)
library(lubridate)
library(jsonlite)
library(pracma)
options(
  tigris_class = "sf",
  tigris_use_cache = T
)

# get the SCC data from their open data portal
scc_cases_by_date <- read_csv("https://data.sccgov.org/resource/6cnm-gchg.csv") %>% filter(!is.na(date))
scc_race_cases <- read_csv("https://data.sccgov.org/resource/ccm2-45w3.csv")
scc_age_cases <- read_csv("https://data.sccgov.org/resource/ige8-ixqu.csv")
scc_deaths_by_date <- read_csv("https://data.sccgov.org/resource/tg4j-23y2.csv") %>% filter(!is.na(date))
scc_race_deaths <- read_csv("https://data.sccgov.org/resource/nd69-4zii.csv")
scc_age_deaths <- read_csv("https://data.sccgov.org/resource/pg8z-gbgv.csv")
scc_hosp_by_date <- read_csv("https://data.sccgov.org/resource/5xkz-6esm.csv") %>% filter(!is.na(date))
scc_testing_by_date <- read_csv("https://data.sccgov.org/resource/dvgc-tzgq.csv") %>% filter(!is.na(collection_date))


# get the SMC data
# SMC testing, cases, and demographics data from scraped data from the SMC dashboards 
smc_testing_by_date <- read_csv("https://raw.githubusercontent.com/stanfordfuturebay/stanfordfuturebay.github.io/master/covid19/smc_tests_scraped.csv")
smc_cases_by_date <- read_csv("https://raw.githubusercontent.com/stanfordfuturebay/stanfordfuturebay.github.io/master/covid19/smc_cases_scraped.csv")
smc_dem_data <- read_csv("https://raw.githubusercontent.com/stanfordfuturebay/stanfordfuturebay.github.io/master/covid19/smc_covid_dem_data_scraped.csv")

# SMC hospitalization and deaths data from the github repo from code for america brigades
many_county_data <- fromJSON("https://raw.githubusercontent.com/sfbrigade/stop-covid19-sfbayarea/master/data/data.json")
smc_data_cfa <- many_county_data[["San Mateo County"]][["cases"]]

# clean the SMC demographic data and split into two data frames for race and age
smc_race_data <- smc_dem_data %>% 
  dplyr::select(demographic, Cases, Deaths) %>% 
  filter(grepl("Race", demographic, fixed = TRUE)) %>% # select only demographics that have to do with race
  mutate(demographic = substr(demographic, 16, nchar(.)), # get just the relevant portion of the string
         demographic = trimws(demographic),
         Deaths = replace_na(Deaths, 0))

smc_age_data <- smc_dem_data %>% 
  dplyr::select(demographic, Cases, Deaths) %>% 
  filter(grepl("Age", demographic, fixed = TRUE)) %>% # select only demographics that have to do with age
  mutate(demographic = substr(demographic, 11, nchar(.)), # get just the relevant portion of the string
         Deaths = replace_na(Deaths, 0))

# get the death and cases update date for demographics from the SMC demographic data frame
smc_death_update_date_str <- smc_dem_data %>% 
  dplyr::select(death_update_date_str) %>%
  filter(!is.na(death_update_date_str))

# convert to date form with function from lubridate package
smc_death_update_date_dem <- parse_date_time(smc_death_update_date_str$death_update_date_str, orders = "mdy")

# same for case update date
smc_case_update_date_str <- smc_dem_data %>% 
  dplyr::select(case_update_date_str) %>%
  filter(!is.na(case_update_date_str))
smc_case_update_date_dem <- parse_date_time(smc_case_update_date_str$case_update_date_str, orders = "mdy")

# save demographic update times for SMC
update_date <- c(smc_case_update_date_dem, smc_death_update_date_dem)
data_type <- c("cases demographics", "deaths demographics")
dem_update_times <- data.frame(data_type, update_date)
# save csv
write.csv(dem_update_times, "covid19/sv_dash_data_processing/smc_dem_data_update_dates.csv")

# join the data for SMC with SCC data
cases_by_date <- scc_cases_by_date %>% 
  rename(new_cases_scc = new_cases, total_cases_scc = total_cases) %>%
  full_join(smc_cases_by_date %>% 
              dplyr::select(date, new_cases, total_cases) %>% 
              rename(total_cases_smc = total_cases, new_cases_smc = new_cases)) %>% 
  # check if there are dates that aren't listed, which we assume have zero new cases
  full_join(data.frame(date = seq(min(scc_cases_by_date$date), max(scc_cases_by_date$date), by = "days"))) %>% 
  arrange(date) %>%
  # combine SCC and SMC values
  mutate(new_cases_scc = replace_na(new_cases_scc, 0),
         total_cases_scc = cumsum(new_cases_scc),
         new_cases_smc = replace_na(new_cases_smc, 0), 
         total_cases_smc = cumsum(new_cases_smc),
         new_cases = new_cases_scc + new_cases_smc, 
         total_cases = cumsum(new_cases),
         new_cases_mov7 = movavg(new_cases, 7, type = "s")) 

# save csv
write.csv(cases_by_date, "covid19/sv_dash_data_processing/sv_cases_by_date.csv")

deaths_by_date <- scc_deaths_by_date %>% 
  dplyr::select(date, total, cumulative) %>% rename(new_deaths_scc = total, total_deaths_scc = cumulative) %>% 
  full_join(smc_data_cfa %>% 
              dplyr::select(date, deaths) %>% 
              rename(total_deaths_smc = deaths) %>% 
              mutate(new_deaths_smc = c(NA, diff(total_deaths_smc)), date = as.Date(date)) %>% 
              filter(total_deaths_smc > 0 | date < as.Date("2020-04-01"))) %>% # last step removes any erroneous entries where the total deaths was reported as zero for the day
  # need to add in dates that aren't listed, which we assume have zero deaths
  full_join(data.frame(date = seq(min(scc_deaths_by_date$date), max(scc_deaths_by_date$date), by = "days"))) %>% 
  arrange(date) %>%
  mutate(new_deaths_smc = replace_na(new_deaths_smc, 0), 
         total_deaths_smc = cumsum(new_deaths_smc),
         new_deaths_scc = replace_na(new_deaths_scc, 0), 
         total_deaths_scc = cumsum(new_deaths_scc),
         new_deaths = new_deaths_scc + new_deaths_smc, 
         total_deaths = cumsum(new_deaths),
         new_deaths_mov7 = movavg(new_deaths, 7, type = "s")) 

# save csv
write.csv(deaths_by_date, "covid19/sv_dash_data_processing/sv_deaths_by_date.csv")

# will use as the race/ethnicity categories: Asian, Black, White, Hispanic/Latino, Pacific Islander, Other, Unknown; these are the categories that SCC uses. SMC gets more detailed, but unfortunately we have to go with the lowest common denominator 
cases_race <- scc_race_cases %>% 
  dplyr::select(race_eth, count) %>%
  # change a few names of the race/ethnicity categories
  mutate(race_eth = case_when(race_eth == "Native Hawaiian & Other Pacific Islander" ~ "Pacific Islander", race_eth == "Latino" ~ "Latino/Hispanic", race_eth == "African American" ~ "Black", TRUE ~ race_eth)) %>%
  rename(count_scc = count) %>%
  # need to coalesce some of the SMC categories because they were more specific than the SCC categories
  left_join(smc_race_data %>% dplyr::select(demographic, Cases) %>%
              rename(count_smc = Cases, race_eth = demographic) %>%
              mutate(race_eth = case_when(race_eth == "American Indian/Alaska Native" ~ "Other", race_eth == "Multirace" ~ "Other", TRUE ~ race_eth)) %>%
              group_by(race_eth) %>%
              summarize(count_smc = sum(count_smc))) %>%
  mutate(total = count_smc + count_scc)

# save csv
write.csv(cases_race, "covid19/sv_dash_data_processing/sv_cases_by_race.csv")

deaths_race <- scc_race_deaths %>%
  dplyr::select(race_eth, counts) %>%
  # change a few names of the race/ethnicity categories
  mutate(race_eth = case_when(race_eth == "Native Hawaiian & Other Pacific Islander" ~ "Pacific Islander", race_eth == "Latino" ~ "Latino/Hispanic", race_eth == "African American" ~ "Black", TRUE ~ race_eth)) %>%
  rename(count_scc = counts) %>%
  # need to coalesce some of the SMC categories because they were more specific than the SCC categories
  left_join(smc_race_data %>% dplyr::select(demographic, Deaths) %>%
              rename(count_smc = Deaths, race_eth = demographic) %>%
              mutate(race_eth = case_when(race_eth == "American Indian/Alaska Native" ~ "Other", race_eth == "Multirace" ~ "Other", TRUE ~ race_eth)) %>%
              group_by(race_eth) %>%
              summarize(count_smc = sum(count_smc))) %>%
  mutate(total = count_smc + count_scc)

# save csv
write.csv(deaths_race, "covid19/sv_dash_data_processing/sv_deaths_by_race.csv")

cases_age <- scc_age_cases %>% 
  dplyr::select(age_group, count) %>%
  rename(count_scc = count) %>%
  left_join(smc_age_data %>% dplyr::select(demographic, Cases) %>%
              rename(count_smc = Cases, age_group = demographic) %>%
              # need to coalesce the <20 age group to fit with SCC data
              mutate(age_group = ifelse(age_group == "< 9" | age_group == "10-19", "19 or under", age_group)) %>%
              group_by(age_group) %>%
              summarize(count_smc = sum(count_smc))) %>%
  mutate(total = count_smc + count_scc)

# save csv
write.csv(cases_age, "covid19/sv_dash_data_processing/sv_cases_by_age.csv")

deaths_age <- scc_age_deaths %>% 
  dplyr::select(age_group, count) %>%
  rename(count_scc = count) %>%
  # some of the names are a little weird so I need to fix them
  mutate(age_group = case_when(age_group == "19 or Under" ~ "19 or under", age_group == "20 - 29" ~ "20-29", TRUE ~ age_group)) %>% 
  left_join(smc_age_data %>% dplyr::select(demographic, Deaths) %>%
              rename(count_smc = Deaths, age_group = demographic) %>%
              # need to coalesce the <20 age group to fit with SCC data
              mutate(age_group = ifelse(age_group == "< 9" | age_group == "10-19", "19 or under", age_group)) %>%
              group_by(age_group) %>%
              summarize(count_smc = sum(count_smc))) %>%
  mutate(total = count_smc + count_scc)

# save csv
write.csv(deaths_age, "covid19/sv_dash_data_processing/sv_deaths_by_age.csv")

hosp_by_date <- scc_hosp_by_date %>% 
  dplyr::select(date, covid_total) %>%  # only using confirmed COVID-19 hospitalizations, not ones under investigation
  rename(hospitalized_scc = covid_total) %>%
  mutate(date = as.Date(date)) %>%
  full_join(smc_data_cfa %>% 
              dplyr::select(date, hospitalized_current, hospitalized) %>%
              # some of the dates have hospitalization numbers listed under "hospitalized_current" and some have them listed under "hospitalized", so need to combine these two into a single column
              mutate(hospitalized_smc = ifelse(is.na(hospitalized_current), hospitalized, hospitalized_current), date = as.Date(date)) %>%
              dplyr::select(date, hospitalized_smc) %>%
              filter(!is.na(hospitalized_smc))) %>% # only include relevant dates
  arrange(date) %>%
  mutate(hospitalized_scc = replace_na(hospitalized_scc, 0),
         hospitalized_smc = replace_na(hospitalized_smc, 0),
         hospitalized = hospitalized_scc + hospitalized_smc)

# save csv
write.csv(hosp_by_date, "covid19/sv_dash_data_processing/sv_hosp_by_date.csv")

testing_by_date <- scc_testing_by_date %>%
  dplyr::select(collection_date, post_rslt, neg_rslt, total) %>%
  rename(pos_scc = post_rslt, neg_scc = neg_rslt, total_scc = total) %>%
  full_join(smc_testing_by_date %>%
              dplyr::select(-cumulative_pos) %>%
              rename(collection_date = date, pos_smc = pos_tests, neg_smc = neg_tests, total_smc = total_tests, perc_pos_smc = perc_pos)) %>%
  # calculate positivity rates and moving average
  mutate(pos_smc = replace_na(pos_smc, 0),
         neg_smc = replace_na(neg_smc, 0),
         total_smc = replace_na(total_smc, 0),
         pos_scc = replace_na(pos_scc, 0),
         neg_scc = replace_na(neg_scc, 0),
         total_scc = replace_na(total_scc, 0),
         perc_pos_scc = pos_scc / total_scc, 
         pos = pos_scc + pos_smc,
         neg = neg_scc + neg_smc,
         total = total_scc + total_smc,
         perc_pos = pos / total,
         perc_pos_mov7 = movavg(perc_pos, 7, type = "s"),
         perc_pos_scc_mov7 = movavg(perc_pos_scc, 7, type = "s"),
         perc_pos_smc_mov7 = movavg(perc_pos_smc, 7, type = "s")) %>%
  dplyr::select(-X1)

# save csv
write.csv(testing_by_date, "covid19/sv_dash_data_processing/sv_testing_by_date.csv")

# get CA data from CA government
ca_data <- read_csv("https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv")

total_ca_cases_by_day <- ca_data %>% 
  # replace any NAs with zero
  mutate(newcountconfirmed = replace_na(newcountconfirmed, 0),
         newcountdeaths = replace_na(newcountdeaths, 0)) %>% 
  group_by(date) %>%
  # summarize all counties in CA in total
  summarize(new_cases = sum(newcountconfirmed),
            new_deaths = sum(newcountdeaths)) %>% 
  ungroup() %>%
  arrange(date) %>%
  mutate(total_cases = cumsum(new_cases), 
         total_deaths = cumsum(new_deaths),
         new_cases_mov7 = movavg(new_cases, 7, type = "s"))

# get US data from NYT
us_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv") %>% 
  mutate(new_cases = c(0, diff(cases)), 
         new_cases_mov7 = movavg(new_cases, 7, type = "s"))

# get world data from WHO
world_data <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv") %>% 
  rename(date = Date_reported) %>%
  group_by(date) %>%
  # summarize all countries in total
  summarize(new_cases = sum(New_cases)) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(total_cases = cumsum(new_cases),
         new_cases_mov7 = movavg(new_cases, 7, type = "s"))

# population data to normalize cases and death counts
smc_pop <- 773244 # source: California Department of Finance (E-1 Estimates for January 2020)
scc_pop <- 1961969 # source: California Department of Finance (E-1 Estimates for January 2020)
ca_pop <- 39782870 # source: California Department of Finance (E-1 Estimates for January 2020)
us_pop <- 329135084 # source: U.S. Census Bureau, Population Clock Estimate
world_pop <- 7795000000 # source: UN Population Fund, World Population Dashboard


# make data frame of the new daily case count for SMC+SCC, CA, and US, normalized by population
comparison_df <- rbind(cases_by_date %>% 
                         dplyr::select(date, new_cases_mov7) %>% 
                         mutate(region = "SMC and SCC", region_pop = smc_pop + scc_pop), 
                       total_ca_cases_by_day %>% 
                         dplyr::select(date, new_cases_mov7) %>% 
                         mutate(region = "CA", region_pop = ca_pop), 
                       us_data %>% 
                         dplyr::select(date, new_cases_mov7) %>% 
                         mutate(region = "US", region_pop = us_pop), 
                       world_data %>% 
                         dplyr::select(date, new_cases_mov7) %>% 
                         mutate(region = "World", region_pop = world_pop)) %>%
  mutate(new_cases_mov7_per_100000 = new_cases_mov7 / region_pop * 100000)

# save csv
write.csv(comparison_df, "covid19/sv_dash_data_processing/comparison_cases_by_date.csv")


# save a csv with the most recent run time
run_time_df <- data.frame(last_time_processed = Sys.time())
write.csv(run_time_df, "covid19/sv_dash_data_processing/last_time_processing_attempted.csv")