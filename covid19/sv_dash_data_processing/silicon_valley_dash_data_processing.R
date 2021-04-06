# script to create backup stores of the data used in the Silicon Valley COVID-19 Dashboard
# processes the data to create the data frames that go directly into the plots in the dashboard

library(tidyverse)
library(lubridate)
library(jsonlite)
library(pracma)
library(googlesheets4)
options(
  tigris_class = "sf",
  tigris_use_cache = T
)

# DOWNLOAD DATA

# get the SCC data from their open data portal 
# including try() statements in case the data isn't available
scc_cases_by_date <- NULL
try(scc_cases_by_date <- read_csv("https://data.sccgov.org/resource/6cnm-gchg.csv") %>% filter(!is.na(date)))

scc_race_cases <- NULL
try(scc_race_cases <- read_csv("https://data.sccgov.org/resource/ccm2-45w3.csv"))

scc_age_cases <- NULL
try(scc_age_cases <- read_csv("https://data.sccgov.org/resource/ige8-ixqu.csv"))

scc_deaths_by_date <- NULL
try(scc_deaths_by_date <- read_csv("https://data.sccgov.org/resource/tg4j-23y2.csv") %>% filter(!is.na(date)))

scc_race_deaths <- NULL
try(scc_race_deaths <- read_csv("https://data.sccgov.org/resource/nd69-4zii.csv"))

scc_age_deaths <- NULL
try(scc_age_deaths <- read_csv("https://data.sccgov.org/resource/pg8z-gbgv.csv"))

scc_hosp_by_date <- NULL
try(scc_hosp_by_date <- read_csv("https://data.sccgov.org/resource/5xkz-6esm.csv") %>% filter(!is.na(date)))

scc_testing_by_date <- NULL
try(scc_testing_by_date <- read_csv("https://data.sccgov.org/resource/dvgc-tzgq.csv") %>% filter(!is.na(collection_date)))

# get the SMC data
# SMC testing, cases, and demographics data from scraped data from the SMC dashboards 
smc_testing_by_date <- NULL
try(smc_testing_by_date <- read_csv("https://raw.githubusercontent.com/stanfordfuturebay/stanfordfuturebay.github.io/master/covid19/smc_tests_scraped.csv"))

smc_cases_by_date <- NULL
try(smc_cases_by_date <- read_csv("https://raw.githubusercontent.com/stanfordfuturebay/stanfordfuturebay.github.io/master/covid19/smc_cases_scraped.csv"))

smc_dem_data <- NULL
try(smc_dem_data <- read_csv("https://raw.githubusercontent.com/stanfordfuturebay/stanfordfuturebay.github.io/master/covid19/smc_covid_dem_data_scraped.csv"))

# SMC hospitalization data from the CA Department of Public Health
many_county_hosp_data <- NULL
try(many_county_hosp_data <- read_csv("https://data.chhs.ca.gov/dataset/2df3e19e-9ee4-42a6-a087-9761f82033f6/resource/47af979d-8685-4981-bced-96a6b79d3ed5/download/covid19hospitalbycounty.csv", col_types = "cDddddddd"))
smc_hosp_by_date <- NULL
try(smc_hosp_by_date <- many_county_hosp_data %>% filter(county == "San Mateo"))

# SMC deaths data from the NY Times
us_county_data_nyt <- NULL
try(us_county_data_nyt <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
smc_deaths_by_date <- NULL
try(smc_deaths_by_date <- us_county_data_nyt %>% filter(county == "San Mateo"))

# get CA data from CA government
ca_data <- NULL
try(ca_data <- read_csv("https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv"))

# get US data from NYT
us_data <- NULL
try(us_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"))

# get world data from WHO
world_data <- NULL
try(world_data <- read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv"))



# PROCESS
# now process data for each graph, only running if the data for both SMC and SCC
# needed for that graph was actually available to be pulled
# note that I grouped all the demographics data into one set for ease of updates,
# including being able to more easily note the data update time

# CASES OVER TIME
if (!is.null(scc_cases_by_date) & !is.null(smc_cases_by_date) & 
    (nrow(scc_cases_by_date) > 0) & (nrow(smc_cases_by_date) > 0)) {
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
  
  # save csv for the update dates
  case_update_date <- c(as.Date(max(scc_cases_by_date$date)), max(smc_cases_by_date$date))
  county_name <- c("SCC", "SMC")
  case_update_date_df <- data.frame(county_name, case_update_date)
  write.csv(case_update_date_df, "covid19/sv_dash_data_processing/sv_cases_update_dates.csv")
}

# DEATHS OVER TIME
if (!is.null(scc_deaths_by_date) & !is.null(smc_deaths_by_date) & 
    (nrow(scc_deaths_by_date) > 0) & (nrow(smc_deaths_by_date) > 0)) {
  deaths_by_date <- scc_deaths_by_date %>% 
    dplyr::select(date, total, cumulative) %>% rename(new_deaths_scc = total, total_deaths_scc = cumulative) %>% 
    full_join(smc_deaths_by_date %>% 
                dplyr::select(date, deaths) %>%
                rename(total_deaths_smc = deaths) %>% 
                mutate(new_deaths_smc = c(NA, diff(total_deaths_smc)), date = as.Date(date))) %>%
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

  # save csv for the update dates
  deaths_update_date <- c(as.Date(max(scc_deaths_by_date$date)), max(smc_deaths_by_date$date))
  county_name <- c("SCC", "SMC")
  deaths_update_date_df <- data.frame(county_name, deaths_update_date)
  write.csv(deaths_update_date_df, "covid19/sv_dash_data_processing/sv_deaths_update_dates.csv") 
}

# CASES AND DEATHS DEMOGRAPHICS
if (!is.null(scc_age_cases) & !is.null(scc_race_cases) & !is.null(scc_age_deaths) & 
    !is.null(scc_race_deaths) & !is.null(smc_dem_data) & (nrow(scc_age_cases) > 0) & 
    (nrow(scc_race_cases) > 0) & (nrow(scc_age_deaths) > 0) & 
    (nrow(scc_race_deaths) > 0) & (nrow(smc_dem_data) > 0)) {
  
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
  smc_case_update_date_dem <- parse_date_time(sub("as of.*", "", smc_case_update_date_str$case_update_date_str), orders = "mdy")
  
  # save demographic update times for SMC
  update_date <- c(smc_case_update_date_dem, smc_death_update_date_dem)
  data_type <- c("cases demographics", "deaths demographics")
  dem_update_times <- data.frame(data_type, update_date)
  # save csv
  write.csv(dem_update_times, "covid19/sv_dash_data_processing/smc_dem_data_update_dates.csv")
  
  # will use as the race/ethnicity categories: Asian, Black, White, Hispanic/Latino,
  # Pacific Islander, Other, Unknown; these are the categories that SCC uses. 
  # SMC gets more detailed, but unfortunately we have to go with the lowest common denominator 
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
    mutate(count_smc = replace_na(count_smc, 0),
           total = count_smc + count_scc)
  
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
    mutate(count_smc = replace_na(count_smc, 0),
           total = count_smc + count_scc)
  
  # save csv
  write.csv(deaths_race, "covid19/sv_dash_data_processing/sv_deaths_by_race.csv")
  
  cases_age <- scc_age_cases %>% 
    dplyr::select(age_group, count) %>%
    rename(count_scc = count) %>%
    mutate(age_group = ifelse(age_group == "19 or Under", "19 or under", age_group)) %>%
    left_join(smc_age_data %>% dplyr::select(demographic, Cases) %>%
                rename(count_smc = Cases, age_group = demographic) %>%
                # need to coalesce the <20 age group to fit with SCC data
                mutate(age_group = ifelse(age_group == "0-9" | age_group == "10-19", "19 or under", age_group)) %>%
                group_by(age_group) %>%
                summarize(count_smc = sum(count_smc))) %>%
    mutate(count_smc = replace_na(count_smc, 0),
           total = count_smc + count_scc)
  
  # save csv
  write.csv(cases_age, "covid19/sv_dash_data_processing/sv_cases_by_age.csv")
  
  deaths_age <- scc_age_deaths %>% 
    dplyr::select(age_group, count) %>%
    rename(count_scc = count) %>%
    # some of the names are a little weird so I need to fix them
    mutate(age_group = case_when(age_group == "19 or Under" ~ "19 or under", 
                                 age_group == "20 - 29" ~ "20-29", 
                                 age_group == "Unkown" ~ "Unknown",
                                 TRUE ~ age_group)) %>% 
    group_by(age_group) %>%
    summarize(count_scc = sum(count_scc)) %>%
    left_join(smc_age_data %>% dplyr::select(demographic, Deaths) %>%
                rename(count_smc = Deaths, age_group = demographic) %>%
                # need to coalesce the <20 age group to fit with SCC data
                mutate(age_group = ifelse(age_group == "0-9" | age_group == "10-19", "19 or under", age_group)) %>%
                group_by(age_group) %>%
                summarize(count_smc = sum(count_smc))) %>%
    mutate(count_smc = replace_na(count_smc, 0),
           total = count_smc + count_scc)
  
  # save csv
  write.csv(deaths_age, "covid19/sv_dash_data_processing/sv_deaths_by_age.csv")
}

# HOSPITALIZATIONS
if (!is.null(scc_hosp_by_date) & !is.null(smc_hosp_by_date) & 
    (nrow(scc_hosp_by_date) > 0) & (nrow(smc_hosp_by_date) > 0)) {
  hosp_by_date <- scc_hosp_by_date %>% 
    dplyr::select(date, covid_total) %>%  # only using confirmed COVID-19 hospitalizations, not ones under investigation. includes ICU
    rename(hospitalized_scc = covid_total) %>%
    mutate(date = as.Date(date)) %>%
    full_join(smc_hosp_by_date %>% 
                dplyr::select(todays_date, hospitalized_covid_confirmed_patients) %>% # only using confirmed COVID-19 hospitalizations, not ones under investigation. includes ICU
                rename(date = todays_date, hospitalized_smc = hospitalized_covid_confirmed_patients) %>%
                filter(!is.na(hospitalized_smc))) %>% # only include relevant dates
    arrange(date) %>%
    mutate(hospitalized_scc = replace_na(hospitalized_scc, 0),
           hospitalized_smc = replace_na(hospitalized_smc, 0),
           hospitalized = hospitalized_scc + hospitalized_smc)
  
  # save csv
  write.csv(hosp_by_date, "covid19/sv_dash_data_processing/sv_hosp_by_date.csv")
  
  # save csv for the update dates
  hosp_update_date <- c(as.Date(max(scc_hosp_by_date$date)), max(smc_hosp_by_date$todays_date))
  county_name <- c("SCC", "SMC")
  hosp_update_date_df <- data.frame(county_name, hosp_update_date)
  write.csv(hosp_update_date_df, "covid19/sv_dash_data_processing/sv_hosp_update_dates.csv") 
}

# TESTING
if (!is.null(scc_testing_by_date) & !is.null(smc_testing_by_date) & 
    (nrow(scc_testing_by_date) > 0) & (nrow(smc_testing_by_date) > 0)) {
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

  # save csv for the update dates
  testing_update_date <- c(as.Date(max(scc_testing_by_date$collection_date)), max(smc_testing_by_date$date))
  county_name <- c("SCC", "SMC")
  testing_update_date_df <- data.frame(county_name, testing_update_date)
  write.csv(testing_update_date_df, "covid19/sv_dash_data_processing/sv_testing_update_dates.csv") 
}

# COMPARISONS
if (!is.null(scc_cases_by_date) & !is.null(smc_cases_by_date) & !is.null(ca_data) & 
    !is.null(us_data) & !is.null(world_data) & (nrow(scc_cases_by_date) > 0) & 
    (nrow(smc_cases_by_date) > 0) & (nrow(ca_data) > 0) & (nrow(us_data) > 0) & 
    (nrow(world_data) > 0)) {
  
  # process SMC and SCC data
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
  
  # process CA data
  total_ca_cases_by_day <- ca_data %>% 
    # select only data that is summarized for the state level
    filter(area_type == "State" & !is.na(date)) %>%
    arrange(date) %>%
    mutate(total_cases = cumsum(reported_cases),
           new_cases_mov7 = movavg(reported_cases, 7, type = "s"))
    
    # # replace any NAs with zero
    # mutate(newcountconfirmed = replace_na(newcountconfirmed, 0),
    #        newcountdeaths = replace_na(newcountdeaths, 0)) %>% 
    # group_by(date) %>%
    # # summarize all counties in CA in total
    # summarize(new_cases = sum(newcountconfirmed),
    #           new_deaths = sum(newcountdeaths)) %>% 
    # ungroup() %>%
    # arrange(date) %>%
    # mutate(total_cases = cumsum(new_cases), 
    #        total_deaths = cumsum(new_deaths),
    #        new_cases_mov7 = movavg(new_cases, 7, type = "s"))
  
  # process US data
  us_data <- us_data %>%
    mutate(new_cases = c(0, diff(cases)),
           new_cases_mov7 = movavg(new_cases, 7, type = "s"))
  
  # process world data
  world_data <- world_data %>%
    rename(date = Date_reported) %>%
    group_by(date) %>%
    # summarize all countries in total
    summarize(new_cases = sum(New_cases)) %>%
    ungroup() %>%
    arrange(date) %>%
    mutate(total_cases = cumsum(new_cases),
           new_cases_mov7 = movavg(new_cases, 7, type = "s"))
  
  # population data to normalize cases and death counts from JVSV's google sheets with this info
  gs4_deauth()
  pop_vals <- read_sheet("https://docs.google.com/spreadsheets/d/1wUUUTglF5sTRFWIeUIWvHYrCFSjY7eoKZSy3B_ArIZA/edit?ts=5f47d197#gid=0")
  smc_pop <- (pop_vals %>% filter(Geography == "San Mateo County"))$Estimate
  scc_pop <- (pop_vals %>% filter(Geography == "Santa Clara County"))$Estimate
  ca_pop <- (pop_vals %>% filter(Geography == "California"))$Estimate
  us_pop <- (pop_vals %>% filter(Geography == "United States"))$Estimate
  world_pop <- (pop_vals %>% filter(Geography == "World"))$Estimate
  
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
  
  # save csv for the update dates
  comp_update_date <- c(as.Date(max(scc_cases_by_date$date)), max(smc_cases_by_date$date),
                        max(total_ca_cases_by_day$date), max(us_data$date),
                        max(world_data$date))
  region_name <- c("SCC", "SMC", "CA", "US", "World")
  comp_update_date_df <- data.frame(region_name, comp_update_date)
  write.csv(comp_update_date_df, "covid19/sv_dash_data_processing/comparison_cases_update_dates.csv") 
}

# save a csv with the most recent run time
run_time_df <- data.frame(last_time_processed = Sys.time())
write.csv(run_time_df, "covid19/sv_dash_data_processing/last_time_processing_attempted.csv")