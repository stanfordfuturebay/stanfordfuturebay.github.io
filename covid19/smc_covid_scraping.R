# this file includes code for scraping the data for covid testing, cases, and demographics (and hospitalizations) from the 
# SMC county dashboards of this data
# for use in Github Actions workflow, with RSelenium loaded already

library(RSelenium)
library(seleniumPipes)
library(tidyverse)
library(dplyr)

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444L,
  path = "/wd/hub",
  browserName = "chrome"
)
remDr$open()

# first getting testing data
remDr$navigate("https://app.powerbigov.us/view?r=eyJrIjoiMWI5NmE5M2ItOTUwMC00NGNmLWEzY2UtOTQyODA1YjQ1NWNlIiwidCI6IjBkZmFmNjM1LWEwNGQtNDhjYy1hN2UzLTZkYTFhZjA4ODNmOSJ9")
Sys.sleep(5)
webElem <- remDr$findElements(using = "class", value = "column") # these correspond to the bars in the bar chart of testing over time

# check if the url was correct and the data is there before scraping and saving the new file
if (length(webElem) != 0) {
# the next chunk of code is from Derek's previous scraping of the lab testing dashboard for SMC
  tests <-
    1:length(webElem) %>% 
    map(function(x){
      webElem[[x]]$getElementAttribute("aria-label") %>% as.character() # aria-label includes the relevant information about the column
    }) %>% 
    unlist() %>% 
    as.data.frame()

  tests_clean <-
    tests %>% 
    rename(text = ".") %>% 
    separate(text, c("date","test_text"), sep = "\\.") %>% 
    separate(test_text, c(NA,"type",NA,"tests")) %>% 
    mutate(
      date = 
        substr(date,23,nchar(.)) %>% 
        as.Date("%A, %B %d, %Y"),
      tests = 
        tests %>% 
        as.numeric()
    ) %>% 
    spread(
      key = type,
      value = tests
    )

  tests_smc <- tests_clean %>% 
    dplyr::select(date, Positive, Negative) %>% 
    rename(pos_tests = Positive, neg_tests = Negative) %>%
    mutate(cumulative_pos = cumsum(pos_tests), # get cumulative positive tests
          total_tests = pos_tests + neg_tests, # total tests
          perc_pos = pos_tests / total_tests) # percent positive tests

  write.csv(tests_smc, "covid19/smc_tests_scraped.csv")
}

# now get demographic data
remDr$navigate("https://app.powerbigov.us/view?r=eyJrIjoiODZkYzM4MGYtNDkxNC00Y2ZmLWIyYTUtMDNhZjlmMjkyYmJkIiwidCI6IjBkZmFmNjM1LWEwNGQtNDhjYy1hN2UzLTZkYTFhZjA4ODNmOSJ9")
Sys.sleep(5)
webElem <- remDr$findElements(using = "class", value = "bar") # these are the elements that correspond to the demographic data bar charts (note they are bars rather than columns)

# again check that the data exists before scraping and saving
if (length(webElem) != 0) {
  dem_data_smc <- 1:length(webElem) %>% 
    map(function(x){
      webElem[[x]]$getElementAttribute("aria-label") %>% as.character()
    }) %>% 
    unlist() %>% 
    as.data.frame()

  dem_data_smc_cleaned <- dem_data_smc %>%
    rename(text = ".") %>% 
    separate(text, c("demographic", "value"), sep = "\\.") %>% 
    separate(value, c(NA, "category", "number")) %>% 
    mutate(number = as.numeric(number),
          # clean a little bit since the format is a little different for a couple of the age group listings between cases and deaths data
          demographic = ifelse(demographic == "Age Group 0 to 9", "Age Group < 9", demographic),
          demographic = ifelse(demographic == "Age Group 10 to 19", "Age Group 10-19", demographic)) %>%
    spread(key = category, value = number)

  # find the text that tells us about update dates
  text_objs <- remDr$findElements(using = "class", value = "textRun")

  # check that the update date information is there
  if (length(text_objs) != 0) {
    text_vals <- 1:length(text_objs) %>% 
      map(function(x){
        text_objs[[x]]$getElementText() %>% unlist()
      }) %>% 
      unlist() %>% 
      as.data.frame()
  
    # find the text with the case update date
    # (which follows the "case data" phrase, at least in the version of the dashboard at this time)
    case_update_date_str <- text_vals %>%
      filter(grepl("case data", tolower(.), fixed = TRUE))
    case_update_date_str <- tolower(case_update_date_str$.)
    if (is_empty(case_update_date_str)) { # if length is zero, no information on case data date was listed
      case_update_date_str <- "no date listed"
    }
  
    # text with death update date
    # (death data update date follows the phrase "death data", at least in the version of the dashboard at this time)
    death_update_date_str <- text_vals %>%
      filter(grepl("death data", tolower(.), fixed = TRUE))
    death_update_date_str <- tolower(death_update_date_str$.)
    if (!is_empty(death_update_date_str)) {
      index_start_death <- unname(str_locate(death_update_date_str, "death data")[1,1])
      death_update_date_str <- substr(death_update_date_str, index_start_death, nchar(death_update_date_str))
    } else { # if length is zero, no information on death data date was listed
      death_update_date_str <- "no date listed"
    }
  
  } else { # no information on the dates
    case_update_date_str <- "no date listed"
    death_update_date_str <- "no date listed"
  }

  # add these to the data frame to be saved
  date_str_vectors <- data.frame(case_update_date_str, death_update_date_str)
  dem_data_smc_cleaned_with_dates <- bind_rows(dem_data_smc_cleaned, date_str_vectors)

  write.csv(dem_data_smc_cleaned_with_dates, "covid19/smc_covid_dem_data_scraped.csv")
}


# also get cases data, from same dashboard
webElem <- remDr$findElements(using = "class", value = "column") # these correspond to the bars in the bar charts of cases over time 

# check the data exists
if (length(webElem) != 0) {
  cases <-
    1:length(webElem) %>% 
    map(function(x){
      webElem[[x]]$getElementAttribute("aria-label") %>% as.character() # aria-label includes the relevant information about the column
    }) %>% 
    unlist() %>% 
    as.data.frame()

  cases_clean <-
    cases %>% 
    rename(text = ".") %>% 
    separate(text, c("date","cases_text"), sep = "\\.") %>% 
    separate(cases_text, c(NA,"type",NA,NA,NA,"cases")) %>% # everything else designated as NA isn't relevant
    mutate(
      date = substr(date,6,nchar(.)) %>% # just get relevant date information
        as.Date("%A, %B %d, %Y"),
      cases = cases %>% as.numeric()) %>%
    spread(key = type, value = cases) %>%
    rename(new_cases = New, total_cases = Total)

  write.csv(cases_clean, "covid19/smc_cases_scraped.csv")
}

# save a csv with the most recent scrape time
scrape_time_df <- data.frame(scrape_last_time_ran = Sys.time())
write.csv(scrape_time_df, "covid19/smc_scrape_last_time_ran.csv")



# code below is to scrape hospital data, which I don't actually keep as active because the SMC dashboard only displays hospital data for the most recent 12 days, so we need to use data from other sources anyway

# # now get hospital data
# remDr$navigate("https://app.powerbigov.us/view?r=eyJrIjoiOWNiMjVlZDAtOWYzYS00ZmYwLTg5MmEtMTViODNlZmFlMTZlIiwidCI6IjBkZmFmNjM1LWEwNGQtNDhjYy1hN2UzLTZkYTFhZjA4ODNmOSJ9")
# Sys.sleep(5)
# webElem <- remDr$findElements(using = "class", value = "column") # similar to testing dashboard, relevant information is in the columns
# hosp_smc <- 1:length(webElem) %>% 
#   map(function(x){
#     webElem[[x]]$getElementAttribute("aria-label") %>% as.character()
#   }) %>% 
#   unlist() %>% 
#   as.data.frame()
# 
# # note that in this, the first set of values correspond to the chart for confirmed cases only (chart on the left in the dashboard). the following ones are for the ICU occupancy and acute care occupancy charts (on the right) 
# hosp_smc_cleaned <- hosp_smc %>% 
#   rename(text = ".") %>% 
#   separate(text, c("date", "description"), sep = "\\.") %>%
#   separate(description, c("description", "number"), sep = -3) %>%
#   mutate(number = as.numeric(gsub("[[:alpha:]]", " ", number)),
#          date = substr(date, 6, length(date)),
#          description = trimws(description)) %>%
#   filter(description == "Acute Care" | description == "ICU") %>%
#   spread(key = description, value = number) %>%
#   mutate(total_hospitalized_covid_confirmed = `Acute Care` + `ICU`)
# 
# write.csv(hosp_smc_cleaned, "covid19/smc_hosp_data_scraped.csv")

remDr$close()