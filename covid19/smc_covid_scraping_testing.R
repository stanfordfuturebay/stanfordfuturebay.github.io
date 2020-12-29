# testing new version of the SMC COVID scraping code

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
Sys.sleep(10)

# to see all of the testing data, one has to click on the button that says "Historical". The next two lines do that before getting the testing data
webElem <- remDr$findElements(using = "css", ".allow-deferred-rendering .themableBackgroundColor") # these are the buttons that change between historical and last 30 days
webElem[[2]]$clickElement() # click the historical button
Sys.sleep(10)

# pull up the tabular view of data
bars <- remDr$findElements(using = "css", value = "[class='column setFocusRing']") # these correspond to the bars in the bar chart of testing over time
spec_bar <- bars[[100]] # particular bar
remDr$mouseMoveToLocation(webElement = spec_bar) # pick a value in the chart
spec_bar$sendKeysToElement(list(key = "shift", key = "f10"))
show_as_table <- remDr$findElement(using = "css", value = "[title='Show as a table']")
show_as_table$clickElement()

# change view so table is bigger
buttons_switch <- remDr$findElements(using = "css", value = "[class='glyphicon pbi-glyph-rotatevertical glyph-small']")
remDr$mouseMoveToLocation(webElement = buttons_switch[[1]])
remDr$click()

# now find values in the table - start at the top and scroll down
result_vals <- data.frame("test_date" = character(0), 
                          "test_type" = character(0), 
                          "test_value" = character(0)) # will store all final results

# start a loop to repeatedly process, then scroll down until all values are captured

# first need to do this once outside of the loop
table <- remDr$findElements(using = "css", value = "[class='bodyCells']")
table_vals <- table[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive tablixAlignRight ']")

curr_result <- NULL

# forwards for first half
for (i in 1:(length(table_vals)/2)) {
  curr_val <- table_vals[[i]]
  # move over that value and get relevant parameters
  remDr$mouseMoveToLocation(webElement = curr_val)
  Sys.sleep(1)
  hover_title <- remDr$findElements(using = "css", value = "[class='tooltip-title-cell']")
  hover_value <- remDr$findElements(using = "css", value = "[class='tooltip-value-cell']")
  # first entry in title/value corresponds to date, second to the value itself
  curr_result <- rbind(curr_result, data.frame(test_date = hover_value[[1]]$getElementText() %>% unlist(),
                                               test_type = hover_title[[2]]$getElementText() %>% unlist(),
                                               test_value = hover_value[[2]]$getElementText() %>% unlist()))
}

# backwards for second half
for (i in length(table_vals):(length(table_vals)/2 + 1)) {
  curr_val <- table_vals[[i]]
  # move over that value and get relevant parameters
  remDr$mouseMoveToLocation(webElement = curr_val)
  Sys.sleep(1)
  hover_title <- remDr$findElements(using = "css", value = "[class='tooltip-title-cell']")
  hover_value <- remDr$findElements(using = "css", value = "[class='tooltip-value-cell']")
  # first entry in title/value corresponds to date, second to the value itself
  curr_result <- rbind(curr_result, data.frame(test_date = hover_value[[1]]$getElementText() %>% unlist(),
                                               test_type = hover_title[[2]]$getElementText() %>% unlist(),
                                               test_value = hover_value[[2]]$getElementText() %>% unlist()))
}

# arrange by date
curr_result <- curr_result %>% arrange(test_date)

# last value's date
last_date <- curr_result$test_date[nrow(curr_result)]

# while have not recorded that date, scroll down, process next table

while(!(last_date %in% result_vals$test_date)) {
  # bind to full results data frame
  result_vals <- rbind(result_vals, curr_result)
  
  # find the down page key
  shift_page_keys <- remDr$findElements(using = "css", value = "[class='unselectable']")
  # the down page key is the 7th one
  down_key <- shift_page_keys[[7]]
  
  processed_days <- length(unique(curr_result$test_date))
  scroll_end <- processed_days - processed_days / 4
  for (i in 1:scroll_end) {
    remDr$mouseMoveToLocation(webElement = down_key)
    remDr$click()
  }
  
  # get the new table
  table <- remDr$findElements(using = "css", value = "[class='bodyCells']")
  table_vals <- table[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive tablixAlignRight ']")
  
  # process table values
  curr_result <- NULL
  
  for (i in 1:(length(table_vals)/2)) {
    try({
      curr_val <- table_vals[[i]]
      # move over that value and get relevant parameters
      remDr$mouseMoveToLocation(webElement = curr_val)
      Sys.sleep(1)
      hover_title <- remDr$findElements(using = "css", value = "[class='tooltip-title-cell']")
      hover_value <- remDr$findElements(using = "css", value = "[class='tooltip-value-cell']")
      # first entry in title/value corresponds to date, second to the value itself
      curr_result <- rbind(curr_result, data.frame(test_date = hover_value[[1]]$getElementText() %>% unlist(),
                                                   test_type = hover_title[[2]]$getElementText() %>% unlist(),
                                                   test_value = hover_value[[2]]$getElementText() %>% unlist()))
    })
  }
  
  # backwards for second half
  for (i in length(table_vals):(length(table_vals)/2 + 1)) {
    try({
      curr_val <- table_vals[[i]]
      # move over that value and get relevant parameters
      remDr$mouseMoveToLocation(webElement = curr_val)
      Sys.sleep(1)
      hover_title <- remDr$findElements(using = "css", value = "[class='tooltip-title-cell']")
      hover_value <- remDr$findElements(using = "css", value = "[class='tooltip-value-cell']")
      # first entry in title/value corresponds to date, second to the value itself
      curr_result <- rbind(curr_result, data.frame(test_date = hover_value[[1]]$getElementText() %>% unlist(),
                                                   test_type = hover_title[[2]]$getElementText() %>% unlist(),
                                                   test_value = hover_value[[2]]$getElementText() %>% unlist()))
    })
  }
  
  # arrange by date
  curr_result <- curr_result %>% arrange(test_date)
  
  # last value's date
  last_date <- curr_result$test_date[nrow(curr_result)]
  
}

# it misses the last values, so run this one more time but only use the second half
# of the picked up table values
processed_days <- length(unique(curr_result$test_date))
scroll_end <- processed_days - processed_days / 4
for (i in 1:scroll_end) {
  remDr$mouseMoveToLocation(webElement = down_key)
  remDr$click()
}

# get the new table
table <- remDr$findElements(using = "css", value = "[class='bodyCells']")
table_vals <- table[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive tablixAlignRight ']")

# process table values
curr_result <- NULL

# second half of table values
for (i in length(table_vals):(length(table_vals)/2 + 1)) {
  try({
    curr_val <- table_vals[[i]]
    # move over that value and get relevant parameters
    remDr$mouseMoveToLocation(webElement = curr_val)
    Sys.sleep(1)
    hover_title <- remDr$findElements(using = "css", value = "[class='tooltip-title-cell']")
    hover_value <- remDr$findElements(using = "css", value = "[class='tooltip-value-cell']")
    # first entry in title/value corresponds to date, second to the value itself
    curr_result <- rbind(curr_result, data.frame(test_date = hover_value[[1]]$getElementText() %>% unlist(),
                                                 test_type = hover_title[[2]]$getElementText() %>% unlist(),
                                                 test_value = hover_value[[2]]$getElementText() %>% unlist()))
  })
}

# arrange by date
curr_result <- curr_result %>% arrange(test_date)

# bind to full results data frame
result_vals <- rbind(result_vals, curr_result)

# only save the unique values in the data frame of results
results_final <- unique(result_vals)

tests_smc <- results_final %>%
  mutate(date = as.Date(test_date, "%m/%d/%y"),
         test_value = as.numeric(str_remove(test_value, ","))) %>%
  spread(key = test_type, value = test_value) %>%
  dplyr::select(date, Positive, Negative) %>%
  rename(pos_tests = Positive, neg_tests = Negative) %>%
  mutate(cumulative_pos = cumsum(pos_tests), # get cumulative positive tests
         total_tests = pos_tests + neg_tests, # total tests
         perc_pos = pos_tests / total_tests) # percent positive tests

write.csv(tests_smc, "covid19/smc_tests_scraped_testing.csv")

# now get demographic data
remDr$navigate("https://app.powerbigov.us/view?r=eyJrIjoiODZkYzM4MGYtNDkxNC00Y2ZmLWIyYTUtMDNhZjlmMjkyYmJkIiwidCI6IjBkZmFmNjM1LWEwNGQtNDhjYy1hN2UzLTZkYTFhZjA4ODNmOSJ9")
Sys.sleep(10)
# make sure historical is selected
webElem <- remDr$findElements(using = "css", ".allow-deferred-rendering .themableBackgroundColor") # these are the buttons that change between historical and last 30 days
webElem[[2]]$clickElement() # click the historical button
Sys.sleep(10)

# find heading locations of the demographic data
headings <- remDr$findElements(using = "css", value = "[class='preTextWithEllipsis']")
headings_text <- sapply(headings, function(x) x$getElementText() %>% unlist())


# function to find the demographic data
findDemData <- function(heading_name, headings) {
  index_selected <- which(headings_text == heading_name)
  
  # pull up the table view
  remDr$mouseMoveToLocation(webElement = headings[[index_selected]])
  spec_bar$sendKeysToElement(list(key = "shift", key = "f10"))
  show_as_table <- remDr$findElement(using = "css", value = "[title='Show as a table']")
  show_as_table$clickElement()
  
  # switch to larger view of table
  buttons_switch <- remDr$findElements(using = "css", value = "[class='glyphicon pbi-glyph-rotatevertical glyph-small']")
  remDr$mouseMoveToLocation(webElement = buttons_switch[[1]])
  remDr$click()
  
  # pull values
  table <- remDr$findElements(using = "css", value = "[class='bodyCells']")
  table_vals <- table[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive tablixAlignRight ']")
  
  result <- data.frame(demographic = character(0), 
                       value = character(0)) 
  
  for (i in 1:length(table_vals)) { 
    curr_val <- table_vals[[i]]
    # move over that value and get relevant parameters
    remDr$mouseMoveToLocation(webElement = curr_val)
    Sys.sleep(1)
    hover_title <- remDr$findElements(using = "css", value = "[class='tooltip-title-cell']")
    hover_value <- remDr$findElements(using = "css", value = "[class='tooltip-value-cell']")
    # first entry in title/value corresponds to date, second to the value itself
    result <- rbind(result, data.frame(demographic = hover_value[[1]]$getElementText() %>% unlist(),
                                       value = hover_value[[2]]$getElementText() %>% unlist()))
  }
  
  # go back to main dashboard
  return_button <- remDr$findElement(using = "css", value = "[class='menuItem']")
  remDr$mouseMoveToLocation(webElement = return_button)
  remDr$click()
  
  return(result)
}

# find the cases and age data
cases_age_result <- findDemData("Cases by Age Group", headings) %>%
  mutate(demographic = paste0("Age Group ", demographic)) %>%
  rename(Cases = value)

# find the cases and race/ethnicity data
cases_race_result <- findDemData("Cases by Race/Ethnicity", headings) %>%
  mutate(demographic = paste0("Race/Ethnicity ", demographic)) %>%
  rename(Cases = value)

# find the deaths and age data
deaths_age_result <- findDemData("Deaths by Age Group", headings) %>%
  mutate(demographic = paste0("Age Group ", demographic)) %>%
  rename(Deaths = value)

# find the deaths and race/ethnicity data
deaths_race_result <- findDemData("Deaths by Race/Ethnicity", headings) %>%
  mutate(demographic = paste0("Race/Ethnicity ", demographic)) %>%
  rename(Deaths = value)

age_data <- full_join(cases_age_result, 
                      deaths_age_result %>% 
                        mutate(demographic = ifelse(demographic == "Age Group 0 to 9", "Age Group < 9", demographic),
                               demographic = ifelse(demographic == "Age Group 10 to 19", "Age Group 10-19", demographic)))
race_data <- full_join(cases_race_result, deaths_race_result)

dem_data_smc_cleaned <- rbind(age_data, race_data)

# find the update time information
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
  # (which follows the "up to and including" phrase, at least in the version of the dashboard at this time - edited 11/25/20)
  case_update_date_str <- text_vals %>%
    filter(grepl("up to and including", tolower(.), fixed = TRUE))
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

write.csv(dem_data_smc_cleaned_with_dates, "covid19/smc_covid_dem_data_scraped_testing.csv")


# now get cases data, from same dashboard
# pull up cases data
index_daily_cases <- which(headings_text == "Cases by Episode Date")

# pull up the table view
remDr$mouseMoveToLocation(webElement = headings[[index_daily_cases]])
spec_bar$sendKeysToElement(list(key = "shift", key = "f10"))
show_as_table <- remDr$findElement(using = "css", value = "[title='Show as a table']")
show_as_table$clickElement()

# switch to larger view of table
buttons_switch <- remDr$findElements(using = "css", value = "[class='glyphicon pbi-glyph-rotatevertical glyph-small']")
remDr$mouseMoveToLocation(webElement = buttons_switch[[1]])
remDr$click()

# now find values in the table - start at the top and scroll down
cases_result_vals <- data.frame("episode_date" = character(0), 
                                "num_cases" = character(0))

# start a loop to repeatedly process, then scroll down until all values are captured

# first need to do this once outside of the loop
table <- remDr$findElements(using = "css", value = "[class='bodyCells']")
table_vals <- table[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive tablixAlignRight ']")

curr_result <- NULL

for (i in 1:(length(table_vals)/2)) { # only do half of length to not run into issues with catching all values
  curr_val <- table_vals[[i]]
  # move over that value and get relevant parameters
  remDr$mouseMoveToLocation(webElement = curr_val)
  Sys.sleep(1)
  hover_title <- remDr$findElements(using = "css", value = "[class='tooltip-title-cell']")
  hover_value <- remDr$findElements(using = "css", value = "[class='tooltip-value-cell']")
  # first entry in title/value corresponds to date, second to the value itself
  curr_result <- rbind(curr_result, data.frame(episode_date = hover_value[[1]]$getElementText() %>% unlist(),
                                               num_cases = hover_value[[2]]$getElementText() %>% unlist()))
}

# backwards for second half
for (i in length(table_vals):(length(table_vals)/2 + 1)) {
  curr_val <- table_vals[[i]]
  # move over that value and get relevant parameters
  remDr$mouseMoveToLocation(webElement = curr_val)
  Sys.sleep(1)
  hover_title <- remDr$findElements(using = "css", value = "[class='tooltip-title-cell']")
  hover_value <- remDr$findElements(using = "css", value = "[class='tooltip-value-cell']")
  # first entry in title/value corresponds to date, second to the value itself
  curr_result <- rbind(curr_result, data.frame(episode_date = hover_value[[1]]$getElementText() %>% unlist(),
                                               num_cases = hover_value[[2]]$getElementText() %>% unlist()))
}

# arrange by date
curr_result <- curr_result %>% 
  mutate(episode_date = as.Date(episode_date, "%A, %B %d, %Y")) %>%
  arrange(episode_date)

# last value's date
last_date <- curr_result$episode_date[nrow(curr_result)]

# while have not recorded that date, scroll down, process next table

while(!(last_date %in% cases_result_vals$episode_date)) {
  # bind to full results data frame
  cases_result_vals <- rbind(cases_result_vals, curr_result)
  
  # find the down page key
  shift_page_keys <- remDr$findElements(using = "css", value = "[class='unselectable']")
  # the down page key is the 7th one
  down_key <- shift_page_keys[[7]]
  
  processed_days <- length(unique(curr_result$episode_date))
  scroll_end <- processed_days - processed_days / 4
  for (i in 1:scroll_end) {
    remDr$mouseMoveToLocation(webElement = down_key)
    remDr$click()
  }
  
  # get the new table
  table <- remDr$findElements(using = "css", value = "[class='bodyCells']")
  table_vals <- table[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive tablixAlignRight ']")
  
  # process table values
  curr_result <- NULL
  
  for (i in 1:(length(table_vals)/2)) {
    try({
      curr_val <- table_vals[[i]]
      # move over that value and get relevant parameters
      remDr$mouseMoveToLocation(webElement = curr_val)
      Sys.sleep(1)
      hover_title <- remDr$findElements(using = "css", value = "[class='tooltip-title-cell']")
      hover_value <- remDr$findElements(using = "css", value = "[class='tooltip-value-cell']")
      # first entry in title/value corresponds to date, second to the value itself
      curr_result <- rbind(curr_result, data.frame(episode_date = hover_value[[1]]$getElementText() %>% unlist(),
                                                   num_cases = hover_value[[2]]$getElementText() %>% unlist()))
    })
  }
  
  # backwards for second half
  for (i in length(table_vals):(length(table_vals)/2 + 1)) {
    try({
      curr_val <- table_vals[[i]]
      # move over that value and get relevant parameters
      remDr$mouseMoveToLocation(webElement = curr_val)
      Sys.sleep(1)
      hover_title <- remDr$findElements(using = "css", value = "[class='tooltip-title-cell']")
      hover_value <- remDr$findElements(using = "css", value = "[class='tooltip-value-cell']")
      # first entry in title/value corresponds to date, second to the value itself
      curr_result <- rbind(curr_result, data.frame(episode_date = hover_value[[1]]$getElementText() %>% unlist(),
                                                   num_cases = hover_value[[2]]$getElementText() %>% unlist()))
    })
  }
  
  # arrange by date
  curr_result <- curr_result  %>% 
    mutate(episode_date = as.Date(episode_date, "%A, %B %d, %Y")) %>%
    arrange(episode_date)
  
  # last value's date
  last_date <- curr_result$episode_date[nrow(curr_result)]
  
}

# it misses the last values, so run this one more time but only use the second half
# of the picked up table values
processed_days <- length(unique(curr_result$episode_date))
scroll_end <- processed_days - processed_days / 4
for (i in 1:scroll_end) {
  remDr$mouseMoveToLocation(webElement = down_key)
  remDr$click()
}

# get the new table
table <- remDr$findElements(using = "css", value = "[class='bodyCells']")
table_vals <- table[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive tablixAlignRight ']")

# process table values
curr_result <- NULL

# second half of table values
for (i in length(table_vals):(length(table_vals)/2 + 1)) {
  try({
    curr_val <- table_vals[[i]]
    # move over that value and get relevant parameters
    remDr$mouseMoveToLocation(webElement = curr_val)
    Sys.sleep(1)
    hover_title <- remDr$findElements(using = "css", value = "[class='tooltip-title-cell']")
    hover_value <- remDr$findElements(using = "css", value = "[class='tooltip-value-cell']")
    # first entry in title/value corresponds to date, second to the value itself
    curr_result <- rbind(curr_result, data.frame(episode_date = hover_value[[1]]$getElementText() %>% unlist(),
                                                 num_cases = hover_value[[2]]$getElementText() %>% unlist()))
  })
}

# arrange by date
curr_result <- curr_result  %>% 
  mutate(episode_date = as.Date(episode_date, "%A, %B %d, %Y")) %>%
  arrange(episode_date)

# bind to full results data frame
cases_result_vals <- rbind(cases_result_vals, curr_result)

# only save the unique values in the data frame of results
cases_results_final <- unique(cases_result_vals)

# process a little more and save
cases_clean <- cases_results_final %>% 
  rename(date = episode_date,
         new_cases = num_cases) %>%
  mutate(total_cases = cumsum(new_cases))

write.csv(cases_clean, "covid19/smc_cases_scraped_testing.csv")

remDr$close()