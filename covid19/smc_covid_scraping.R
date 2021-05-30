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

# 5/13/21 redone code based on the updated dashboards
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
result_vals <- vector(mode = "list")
date_vals <- NULL
# start a loop to repeatedly process, then scroll down until all values are captured

# first need to do this once outside of the loop
table <- remDr$findElements(using = "css", value = "[class='bodyCells']")
table_vals <- table[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive tablixAlignRight ']")

curr_result <- NULL

# get all the values in the table
for (i in 1:length(table_vals)) {
  curr_val <- table_vals[[i]]
  
  curr_result <- c(curr_result, curr_val$getElementAttribute("title")[[1]])
}

# process - values are in chunks of 20, with the first 20 positive values,
# the next 20 negative values, and the last 20 inconclusive values
for (i in seq(1, length(curr_result), by = 60)) {
  curr_result_subset_pos <- curr_result[i:(i+19)]
  curr_result_subset_neg <- curr_result[(i+20):(i+39)]
  curr_result_subset_inc <- curr_result[(i+40):(i+59)]
  curr_result_vals <- data.frame(Positive = curr_result_subset_pos,
                                 Negative = curr_result_subset_neg,
                                 Inconclusive = curr_result_subset_inc)
  # store as entry in the list 
  result_vals[[length(result_vals) + 1]] <- curr_result_vals
}

# get date values
row_headers <- remDr$findElements(using = "css", value = "[class='rowHeaders']")
row_header_vals <- row_headers[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive ']")  

curr_dates <- NULL

# get the date values
for (i in 1:length(row_header_vals)) {
  curr_val <- row_header_vals[[i]]
  
  curr_dates <- c(curr_dates, curr_val$getElementAttribute("title")[[1]])
}

date_vals <- c(date_vals, curr_dates)

# find the down page key
shift_page_keys <- remDr$findElements(using = "css", value = "[class='unselectable']")
# the down page key is the 7th one
down_key <- shift_page_keys[[7]]

processed_days <- length(curr_result) / 3
scroll_end <- processed_days / 3 # had some trial and error to find a scroll value that worked

# scroll down
for (i in 1:scroll_end) {
  remDr$mouseMoveToLocation(webElement = down_key)
  remDr$click()
}

# now start the loop, continue running until get NA values
while (!is.na(curr_result_subset_inc[length(curr_result_subset_inc)])) {
  # first need to do this once outside of the loop
  table <- remDr$findElements(using = "css", value = "[class='bodyCells']")
  table_vals <- table[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive tablixAlignRight ']")
  
  curr_result <- NULL
  
  # get all the values in the table
  for (i in 1:length(table_vals)) {
    curr_val <- table_vals[[i]]
    
    curr_result <- c(curr_result, curr_val$getElementAttribute("title")[[1]])
  }
  
  # process - values are in chunks of 20, with the first 20 positive values,
  # the next 20 negative values, and the last 20 inconclusive values
  # note this doesn't work correctly for the last set (so need to edit that later)
  for (i in seq(1, length(curr_result), by = 60)) {
    curr_result_subset_pos <- curr_result[i:(i+19)]
    curr_result_subset_neg <- curr_result[(i+20):(i+39)]
    curr_result_subset_inc <- curr_result[(i+40):(i+59)]
    curr_result_vals <- data.frame(Positive = curr_result_subset_pos,
                                   Negative = curr_result_subset_neg,
                                   Inconclusive = curr_result_subset_inc)
    # store as entry in the list 
    result_vals[[length(result_vals) + 1]] <- curr_result_vals
  }
  
  # get date values
  row_headers <- remDr$findElements(using = "css", value = "[class='rowHeaders']")
  row_header_vals <- row_headers[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive ']")  
  
  curr_dates <- NULL
  
  # get the date values
  for (i in 1:length(row_header_vals)) {
    curr_val <- row_header_vals[[i]]
    
    curr_dates <- c(curr_dates, curr_val$getElementAttribute("title")[[1]])
  }
  
  date_vals <- c(date_vals, curr_dates)
  
  # find the down page key
  shift_page_keys <- remDr$findElements(using = "css", value = "[class='unselectable']")
  # the down page key is the 7th one
  down_key <- shift_page_keys[[7]]
  
  processed_days <- length(curr_result) / 3
  scroll_end <- processed_days / 3 # had some trial and error to find a scroll value that worked
  
  # scroll down
  for (i in 1:scroll_end) {
    remDr$mouseMoveToLocation(webElement = down_key)
    remDr$click()
  }
}

# get date values one more time just in case
# get date values
row_headers <- remDr$findElements(using = "css", value = "[class='rowHeaders']")
row_header_vals <- row_headers[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive ']")  

curr_dates <- NULL

# get the date values
for (i in 1:length(row_header_vals)) {
  curr_val <- row_header_vals[[i]]
  
  curr_dates <- c(curr_dates, curr_val$getElementAttribute("title")[[1]])
}

date_vals <- c(date_vals, curr_dates)

# get unique results
result_vals_unique <- unique(result_vals)
date_vals_unique <- unique(date_vals)
# get dates in date form and in ascending order
date_vals_unique <- sort(as.Date(date_vals_unique, "%m/%d/%Y"))

# get results as data frame
result_vals_joined <- NULL
for (i in 1:length(result_vals_unique)) {
  curr_result_vals <- result_vals_unique[[i]]
  # only bind if current results don't have any NA values
  if (!any(is.na(curr_result_vals)))
  result_vals_joined <- rbind(result_vals_joined, curr_result_vals)
}

# deal with any NA values - the last set of values (containing NAs) is not
# handled correctly, so we need to split them up manually, putting the
# values (by threes) in positive, negative, an inconclusive columns respectively

# # first only save values without NAs
# tests_smc <- result_vals_joined %>%
#   filter(!is.na(Positive) & !is.na(Negative) & !is.na(Inconclusive))
tests_smc <- result_vals_joined

# handle the last set of values
# vals_to_handle <- result_vals_joined %>%
#   filter(is.na(Positive) | is.na(Negative) | is.na(Inconclusive)) %>%
#   unlist()
vals_to_handle <- result_vals_unique[[length(result_vals_unique)]]

# get values to actually save (non-NAs)
vals_to_handle <- vals_to_handle[!is.na(vals_to_handle)]

# store as data frame, appropriately separating and labeling columns
# vals_to_handle_df <- data.frame(Positive = vals_to_handle[seq(1, length(vals_to_handle) - 2, by = 3)],
#                                 Negative = vals_to_handle[seq(2, length(vals_to_handle) - 1, by = 3)],
#                                 Inconclusive = vals_to_handle[seq(3, length(vals_to_handle), by = 3)])
vals_to_handle_df <- data.frame(Positive = vals_to_handle[seq(1, length(vals_to_handle) / 3, by = 1)],
                                Negative = vals_to_handle[seq(length(vals_to_handle) / 3 + 1, 2 * length(vals_to_handle) / 3, by = 1)],
                                Inconclusive = vals_to_handle[seq(length(vals_to_handle)*2 / 3 + 1, length(vals_to_handle), by = 1)])

# join to rest of results
tests_smc <- rbind(tests_smc, vals_to_handle_df)

# also add date values
tests_smc$date <- date_vals_unique

# calculate cumulative values
tests_smc <- tests_smc %>%
  dplyr::select(date, Positive, Negative, Inconclusive) %>%
  # change to numeric
  mutate(Positive = as.numeric(str_remove(Positive, ",")),
         Negative = as.numeric(str_remove(Negative, ",")),
         Inconclusive = as.numeric(str_remove(Inconclusive, ",")),) %>%
  rename(pos_tests = Positive, neg_tests = Negative, inconclusive_tests = Inconclusive) %>%
  mutate(cumulative_pos = cumsum(pos_tests), # get cumulative positive tests
         total_tests = pos_tests + neg_tests + inconclusive_tests, # total tests
         perc_pos = pos_tests / total_tests) # percent positive tests

write.csv(tests_smc, "covid19/smc_tests_scraped.csv")


# now get demographic data
remDr$navigate("https://app.powerbigov.us/view?r=eyJrIjoiODZkYzM4MGYtNDkxNC00Y2ZmLWIyYTUtMDNhZjlmMjkyYmJkIiwidCI6IjBkZmFmNjM1LWEwNGQtNDhjYy1hN2UzLTZkYTFhZjA4ODNmOSJ9")
Sys.sleep(10)
# make sure historical is selected
webElem <- remDr$findElements(using = "css", ".allow-deferred-rendering .themableBackgroundColor") # these are the buttons that change between historical and last 30 days
webElem[[2]]$clickElement() # click the historical button
Sys.sleep(10)


# function to find the demographic data
findDemData <- function() {
  
  # switch to larger view of table
  buttons_switch <- remDr$findElements(using = "css", value = "[class='glyphicon pbi-glyph-rotatevertical glyph-small']")
  remDr$mouseMoveToLocation(webElement = buttons_switch[[1]])
  remDr$click()
  
  # get values first
  result_vals <- NULL
  
  # pull values
  table <- remDr$findElements(using = "css", value = "[class='bodyCells']")
  table_vals <- table[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive tablixAlignRight ']")
  
  for (i in 1:length(table_vals)) { 
    curr_val <- table_vals[[i]]
    
    result_vals <- c(result_vals, curr_val$getElementAttribute("title")[[1]])
  }
  
  # get row labels
  result_labels <- NULL
  
  # get date values
  row_headers <- remDr$findElements(using = "css", value = "[class='rowHeaders']")
  row_header_vals <- row_headers[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive ']")  
  
  for (i in 1:length(row_header_vals)) { 
    curr_val <- row_header_vals[[i]]
    
    result_labels <- c(result_labels, curr_val$getElementAttribute("title")[[1]])
  }
  
  result <- data.frame(demographic = result_labels, value = result_vals)
  
  # go back to main dashboard
  return_button <- remDr$findElement(using = "css", value = "[class='menuItem']")
  remDr$mouseMoveToLocation(webElement = return_button)
  remDr$click()
  
  return(result)
}

# find all demographic data bars
dem_data_bars <- remDr$findElements(using = "css", value = "[class='bar setFocusRing']")
# note that the bars correspond to: 
# bars 1-10: age and cases data
# bars 11-13: sex and cases data
# bars 14-23: age and deaths data
# bars 24-25: sex and deaths data
# bars 26-33: race and deaths data
# bars 34-42: race and cases data
# will use ones in the middle of those chunks for the respective demographic groups,
# in case they get shifted a bit
age_cases_index <- 5
age_deaths_index <- 18
race_deaths_index <- 29
race_cases_index <- 38

# find the cases and age data
# pull up the table view
selected_elem <- dem_data_bars[[age_cases_index]]
remDr$mouseMoveToLocation(webElement = selected_elem)
selected_elem$sendKeysToElement(list(key = "shift", key = "f10"))
show_as_table <- remDr$findElement(using = "css", value = "[title='Show as a table']")
show_as_table$clickElement()
# find resulting data
cases_age_result <- findDemData() %>%
  mutate(demographic = paste0("Age Group ", demographic)) %>%
  rename(Cases = value)

# find the cases and race/ethnicity data
# pull up the table view
selected_elem <- dem_data_bars[[race_cases_index]]
remDr$mouseMoveToLocation(webElement = selected_elem)
selected_elem$sendKeysToElement(list(key = "shift", key = "f10"))
show_as_table <- remDr$findElement(using = "css", value = "[title='Show as a table']")
show_as_table$clickElement()
# find resulting data
cases_race_result <- findDemData() %>%
  mutate(demographic = paste0("Race/Ethnicity ", demographic)) %>%
  rename(Cases = value)

# find the deaths and age data
# pull up the table view
selected_elem <- dem_data_bars[[age_deaths_index]]
remDr$mouseMoveToLocation(webElement = selected_elem)
selected_elem$sendKeysToElement(list(key = "shift", key = "f10"))
show_as_table <- remDr$findElement(using = "css", value = "[title='Show as a table']")
show_as_table$clickElement()
# find resulting data
deaths_age_result <- findDemData() %>%
  mutate(demographic = paste0("Age Group ", demographic)) %>%
  rename(Deaths = value)

# find the deaths and race/ethnicity data
# pull up the table view
selected_elem <- dem_data_bars[[race_deaths_index]]
remDr$mouseMoveToLocation(webElement = selected_elem)
selected_elem$sendKeysToElement(list(key = "shift", key = "f10"))
show_as_table <- remDr$findElement(using = "css", value = "[title='Show as a table']")
show_as_table$clickElement()
# find resulting data
deaths_race_result <- findDemData() %>%
  mutate(demographic = paste0("Race/Ethnicity ", demographic)) %>%
  rename(Deaths = value)

age_data <- full_join(cases_age_result %>% 
                        mutate(demographic = ifelse(demographic == "Age Group < 9", "Age Group 0-9", demographic)), 
                      deaths_age_result)
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

write.csv(dem_data_smc_cleaned_with_dates, "covid19/smc_covid_dem_data_scraped.csv")



# now get cases data, from same dashboard
# reload page
remDr$navigate("https://app.powerbigov.us/view?r=eyJrIjoiODZkYzM4MGYtNDkxNC00Y2ZmLWIyYTUtMDNhZjlmMjkyYmJkIiwidCI6IjBkZmFmNjM1LWEwNGQtNDhjYy1hN2UzLTZkYTFhZjA4ODNmOSJ9")
Sys.sleep(10)
# make sure historical is selected
webElem <- remDr$findElements(using = "css", ".allow-deferred-rendering .themableBackgroundColor") # these are the buttons that change between historical and last 30 days
webElem[[2]]$clickElement() # click the historical button
Sys.sleep(10)
# pull up cases data
# find columns in the cases over time chart (note this finds both values in the cases
# by episode date and total cases by episode date chart, but we use an index to select
# a bar in the former graph)
columns <- remDr$findElements(using = "css", value = "[class='column setFocusRing']")
index_to_select <- round(length(columns) / 4,0) # this index is within the daily cases graph, which is the first half of the returned list of columns
column_selected <- columns[[index_to_select]]
# pull up the table view
remDr$mouseMoveToLocation(webElement = column_selected)
column_selected$sendKeysToElement(list(key = "shift", key = "f10"))
show_as_table <- remDr$findElement(using = "css", value = "[title='Show as a table']")
show_as_table$clickElement()

# switch to larger view of table
buttons_switch <- remDr$findElements(using = "css", value = "[class='glyphicon pbi-glyph-rotatevertical glyph-small']")
remDr$mouseMoveToLocation(webElement = buttons_switch[[1]])
remDr$click()

# for this one, we need to expand the table width to be able to see all the values
# on one line, so first find the headers of the table
# dates header
dates_header_parent <- remDr$findElements(using = "css", value = "[class='corner']")
dates_header <- dates_header_parent[[1]]$findChildElement(using = "css", value = "[class='pivotTableCellWrap cell-interactive '")
# get location
dates_header_loc <- dates_header$getElementSize()
# move the mouse to the right-most location of the header
remDr$mouseMoveToLocation(webElement = dates_header) # moves to location of header
remDr$mouseMoveToLocation(x = round(0.5*dates_header_loc$width, 0) - 1, y = 0) # moves from previous location to the edge of the header
# press and hold, move mouse to drag that header to make it bigger
remDr$buttondown()
remDr$mouseMoveToLocation(x = round(0.5*dates_header_loc$width, 0), y = 0)
remDr$buttonup()
# repeat for the values column
# values header
values_header_parent <- remDr$findElements(using = "css", value = "[class='columnHeaders']")
values_header <- values_header_parent[[1]]$findChildElement(using = "css", value = "[class='pivotTableCellWrap cell-interactive '")
values_header_loc <- values_header$getElementSize()
remDr$mouseMoveToLocation(webElement = values_header) # moves to location of header
remDr$mouseMoveToLocation(x = round(0.5*values_header_loc$width, 0), y = 0) # moves from previous location to the edge of the header
# press and hold, move mouse to drag that header to make it bigger
remDr$buttondown()
# need to offset by more since this column starts smaller
remDr$mouseMoveToLocation(x = 2*values_header_loc$width, y = 0)
remDr$buttonup()
# move mouse away
remDr$mouseMoveToLocation(x = 10, y = 0)
remDr$click()

Sys.sleep(5)

# now find values in the table - start at the top and scroll down
cases_result_vals <- vector(mode = "list")
cases_date_vals <- NULL
# start a loop to repeatedly process, then scroll down until all values are captured

# first need to do this once outside of the loop
table <- remDr$findElements(using = "css", value = "[class='bodyCells']")
table_vals <- table[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive tablixAlignRight ']")

curr_result <- NULL

# get all the values in the table, up to the first 20 to not repeat
for (i in 1:min(length(table_vals), 20)) {
  curr_val <- table_vals[[i]]
  
  curr_result <- c(curr_result, curr_val$getElementAttribute("title")[[1]])
}

# store
cases_result_vals[[length(cases_result_vals) + 1]] <- curr_result

# get date values
row_headers <- remDr$findElements(using = "css", value = "[class='rowHeaders']")
row_header_vals <- row_headers[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive ']")  

curr_dates <- NULL

# get the date values
for (i in 1:length(row_header_vals)) {
  curr_val <- row_header_vals[[i]]
  
  curr_dates <- c(curr_dates, curr_val$getElementAttribute("title")[[1]])
}

cases_date_vals <- c(cases_date_vals, curr_dates)

# find the down page key
shift_page_keys <- remDr$findElements(using = "css", value = "[class='unselectable']")
# the down page key is the 7th one
down_key <- shift_page_keys[[7]]

processed_days <- length(curr_result)
scroll_end <- processed_days*2 / 3 # had some trial and error to find a scroll value that worked

# scroll down
for (i in 1:scroll_end) {
  remDr$mouseMoveToLocation(webElement = down_key)
  remDr$click()
}

# now start the loop, continue running until get repeated entries
while (length(cases_result_vals) < 3 || 
       length(cases_result_vals[[length(cases_result_vals)]]) != 
       length(cases_result_vals[[length(cases_result_vals) - 1]]) || 
       length(cases_result_vals[[length(cases_result_vals)]]) != 
       length(cases_result_vals[[length(cases_result_vals) - 2]]) ||
       !(all(cases_result_vals[[length(cases_result_vals)]] == cases_result_vals[[length(cases_result_vals) - 1]]) & 
         all(cases_result_vals[[length(cases_result_vals)]] == cases_result_vals[[length(cases_result_vals) - 2]]))) {
  table <- remDr$findElements(using = "css", value = "[class='bodyCells']")
  table_vals <- table[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive tablixAlignRight ']")
  
  curr_result <- NULL
  
  # get all the values in the table, up to the first 20 to not repeat
  for (i in 1:min(length(table_vals), 20)) {
    curr_val <- table_vals[[i]]
    
    curr_result <- c(curr_result, curr_val$getElementAttribute("title")[[1]])
  }
  
  # store
  cases_result_vals[[length(cases_result_vals) + 1]] <- curr_result
  
  # get date values
  row_headers <- remDr$findElements(using = "css", value = "[class='rowHeaders']")
  row_header_vals <- row_headers[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive ']")  
  
  curr_dates <- NULL
  
  # get the date values
  for (i in 1:length(row_header_vals)) {
    curr_val <- row_header_vals[[i]]
    
    curr_dates <- c(curr_dates, curr_val$getElementAttribute("title")[[1]])
  }
  
  cases_date_vals <- c(cases_date_vals, curr_dates)
  
  # find the down page key
  shift_page_keys <- remDr$findElements(using = "css", value = "[class='unselectable']")
  # the down page key is the 7th one
  down_key <- shift_page_keys[[7]]
  
  processed_days <- length(curr_result)
  scroll_end <- processed_days*2 / 3 # had some trial and error to find a scroll value that worked
  
  # scroll down
  for (i in 1:scroll_end) {
    remDr$mouseMoveToLocation(webElement = down_key)
    remDr$click()
  }
}

# one last time to catch any missed values 
# (ones after the first 20 values in the last set of values)
table <- remDr$findElements(using = "css", value = "[class='bodyCells']")
table_vals <- table[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive tablixAlignRight ']")

curr_result <- NULL

# get any values after the first 20
for (i in 21:length(table_vals)) {
  curr_val <- table_vals[[i]]
  
  curr_result <- c(curr_result, curr_val$getElementAttribute("title")[[1]])
}

# store
cases_result_vals[[length(cases_result_vals) + 1]] <- curr_result

# get unique results
cases_result_vals_unique <- unique(cases_result_vals)
cases_date_vals_unique <- unique(cases_date_vals)
# also convert to date form
cases_date_vals_unique <- sort(as.Date(cases_date_vals_unique, "%A, %B %d, %Y"))

# get results joined
cases_result_vals_joined <- c()
for (i in 1:length(cases_result_vals_unique)) {
  cases_result_vals_joined <- c(cases_result_vals_joined, cases_result_vals_unique[[i]])
}

# make data frame and process
cases_clean <- data.frame(date = cases_date_vals_unique,
                          new_cases = cases_result_vals_joined) %>%
  mutate(total_cases = cumsum(new_cases))

write.csv(cases_clean, "covid19/smc_cases_scraped.csv")

# save a csv with the most recent scrape time
scrape_time_df <- data.frame(scrape_last_time_ran = Sys.time())
write.csv(scrape_time_df, "covid19/smc_scrape_last_time_ran.csv")

remDr$close()