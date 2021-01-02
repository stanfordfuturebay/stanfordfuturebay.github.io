# testing new version of the SMC COVID scraping code - just the cases scraping
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

# manually set width and height to match what I have on my screen when I was creating this code
window_height <- 682
window_width <- 1200
remDr$setWindowSize(window_width, window_height)

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
# values header
values_header_parent <- remDr$findElements(using = "css", value = "[class='columnHeaders']")
values_header <- values_header_parent[[1]]$findChildElement(using = "css", value = "[class='pivotTableCellWrap cell-interactive '")

# start with the dates header
# get location
dates_header_loc <- dates_header$getElementSize()
# move the mouse to the right-most location of the header
remDr$mouseMoveToLocation(webElement = dates_header,
                          # following arguments are the offset from the element
                          x = 0.5*dates_header_loc$width,
                          y = 0)
# press and hold, move mouse
remDr$buttondown()
remDr$mouseMoveToLocation(webElement = dates_header,
                          # following arguments are the offset from the element
                          x = dates_header_loc$width,
                          y = 0)
remDr$buttonup()
# repeat for the values column
values_header_loc <- values_header$getElementSize()
remDr$mouseMoveToLocation(webElement = values_header,
                          x = 0.5*values_header_loc$width,
                          y = 0)
# press and hold, move mouse
remDr$buttondown()
remDr$mouseMoveToLocation(webElement = values_header,
                          # need to offset by more since this column starts smaller
                          x = 4*values_header_loc$width,
                          y = 0)
remDr$buttonup()
# move mouse away
remDr$mouseMoveToLocation(x = 10, y = 0)
remDr$click()

# now find values in the table - start at the top and scroll down
cases_result_vals <- data.frame("episode_date" = character(0),
                                "num_cases" = character(0))

# start a loop to repeatedly process, then scroll down until all values are captured

# first need to do this once outside of the loop
table <- remDr$findElements(using = "css", value = "[class='bodyCells']")
table_vals <- table[[1]]$findChildElements(using = "css", value = "[class='pivotTableCellWrap cell-interactive tablixAlignRight ']")

curr_result <- NULL

Sys.sleep(1)

for (i in 1:length(table_vals)) { 
  curr_val <- table_vals[[i]]
  # move over that value and get relevant parameters
  remDr$mouseMoveToLocation(webElement = curr_val)
  Sys.sleep(1)
  hover_title <- remDr$findElements(using = "css", value = "[class='tooltip-title-cell']")
  hover_value <- remDr$findElements(using = "css", value = "[class='tooltip-value-cell']")
  Sys.sleep(1)
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
  
  Sys.sleep(1)
  
  for (i in length(table_vals):1) {
    
    curr_val <- table_vals[[i]]
    # move over that value and get relevant parameters
    remDr$mouseMoveToLocation(webElement = curr_val)
    Sys.sleep(1)
    hover_title <- remDr$findElements(using = "css", value = "[class='tooltip-title-cell']")
    hover_value <- remDr$findElements(using = "css", value = "[class='tooltip-value-cell']")
    Sys.sleep(1)
    # first entry in title/value corresponds to date, second to the value itself
    curr_result <- rbind(curr_result, data.frame(episode_date = hover_value[[1]]$getElementText() %>% unlist(),
                                                 num_cases = hover_value[[2]]$getElementText() %>% unlist()))
  }
  
  # arrange by date
  curr_result <- curr_result  %>%
    mutate(episode_date = as.Date(episode_date, "%A, %B %d, %Y")) %>%
    arrange(episode_date)
  
  # last value's date
  last_date <- curr_result$episode_date[nrow(curr_result)]
  
}

# only save the unique values in the data frame of results
cases_results_final <- unique(cases_result_vals)

# process a little more and save
cases_clean <- cases_results_final %>%
  rename(date = episode_date,
         new_cases = num_cases) %>%
  mutate(total_cases = cumsum(new_cases))

write.csv(cases_clean, "covid19/smc_cases_scraped_testing.csv")