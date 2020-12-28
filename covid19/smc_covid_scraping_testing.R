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

for (i in 1:(length(table_vals)/2)) { # only do half of length to not run into issues with catching all values
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
  
  # find the down page key, which should be the one in the farthest x and y position and with correct dimensions
  shift_page_keys <- remDr$findElements(using = "css", value = "[class='unselectable']")
  shift_page_keys_loc <- lapply(shift_page_keys, function(x) x$getElementLocation())
  
  # the down page key has dimensions of 26 height and 9 width, find the key with those
  # dimensions and largest y value
  height_val <- 26
  width_val <- 9
  relevant_y_pos <- sapply(shift_page_keys_loc, function(val) ifelse(val$height == height_val & val$width == width_val, val$y, NA))
  index_max_y <- which.max(relevant_y_pos)
  down_key <- shift_page_keys[[index_max_y]]
  
  processed_days <- length(unique(curr_result$test_date))
  scroll_end <- processed_days * 2 - processed_days / 2
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
  
  # arrange by date
  curr_result <- curr_result %>% arrange(test_date)
  
  # last value's date
  last_date <- curr_result$test_date[nrow(curr_result)]
  
}

# it misses the last values, so run this one more time but only use the second half
# of the picked up table values
processed_days <- length(unique(curr_result$test_date))
scroll_end <- processed_days * 2 - processed_days / 2
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
for (i in (length(table_vals)/2 + 1):length(table_vals)) {
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

remDr$close()