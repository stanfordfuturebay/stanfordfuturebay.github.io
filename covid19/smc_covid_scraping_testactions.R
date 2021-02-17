# just testing if R Selenium in github actions is working
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
remDr$navigate("https://app.powerbi.com/view?r=eyJrIjoiNDIyYWJjM2ItYTgwMi00NjQzLWEzYTItZWMwNzIzOTY2MDUxIiwidCI6IjA0ZWM2MTA5LTRjNzktNGM3My1hZTcxLWE0NzRjMDlhMWY1YSJ9")

remDr$close()