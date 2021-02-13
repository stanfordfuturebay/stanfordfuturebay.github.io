# just testing if R Selenium in github actions is working
library(RSelenium)
library(seleniumPipes)
library(tidyverse)
library(dplyr)

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444L,
  path = "/wd/hub",
  browserName = "firefox"
)
remDr$open()

# first getting testing data
remDr$navigate("https://app.powerbigov.us/view?r=eyJrIjoiMWI5NmE5M2ItOTUwMC00NGNmLWEzY2UtOTQyODA1YjQ1NWNlIiwidCI6IjBkZmFmNjM1LWEwNGQtNDhjYy1hN2UzLTZkYTFhZjA4ODNmOSJ9")

remDr$close()