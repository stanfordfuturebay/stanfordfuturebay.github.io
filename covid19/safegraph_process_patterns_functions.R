#This script contains a set of functions to process Safegraph weekly patterns data for you.
#To add these functions, simply write:
#source(<address>/safegraph_process_patterns_functions.R)
#You will also need safegraph_normalization_functions.R in the same working directory. This will source all normalization functions too.
#use process_patterns_origins(<patterns>,<home panel summary>) to get visit_counts column in your patterns data.

library(tidyverse)
library(tigris)
library(censusapi)
library(sf)
library(mapview)
library(rjson)

options(
  tigris_class = "sf",
  tigris_use_cache = T
)

source("safegraph_normalization_functions.R")

#Function to do process weekly patterns such that you have individual daily visits for each POI.
#param patterns: Safegraph weekly patterns dataset. Can be pre-filtered by geography or POI type.
#param home_summary: Safegraph home_panel_summary dataset for the same time period.
#returns patterns dataset with individual rows for individual dates, and upper and lower bounds for visit counts.
process_patterns_daily <- function(patterns, home_panel_summary){
  
  # print("Load data")
  
  #Load the SafeGraph patterns dataset.
  sg <- 
    patterns %>% 
    dplyr::select(
      safegraph_place_id,
      date_range_start,
      date_range_end,
      raw_visit_counts,
      raw_visitor_counts,
      visits_by_day,
      visitor_home_cbgs
    )
  
  #Load the SafeGraph home panel summary.
  hps <- home_panel_summary
  
  # print("Normalize data")
  
  # consider speeding up this step by looking for existing result in P drive
  sg_norm <- normBG(sg, hps) 
  
  sum <- 
    sg_norm %>% 
    group_by(safegraph_place_id) %>% 
    summarize(
      visit_counts_high = sum(visit_counts_high, na.rm=T),
      visit_counts_low = sum(visit_counts_low, na.rm=T)
    ) %>% 
    left_join(
      sg %>% 
        dplyr::select(
          safegraph_place_id,
          date_range_start,
          date_range_end,
          visits_by_day,
          raw_visit_counts
        ), 
      by = "safegraph_place_id"
    ) %>%
    mutate(
      date_range_start = date_range_start %>%  substr(1,10) %>% as.Date(),
      date_range_end = date_range_end %>%  substr(1,10) %>% as.Date()
    ) 
  
  # print("Expand daily visits")
  
  day_exploded <- 
    1:nrow(sum) %>% 
    map_dfr(function(i){
      
      # if(i%%100 == 0) print(i)
      
      daily_visits <-
        substr(sum$visits_by_day[i],2,nchar(sum$visits_by_day[i])-1) %>% 
        strsplit(',') %>% 
        .[[1]] %>% 
        as.numeric() %>% 
        as.data.frame() %>% 
        rename(daily_visits = ".") %>% 
        mutate(
          date = sum$date_range_start[i]+0:6,
          safegraph_place_id = sum$safegraph_place_id[i]
        )
    })
  
  day_final <- 
    day_exploded %>%
    left_join(
      sum %>% 
        dplyr::select(
          -date_range_start,
          -date_range_end,
          -visits_by_day
        ), 
      by = "safegraph_place_id"
    ) %>% 
    mutate(
      ratio_high = visit_counts_high/raw_visit_counts,
      ratio_low = visit_counts_low/raw_visit_counts,
      visit_counts_high = daily_visits * ratio_high,
      visit_counts_low = daily_visits * ratio_low
    )
  
  return(day_final)
}

#Function to do process weekly patterns such that you have individual daily visits for each POI, but also broken out by origin.
#param patterns: Safegraph weekly patterns dataset. Can be pre-filtered by geography or POI type.
#param home_summary: Safegraph home_panel_summary dataset for the same time period.
#returns patterns dataset with individual rows for individual dates and origins by POI, and upper and lower bounds for visit counts.
process_patterns_origins_daily <- function(patterns, home_panel_summary){
  
  # print("Load data")
  
  #Load the SafeGraph patterns dataset.
  sg <- 
    patterns %>% 
    dplyr::select(
      safegraph_place_id,
      date_range_start,
      date_range_end,
      raw_visit_counts,
      raw_visitor_counts,
      visits_by_day,
      visitor_home_cbgs
    )
  
  #Load the SafeGraph home panel summary.
  hps <- home_panel_summary
  
  # print("Normalize data")
  
  sg_norm <- normBG(sg, hps) 
  
  sum <- 
    sg_norm %>% 
    mutate(
      date_range_start = date_range_start %>%  substr(1,10) %>% as.Date(),
      date_range_end = date_range_end %>%  substr(1,10) %>% as.Date()
    ) 
  
  # print("Expand daily visits")
  
  day_exploded <- 
    1:nrow(sum) %>%
    map_dfr(function(i){
      
      # if(i%%100 == 0) print(i)
      
      daily_visits <-
        substr(sum$visits_by_day[i],2,nchar(sum$visits_by_day[i])-1) %>% 
        strsplit(',') %>% 
        .[[1]] %>% 
        as.numeric() %>% 
        as.data.frame() %>% 
        rename(daily_visits = ".") %>% 
        mutate(
          date = sum$date_range_start[i]+0:6,
          safegraph_place_id = sum$safegraph_place_id[i],
          origin_census_block_group = sum$origin_census_block_group[i]
        )
    })
  
  day_final <- 
    day_exploded %>%
    left_join(
      sum %>% 
        dplyr::select(
          -date_range_start,
          -date_range_end,
          -visits_by_day
        ), 
      by = c("safegraph_place_id","origin_census_block_group")
    ) %>% 
    mutate(
      ratio_high = visit_counts_high/raw_visit_counts,
      ratio_low = visit_counts_low/raw_visit_counts,
      visit_counts_high = daily_visits * ratio_high,
      visit_counts_low = daily_visits * ratio_low
    )
  
  return(day_final)
}

#Function to do process weekly patterns such that you have individual hourly visits for each POI.
#param patterns: Safegraph weekly patterns dataset. Can be pre-filtered by geography or POI type.
#param home_summary: Safegraph home_panel_summary dataset for the same time period.
#returns patterns dataset with individual rows for individual hours of each day, and upper and lower bounds for visit counts.
process_patterns_hourly <- function(patterns, home_panel_summary){
  
  # print("Load data")
  
  #Load the SafeGraph patterns dataset.
  sg <- 
    patterns %>% 
    dplyr::select(
      safegraph_place_id,
      date_range_start,
      date_range_end,
      raw_visit_counts,
      raw_visitor_counts,
      visits_by_each_hour,
      visitor_home_cbgs
    )
  
  #Load the SafeGraph home panel summary.
  hps <- home_panel_summary
  
  # print("Normalize data")
  
  sg_norm <- normBG(sg, hps) 
  
  sum <- 
    sg_norm %>% 
    group_by(safegraph_place_id) %>% 
    summarize(
      visit_counts_high = sum(visit_counts_high, na.rm=T),
      visit_counts_low = sum(visit_counts_low, na.rm=T)
    ) %>% 
    left_join(
      sg %>% 
        dplyr::select(
          safegraph_place_id,
          date_range_start,
          date_range_end,
          raw_visit_counts,
          visits_by_each_hour
        ), 
      by = "safegraph_place_id"
    ) %>%
    mutate(
      date_range_start = date_range_start %>%  substr(1,10) %>% as.Date(),
      date_range_end = date_range_end %>%  substr(1,10) %>% as.Date()
    ) 
  
  # print("Expand hourly visits")
  
  hour_exploded <- 
    1:nrow(sum) %>% 
    map_dfr(function(i){
      
      # if(i%%100 == 0) print(i)
      
      hourly_visits <-
        substr(sum$visits_by_each_hour[i],2,nchar(sum$visits_by_each_hour[i])-1) %>% 
        strsplit(',') %>% 
        .[[1]] %>% 
        as.numeric() %>% 
        as.data.frame() %>% 
        rename(hourly_visits = ".") %>% 
        mutate(
          date = rep(sum$date_range_start[i]+0:6,each=24),
          hour = rep(1:24,7),
          safegraph_place_id = sum$safegraph_place_id[i]
        )
    })
  
  hour_final <-
    hour_exploded %>%
    left_join(
      sum %>% 
        dplyr::select(
          -date_range_start,
          -date_range_end,
          -visits_by_each_hour
        ), 
      by = "safegraph_place_id"
    ) %>% 
    mutate(
      ratio_high = visit_counts_high/raw_visit_counts,
      ratio_low = visit_counts_low/raw_visit_counts,
      visit_counts_hourly_high = hourly_visits * ratio_high,
      visit_counts_hourly_low = hourly_visits * ratio_low
    )
  
  return(hour_final)
}

#Function to do process weekly patterns such that you have individual hourly visits for each POI, but also broken out by origin.
#param patterns: Safegraph weekly patterns dataset. Can be pre-filtered by geography or POI type.
#param home_summary: Safegraph home_panel_summary dataset for the same time period.
#param sparsifier: int. If sparsifier = -1, then hours with 0 visits are counted. If set to 0, then all 0s are removed.
#returns patterns dataset with individual rows for individual hours and origins by POI, and upper and lower bounds for visit counts.
process_patterns_origins_hourly <- function(patterns, home_panel_summary, sparsifier = -1){
  
  # print("Load data")
  
  #Load the SafeGraph patterns dataset.
  sg <- 
    patterns %>% 
    dplyr::select(
      safegraph_place_id,
      date_range_start,
      date_range_end,
      raw_visit_counts,
      raw_visitor_counts,
      visits_by_each_hour,
      visitor_home_cbgs
    )
  
  #Load the SafeGraph home panel summary.
  hps <- home_panel_summary
  
  # print("Normalize data")
  
  sg_norm <- normBG(sg, hps) 
  
  sum <- 
    sg_norm %>% 
    mutate(
      date_range_start = date_range_start %>%  substr(1,10) %>% as.Date(),
      date_range_end = date_range_end %>%  substr(1,10) %>% as.Date()
    ) 
  
  # print("Expand daily visits")
  
  hour_exploded <- 
    1:nrow(sum) %>% 
    map_dfr(function(i){
      
      # if(i%%100 == 0) print(i)
      
      hourly_visits <-
        substr(sum$visits_by_each_hour[i],2,nchar(sum$visits_by_each_hour[i])-1) %>% 
        strsplit(',') %>% 
        .[[1]] %>% 
        as.numeric() %>% 
        as.data.frame() %>% 
        rename(hourly_visits = ".") %>% 
        mutate(
          date = rep(sum$date_range_start[i]+0:6,each=24),
          hour = rep(1:24,7)) %>%
        filter(hourly_visits > sparsifier) %>%
        mutate(
          safegraph_place_id = sum$safegraph_place_id[i],
          origin_census_block_group = sum$origin_census_block_group[i]
        )
    })
  
  hour_final <-
    hour_exploded %>%
    left_join(
      sum %>% 
        dplyr::select(
          -date_range_start,
          -date_range_end,
          -visits_by_each_hour,
          -visitor_home_cbgs
        ), 
      by = c("safegraph_place_id","origin_census_block_group")
    ) %>% 
    mutate(
      ratio_high = visit_counts_high/raw_visit_counts,
      ratio_low = visit_counts_low/raw_visit_counts,
      visit_counts_hourly_high = hourly_visits * ratio_high,
      visit_counts_hourly_low = hourly_visits * ratio_low
    )
  
  return(hour_final)
}