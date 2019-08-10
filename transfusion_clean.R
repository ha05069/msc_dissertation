# Load Libraries

library(tidyr)
library(dplyr)
library(ggplot)
library(readr)
library(reshape2)
library(stringr)
library(inspectEHR)
library(lme4)
library(lattice)
library(tableone)
library(data.table)

# create transfusion cohort

# read in ICIP extract for transfusions between 2014 and 2018

pc1 <- read_csv("packed_cells_1.csv") # 10,530 transfusion instances

# check for standard units

unique(pc1$"unit Of Measure")

# check for single site

unique(pc1$"display Label")

pc1 <-
    pc1 %>%
    filter("main State" == "Completed" | "main State" == "Chart") %>% # removes 6 observations
    select("life Time Number", "chart Time", "value Number") %>%
    rename(patient_id = "life Time Number",
           chart_time = "chart Time",
           prc_vol = "value Number")

# remove duplicate records which have the same chart datetime for the patient_id

pc1 <-
    pc1 %>%
    distinct() %>%
    group_by(patient_id, chart_time) %>%
    mutate(unique_tf = length(unique(chart_time))) %>%
    ungroup() %>%
    filter(unique_tf == 1) %>%
    select(-unique_tf)

# exclude patients meeting criteria for massive transfusion
# 10 units within 24 hours or > 4 units in 1 hour

pc1a <-
    pc1 %>%
    mutate(chart_datetime = dmy_hms(chart_time)) %>%
    select(-chart_time) %>%
    group_by(patient_id) %>%
    filter(n() > 1) %>%
    distinct(chart_datetime, .keep_all = T) %>%
    mutate(time_next = lead(chart_datetime, 1)) %>%
    mutate(time_next = if_else(is.na(time_next), lubridate::now(), time_next)) %>%
    mutate(delta_time = floor(difftime(time_next, chart_datetime, units = "hours"))) %>%
    filter(delta_time < 24) %>%
    filter(n() >= 10) %>%
    summarise(count = unique(n()))

# use the pc1a to remove all observations that meet bleeding criteria

pc1 <-
    pc1 %>%
    filter(!patient_id %in% pc1a$patient_id)

# same process to remove patients receiving more than 4 units in an hour

pc1b <-
    pc1 %>%
    mutate(chart_datetime = dmy_hms(chart_time)) %>%
    select(-chart_time) %>%
    group_by(patient_id) %>%
    filter(n() > 1) %>%
    distinct(chart_datetime, .keep_all = T) %>%
    mutate(time_next = lead(chart_datetime, 1)) %>%
    mutate(time_next = if_else(is.na(time_next), lubridate::now(), time_next)) %>%
    mutate(delta_time = floor(difftime(time_next, chart_datetime, units = "mins"))) %>%
    filter(delta_time < 60) %>%
    filter(n() >= 4) %>%
    summarise(count = unique(n()))

pc1 <-
    pc1 %>%
    filter(!patient_id %in% pc1b$patient_id)

# convert datetimes for easier joining

pc1 <-
    pc1 %>%
    mutate(chart_datetime = dmy_hms(chart_time))

# prep clean table for join with consultant data

pc1 <-
    pc1 %>%
    mutate(chart_day = as.Date(chart_datetime)) %>%
    select(patient_id, chart_datetime, chart_day, prc_vol)

