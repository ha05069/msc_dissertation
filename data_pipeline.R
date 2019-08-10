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

# create dataset for first analysis of all transfusions in dataset

# read in ICIP data extracts:

cons <- read_csv("consultant_on_ward_round.csv")

cons <-
    cons %>%
    select("encounter ID", "life Time Number", "chart Time", "terse Form", "display Label") %>%
    rename(encounter_id = "encounter ID",
    patient_id = "life Time Number",
    chart_time = "chart Time",
    consultant = "terse Form",
    site = "display Label")

# number of patients:

cons %>%
    group_by(patient_id) %>%
    summarise(count = n()) %>%
    View()

# select only relevant centre patients:

cons <-
    cons %>%
    filter(site == "UCH ICU")

# change chart_datetime from character to datetime object:

cons <-
    cons %>%
    mutate(chart_datetime = dmy_hms(chart_time))

cons <-
    cons %>%
    select(-chart_time)

# split datetime into date to allow easier joining

cons <-
    cons %>%
    mutate(chart_day = as.Date(chart_datetime))

# remove patients who saw more than one consultant per day

cons %>%
    group_by(patient_id, chart_day) %>%
    summarise(consultant_unique = length(unique(consultant))) %>%
    filter(consultant_unique > 1) %>%
    View()

cons <-
    cons %>%
    group_by(patient_id, chart_day) %>%
    mutate(consultant_unique = length(unique(consultant))) %>%
    filter(consultant_unique <= 1) %>%
    select(-consultant_unique)

# basic cleaning of consultant ID, more will be required later

cons <-
    cons %>%
    mutate(consultant = tolower(consultant)) %>%
    mutate(consultant = str_remove_all(string = consultant, pattern = "\\.")) %>%
    mutate(consultant = str_remove_all(consultant, "dr ")) %>%
    mutate(consultant = str_remove_all(consultant, "(prof|professor) "))



    
