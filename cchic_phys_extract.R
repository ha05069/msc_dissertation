library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(reshape2)
library(lubridate)
library(stringr)
library(inspectEHR)
library(lme4)
library(lattice)
library(tableone)
library(data.table)

# cchic physiology variable extract and join:

# extract, clean and join physiological variables from CCHIC

phys_vars <- c(
               "NIHR_HIC_ICU_0108", # heart rate
               "NIHR_HIC_ICU_0110", # invasive mean arterial pressure
               "NIHR_HIC_ICU_0111", # non-invasive mean arterial pressure
               "NIHR_HIC_ICU_0122", # lactate (abg)
               "NIHR_HIC_ICU_0123", # lactate (lab)
               "NIHR_HIC_ICU_0162", # urine output
               "NIHR_HIC_ICU_0150") # fiO2

epi_id_list <- unique(transfusion$episode_id)

phys_full <-
    tbls$events %>%
    filter(code_name %in% phys_vars) %>%
    filter(episode_id %in% epi_id_list) %>%
    collect()

save()

# extract individual variables and clean and filter e.g.

# heart rate:

hr <- # extract relevant columns and filter for unique episode_ids and physiologically possible values
    phys_full %>%
    filter(code_name == "NIHR_HIC_ICU_0108") %>%
    select(datetime, integer, episode_id) %>%
    rename(hr == integer) %>%
    distinct() %>%
    arrange(episode_id, datetime) %>%
    filter(hr > 0 & hr < 300) %>%
    group_by(episode_id)

hr2 <- # join to transfusion table by episode id
    hr %>%
    left_join(transfusion, by = "episode_id")

hr2 <- # filter for valid heart rates by identifying those recorded during the episode window
    hr2 %>%
    mutate(valid_hr = datetime >= epi_start_dttm & datetime <= epi_end_dttm) %>%
    filter(valid_hr == TRUE) %>%
    select(-valid_hr)

# window for readings in the 24 hours prior to transfusion

hr3 <-
    hr2 %>%
    rename(hr_datetime = "datetime") %>%
    mutate(hr_datetime = as_datetime(hr_datetime)) %>%
    mutate(transfusion_minus_24 = transfusion_dttm - hours(24)) %>%
    mutate(valid_hr = hr_datetime < transfusion_dttm & hr_datetime >= transfusion_minus_24) %>%
    filter(valid_hr) %>%
    select(-valid_hr, - transfusion_minus_24)

# calculate mean heart rate pre transfusion

hr3a <-
    hr3 %>%
    arrange(patient_id, transfusion_dttm, hr_datetime) %>%
    group_by(patient_id, transfusion_dttm) %>%
    summarise(mean_hr = round(mean(hr)))

# do the same for the 24 hours following transfusion

hr4 <-
    hr2 %>%
    rename(hr_datetime = "datetime") %>%
    mutate(hr_datetime = as_datetime(hr_datetime)) %>%
    mutate(transfusion_plus_24 = transfusion_dttm + hours(24)) %>%
    mutate(valid_hr = hr_datetime > transfusion_dttm & hr_datetime <= transfusion_plus_24) %>%
    filter(valid_hr) %>%
    select(-valid_hr, - transfusion_plus_24) %>%
    arrange(patient_id, transfusion_dttm, hr_datetime) %>%
    group_by(patient_id, transfusion_dttm) %>%
    summarise(mean_postxm_24hr = round(mean(hr)))

# join pre and post average heart rates together

hr_join <-
    left_join(hr3a, hr4, by = c("patient_id", "transfusion_dttm")) %>%
    rename(pre_xm_avhr = "mean_hr", post_xm_avhr = "mean_postxm_24hr") %>%
    filter(!is.na(post_xm_avhr)) %>%
    group_by(patient_id) %>%
    mutate(delta_hr = post_xm_avhr - pre_xm_avhr)

# join to demographic data

transfusion <-
    left_join(transfusion, hr_join, by = c("patient_id", "transfusion_dttm"))

save()

# repeat the same process for each physiology variable listed above and create main datatable for
# analysis of all transfusion instances


