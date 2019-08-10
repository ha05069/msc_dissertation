# Extract and join CCHIC demographic data

# establish connection to HIC database and retrieve tables

ctn <-

tbls <- 

# extract demographic codes:

demographic_codes <-
    tribble(
            ~hic_codes, ~ short_name,
            "NIHR_HIC_ICU_0001", "pas_number",
            "NIHR_HIC_ICU_0411", "admission_dt",
            "NIHR_HIC_ICU_0412", "discharge_dt",
            "NIHR_HIC_ICU_0033", "dob",
            "NIHR_HIC_ICU_0093", "sex",
            "NIHR_HIC_ICU_0398", "admission_type",
            "NIHR_HIC_ICU_0399", "primary_admission_reason",
            "NIHR_HIC_ICU_0409", "apache_score",
            "NIHR_HIC_ICU_0410", "apache_prob")

# extract valid episodes

cor_tbl <- make_core(ctn)
ref_tbl <- make_reference(ctn)

# extract all HIC episodes

episodes <- epi_length(cor_tbl, ref_tbl, tbls[["events"]])

# extract valid episodes for site only

episodes_site<-
    episodes %>%
    filter(site == "xxx") %>%
    filter(validity == 0)

# extract the required demographic variable data

demographic_data <-
    tbls$provenance %>%
    filter(site == "xxx") %>%
    select(file_id) %>%
    inner_join(tbls$episodes, by = c("file_id" = "provenance")) %>%
    inner_join(tbls$events, by = "episode_id") %>%
    extract_demographics(events = .,
                         metadata = tbls$variables,
                         code_names = demographic_codes$hic_codes,
                         rename = demographic_codes$short_name)

# select only the demographic data for the valid episodes

valid_demographics <-
    left_join(episodes_site, demographic_data, by = "episode_id")

# join to transfusion data

transfusion <-
    left_join(transfused, valid_demographics, by = c("patient_id" = "pas_number"))

transfusion <-
    transfusion %>%
    filter(!is.na(episode_id)) %>%
    rename(transfusion_dttm = "chart_datetime.x",
           transfusion_chartday = "chart_day",
           cons_dttm = "chart_datetime.y") %>%
           select(-encounter_id, - site.y, - validity, - discharge_dt, - admission_dt)

# filter for valid transfusion episodes
# (ensure transfusion occurs during admission window)

transfusion <-
    transfusion %>%
    mutate(valid_transfusion =
           transfusion_dttm <= epi_end_dttm &
           transfusion_dttm >= epi_start_dttm) %>%
           filter(valid_transfusion) # 4175 observations

transfusion <-
    transfusion %>%
    select(-site.x)

# tidy demographic variables

transfusion <- # create age at admission from dob variable
    transfusion %>%
    mutate(age = floor(difftime(transfusion_dttm, dob, units = "weeks"))) %>%
    mutate(age = floor(age / 52)) %>%
    select(-dob)

transfusion <- # round length of stay to nearest day
    transfusion %>%
    mutate(los = floor(los))

# join to previously hand cleaned consultant IDs:

cons_clean <-
    read.csv("cons_clean.csv")

transfusion <-
    transfusion %>%
    left_join(cons_clean, by = "consultant") %>%
    select(-consultant) %>%
    rename(consultant = "clean_consultant")

