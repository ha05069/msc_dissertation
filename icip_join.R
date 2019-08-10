# join consultant identity to transfusion instances

# rename for convenience

transfusion <- pc1
consultants <- cons

icip_join <- left_join(transfusion, consultants, by = c("patient_id", "chart_day"))

icip_join <-
    icip_join %>%
    group_by(patient_id) %>%
    distinct(chart_datetime.x, .keep_all = T)

# number of transfusion instances missing consultant identifiers

icip_join %>%
    filter(is.na(consultant)) %>%
    View() # 1133

# remove these transfusion instances

icip_join <-
    icip_join %>%
    filter(!is.na(consultant))

# rename for convenience

transfused <- icip_join

# check number of patients within transfused cohort

transfused %>%
    group_by(patient_id) %>%
    distinct(patient_id)



