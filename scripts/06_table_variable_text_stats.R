# variable stats
dat90_20_nas %>% 
    dplyr::select(uniqueID:b_burn01, group_involve2) %>%
    pivot_longer(!uniqueID, names_to = "variable", values_to = "value") %>%
    group_by(variable) %>%
    summarize(mean = mean(value),
              sd = sd(value))

# percent tree cover stats 
dat1990_2020_wide %>% #from 02_process_data.R
    right_join(dat90_20_nas, by = "uniqueID") %>%
    dplyr::select(uniqueID, trees1km_1990, trees1km_2020, trees100km_1990, trees100km_2020, trees1_9020, trees100_9020) %>%
    summary()

# demographic stats
demo_dat <- 
    read_csv("data/original/NebraskaProducerSurvey_AllPrompts_General_clean.csv") %>%
    right_join(dat90_20_nas) %>%
    dplyr::select(uniqueID, q3a, q3b, q23, q24, q25, q26) %>%
    rename("acres_owned" = "q3a",
           "acres_rented" = "q3b",
           "age" = "q23",
           "gender" = "q24",
           "edu" = "q25",
           "income" = "q26")

summary(demo_dat)

demo_dat %>%
    dplyr::select(uniqueID,gender, edu, income) %>%
    pivot_longer(!uniqueID, names_to = "variable", values_to = "value") %>%
    group_by(variable, value) %>%
    summarize(n = n()) %>%
    mutate(percent = n/396*100)


