burn_df<-
    summary(burn)$fixed[,c(1,3:4)] %>%
    tibble::rownames_to_column("Parameter") %>%
    mutate(Model = "Standardized")

burn_raw_df<-
    summary(burn_raw)$fixed[,c(1,3:4)] %>%
    tibble::rownames_to_column("Parameter") %>%
    mutate(Model = "Unstandardized")

options(scipen = 999)

mixed_df <-
    bind_rows(
    burn_df, burn_raw_df) %>% 
    #filter(!term=="(Intercept)") %>%
    mutate(Parameter=recode_factor(Parameter,
                              "trees100_9020:trees1_9020" = "Interaction of local- and regional-level\nchange in mean % tree cover",
                              "trees100_9020" = "Regional-level (100 km radius) change in\nmean % tree cover from 1990 to 2020",
                              "Itrees1_9020E2" = "Local-level (1 km radius) change in\nmean % tree cover from 1990 to 2020\nsquared",
                              "trees1_9020" = "Local-level (1 km radius) change in\nmean % tree cover from 1990 to 2020",
                              "group_involve2"="Group involvement",
                              "(Intercept)" = "Intercept")) %>%
    mutate(Cred_Int = paste0("(", round(`l-95% CI`,2), ", ", round(`u-95% CI`,2), ")")#,
           #Estimate = round(Estimate, 3)
           ) %>%
    dplyr::select(Model, Parameter, Estimate, Cred_Int)

#write_csv(mixed_df, "figs/parameter_estimates_table.csv") #wrap text to see line breaks


