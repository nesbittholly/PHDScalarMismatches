#run 03_bayesian_burn_model.R first

tidybayes::get_variables(burn)

burn %>%
    tidybayes::gather_draws(b_Intercept,
                            b_trees100_9020,
                            b_trees1_9020,
                            b_group_involve2,
                            `b_trees100_9020:trees1_9020`
                            ) %>%
    mutate(.variable=recode_factor(.variable,
                              "b_trees100_9020:trees1_9020" = "Local- and regional-level\nencroachment interaction",
                              "b_trees100_9020" = "Regional-level encroachment",
                              "b_trees1_9020" = "Local-level encroachment",
                              "b_group_involve2"="Group involvement",
                              "b_Intercept" = "Intercept")) %>%
    ggplot(aes(y = reorder(.variable, -.value), x = .value)) +
    ggdist::stat_halfeye() +
    geom_vline(xintercept = 0, col = "black", linewidth = 1, linetype = "dotted") +
    xlab("Posterior probability") +
    ylab("") +
    theme_classic(base_size = 15) +
    theme(axis.text=element_text(size=15, color="black"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()
          # panel.grid.major.y = element_line(linetype="dotted", color="darkgray", linewidth=.5),
          # legend.position = "bottom",
          # legend.title = element_blank(),
          # legend.text=element_text(size=15, hjust=0.5, margin = margin(r = 1, unit = "cm")),
          # legend.spacing.x = unit(0.2, 'cm'),
          # legend.key.size = unit(1, "cm")
          )
    
#ggsave("figs/posterior_prob_burn_ES2.png", width = 9, height=5, units="in", dpi=300, bg="white")

knitr::kable(fixef(burn))

table_MS <-
    fixef(burn) %>%
    as_tibble() %>%
    mutate(Variable = c("Intercept",
                        "Regional-level encroachment",
                        "Local-level encroachment",
                        "Group involvement",
                        "Interaction of local- and regional-level encroachment"),
           Estimate = round(Estimate, digits = 2),
           Est.Error = round(Est.Error, digits = 2),
           Q2.5 = round(Q2.5, digits = 2),
           Q97.5 = round(Q97.5, digits = 2),
           Estimate.CI = paste0(Estimate, " (", Q2.5, ", ", Q97.5, ")"),
           order = c(1,4,3,2,5)) %>%
    arrange(order)%>%
    dplyr::select(Variable, Estimate.CI, Est.Error)

write_csv(table_MS, "figs/parameter_estimates_table_ES2.csv")    

table_MS
