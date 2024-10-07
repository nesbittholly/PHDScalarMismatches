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
                              "b_trees100_9020:trees1_9020" = "Interaction of local- and regional-level\nchange in mean % tree cover",
                              "b_trees100_9020" = "Regional-level (100 km radius) change in\nmean % tree cover from 1990 to 2020",
                              "b_trees1_9020" = "Local-level (1 km radius) change in\nmean % tree cover from 1990 to 2020",
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
    
#ggsave("figs/posterior_prob_burn.png", width = 9, height=5, units="in", dpi=300, bg="white")
