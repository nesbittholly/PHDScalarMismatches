#run 03_bayesian_burn_model.R first

tidybayes::get_variables(burn)

burn %>%
    tidybayes::spread_draws(b_Intercept, r_nrd[nrd,]) %>%
    mutate(nrd_mean = b_Intercept + r_nrd, 
           nrd = recode_factor(nrd,
                               "Central.Platte" = "Central Platte",
                               "Lower.Big.Blue" = "Lower Big Blue",
                               "Lower.Loup" = "Lower Loup",
                               "Lower.Platte.South" = "Lower Platte South",
                               "Middle.Niobrara" = "Middle Niobrara",
                               "Nemaha" = "Nemaha",
                               "North.Platte" = "North Platte",
                               "Twin.Platte" = "Twin Platte",
                               "Upper.Big.Blue" = "Upper Big Blue",
                               "Upper.Loup" = "Upper Loup",
                               "Upper.Niobrara-White" = "Upper Niobrara-White"),
           cost_share = case_when(nrd == "Twin Platte" ~ "No",
                                  nrd == "Upper Loup" ~ "No",
                                  TRUE ~ "Yes"),
           order = case_when(nrd == "Upper Niobrara-White" ~ 1,
                             nrd == "North Platte" ~ 2,
                             nrd == "Middle Niobrara" ~ 3,
                             nrd == "Upper Loup" ~ 4,
                             nrd == "Twin Platte" ~ 5,
                             nrd == "Lower Loup" ~ 6,
                             nrd == "Central Platte" ~ 7,
                             nrd == "Upper Big Blue" ~ 8,
                             nrd == "Lower Platte South" ~ 9,
                             nrd == "Lower Big Blue" ~ 10,
                             nrd == "Nemaha" ~ 11)) %>%
    ggplot(aes(y = reorder(nrd, -order), x = nrd_mean, fill = cost_share)) +
    ggdist::stat_halfeye() + 
    lims(x = c(-3, 2.5)) + # cuts off some values in the tails if you include this
    geom_vline(xintercept = 0, col = "black", linewidth = 1, linetype = "dotted") +
    xlab("Posterior probability") +
    ylab("Natural Resources District") +
    scale_fill_manual(values = c("darkgrey", "darkolivegreen4")) +
    guides(fill = guide_legend(title = "Juniper\ncost share")) +
    theme_classic(base_size = 15) +
    theme(axis.text=element_text(size=15, color="black"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          # panel.grid.major.y = element_line(linetype="dotted", color="darkgray", linewidth=.5)
          legend.position = c(0.9,.75)
          # legend.title = element_blank(),
          # legend.text=element_text(size=15, hjust=0.5, margin = margin(r = 1, unit = "cm")),
          # legend.spacing.x = unit(0.2, 'cm'),
          # legend.key.size = unit(1, "cm")
    )

#Upper Niobrara is in the NW corner
#Lower Loup is pretty central, lower big blue is SE, and Central Platte is just south of Lower Loup
#Twin Platte and Upper Loup have eliminated juniper cost share programs

#ggsave("figs/nrd_effect.png", width = 8, height=5, units="in", dpi=300, bg="white")