library(tidyverse)

# read in data and prep for model
dat90_20_nas<-read_csv("data/processed/ProducerDF_TreeCoverChangeCounty_NAsRemoved.csv")

# models
burn1 <- brm(b_burn ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 I(trees1_9020^2) +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|county2),
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123,
             family = bernoulli)

remo1 <- brm(b_remo ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 I(trees1_9020^2) +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|county2),
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123,
             family = bernoulli)

# parameter estimate dataframe
burn_df<-
    summary(burn1)$fixed[,c(1,3:4)] %>%
    tibble::rownames_to_column("Parameter") %>%
    mutate(Model = "Prescribed burning")
colnames(burn_df) <- c("term", "estimate", "conf.low", "conf.high", "Model")

remo_df<-
    summary(remo1)$fixed[,c(1,3:4)] %>%
    tibble::rownames_to_column("Parameter") %>%
    mutate(Model = "Mechanical removal")
colnames(remo_df) <- c("term", "estimate", "conf.low", "conf.high", "Model")


mixed_df<- bind_rows(
    burn_df, remo_df) %>% 
    #filter(!term=="(Intercept)") %>%
    mutate(term=recode_factor(term,
                              "trees100_9020:trees1_9020" = "Interaction of local- and regional-level\nchange in mean % tree cover",
                              "trees100_9020" = "Regional-level (100 km radius) change in\nmean % tree cover from 1990 to 2020",
                              "trees1_9020" = "Local-level (1 km radius) change in\nmean % tree cover from 1990 to 2020",
                              "group_involve2"="Group involvement",
                              "(Intercept)" = "Intercept",
                              "Itrees1_9020E2" = "Local-level (1 km radius) squared"))

# plot
ggplot(mixed_df, aes(y=term, x=estimate, group=Model))+
    geom_pointrange(aes(xmin=conf.low,
                        xmax=conf.high,
                        shape=Model,
                        color=Model), 
                    linewidth=2,
                    position=position_dodge(width = 0.5),
                    size=1.1,
                    fill="white")+
    #coord_fixed(ratio =0.45)+ #reduces space between y-axis ticks but relative to the vertical plot window so leaves huge margins, play with saving different sizes instead
    geom_vline(xintercept=0, linetype="dashed", linewidth=1)+
    scale_shape_manual(values=c(22,23),
                       breaks=c("Mechanical removal", "Prescribed burning"))+
    scale_color_manual(values = c("#fc9f07", "#d44842"), #https://waldyrious.net/viridis-palette-generator/
                       breaks=c("Mechanical removal", "Prescribed burning"))+
    xlab("Log odds estimate") +
    ylab("")+
    theme_classic(base_size = 15) +
    theme(axis.text=element_text(size=15, color="black"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linetype="dotted", color="darkgray", linewidth=.5),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text=element_text(size=15, hjust=0.5, margin = margin(r = 1, unit = "cm")),
          legend.spacing.x = unit(0.2, 'cm'),
          legend.key.size = unit(1, "cm"))

#ggsave("figs/parameter_estimates.png", width = 9, height=5, units="in", dpi=300, bg="white")
