library(tidyverse)
library(brms)
library(tidybayes)

# read in data
dat90_20_nas<-read_csv("data/processed/ProducerDF_TreeCoverChangeCounty_NAsRemoved.csv")

# models
burn1 <- brm(b_burn ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 #I(trees1_9020^2) +
                 trees100_9020*trees1_9020 +
                 group_involve2, #+
                 #(1|nrd),
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123,
             family = bernoulli)

remo1 <- brm(b_remo ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 #I(trees1_9020^2) +
                 trees100_9020*trees1_9020 +
                 group_involve2, #+
                 #(1|nrd),
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123,
             family = bernoulli)

# generating data, only fixed effects variables
newdata <- expand_grid(trees100_9020 = c(0, 0.102725),
                        trees1_9020 = seq(min(dat90_20_nas$trees1_9020), max(dat90_20_nas$trees1_9020), len = 100),
                        group_involve2 = c(0,1))

#https://gist.github.com/mcgoodman/fb6fce24b4ff08f41d528574a5f84263
### Posterior mean --------------------------------------------------

## re_formula = NA allows us to average over observer-level effects
post_mean <- tidybayes::epred_draws(remo1, newdata, re_formula = NA)

## Summarize samples of posterior mean
## In this case, take mean, 95% credible interval (2.5% and 97.5% quantiles)
## and 80% credible interval (10% and 90% quantiles)
post_ci <- post_mean %>% 
    group_by(trees1_9020, group_involve2, trees100_9020) %>% 
    summarize(
        mean = mean(.epred), 
        `2.5%` = quantile(.epred, 0.025), 
        #`10%` = quantile(.epred, 0.1), 
        #`90%` = quantile(.epred, 0.9), 
        `97.5%` = quantile(.epred, 0.975), 
    ) %>%
    mutate(group = if_else(group_involve2 == 0, "Not involved in local groups", "Involved in local groups"), 
           trees_level = if_else(trees100_9020 == 0, "Low change in regional-level (100 km radius) mean percent tree cover", "High change in regional-level (100 km radius) mean percent tree cover"),
           trees100_group = paste0(trees_level, "-", group))

## Plot posterior mean and shaded 80% and 95% credible intervals
p1<-post_ci %>% 
    ggplot(aes(trees1_9020, mean,
               group = trees100_group,
               fill = trees_level,
               color = trees_level)) + 
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2, color = NA) +
    #geom_ribbon(aes(ymin = `10%`, ymax = `90%`), alpha = 0.2, color = NA) + 
    geom_line(aes(linetype = group),
              linewidth = 1) + 
    theme_classic(base_size = 18) +
    scale_color_manual(values = c("#6CD3ADFF","#0B0405FF")) + #scales::viridis_pal(option = "G")(12)
    scale_fill_manual(values = c("#6CD3ADFF","#0B0405FF")) +
    # scale_color_manual(values = c("#fde725","#3b528b")) + #https://waldyrious.net/viridis-palette-generator/
    # scale_fill_manual(values = c("#fde725","#3b528b")) +
    labs(color = "",
         fill = "",
         linetype="",
         x = "",
         y = "Probability of mechanical removal") +
    scale_x_continuous(expand = expansion(mult = c(0,0.02))) +
    scale_y_continuous(expand = expansion(mult = c(0,0.02)))+
    theme(legend.position = "bottom", axis.text=element_text(color="black"), legend.text=element_text(size=13))+
    guides(color=guide_legend(nrow=2, byrow=T),
           linetype=guide_legend(nrow=2, byrow=T))


## re_formula = NA allows us to average over observer-level effects
post_mean <- tidybayes::epred_draws(burn1, newdata, re_formula = NA)

## Summarize samples of posterior mean
## In this case, take mean, 95% credible interval (2.5% and 97.5% quantiles)
## and 80% credible interval (10% and 90% quantiles)
post_ci <- post_mean %>% 
    group_by(trees1_9020, group_involve2, trees100_9020) %>% 
    summarize(
        mean = mean(.epred), 
        `2.5%` = quantile(.epred, 0.025), 
        #`10%` = quantile(.epred, 0.1), 
        #`90%` = quantile(.epred, 0.9), 
        `97.5%` = quantile(.epred, 0.975), 
    ) %>%
    mutate(group = if_else(group_involve2 == 0, "Not involved in local groups", "Involved in local groups"), 
           trees_level = if_else(trees100_9020 == 0, "Low change in regional-level (100 km radius) mean percent tree cover", "High change in regional-level (100 km radius) mean percent tree cover"),
           trees100_group = paste0(trees_level, "-", group))

## Plot posterior mean and shaded 80% and 95% credible intervals
p2<-post_ci %>% 
    ggplot(aes(trees1_9020, mean,
               group = trees100_group,
               fill = trees_level,
               color = trees_level)) + 
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2, color = NA) +
    #geom_ribbon(aes(ymin = `10%`, ymax = `90%`), alpha = 0.2, color = NA) + 
    geom_line(aes(linetype = group),
              linewidth = 1) + 
    theme_classic(base_size = 18) +
    scale_color_manual(values = c("#6CD3ADFF","#0B0405FF")) + #scales::viridis_pal(option = "G")(12)
    scale_fill_manual(values = c("#6CD3ADFF","#0B0405FF")) +
    # scale_color_manual(values = c("#fde725","#3b528b")) + #https://waldyrious.net/viridis-palette-generator/
    # scale_fill_manual(values = c("#fde725","#3b528b")) +
    labs(color = "",
         fill = "",
         linetype="",
         x = "",
         y = "Probability of prescribed burning") +
    scale_x_continuous(expand = expansion(mult = c(0,0.02))) +
    scale_y_continuous(expand = expansion(mult = c(0,0.02)))+
    theme(legend.position = "bottom", axis.text=element_text(color="black"), legend.text=element_text(size=13))+
    guides(color=guide_legend(nrow=2, byrow=T),
           linetype=guide_legend(nrow=2, byrow=T))


### extract legend
p_legend<-post_ci %>% 
    ggplot(aes(trees1_9020, mean,
               group = trees100_group,
               fill = trees_level,
               color = trees_level)) + 
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2, color = NA) +
    #geom_ribbon(aes(ymin = `10%`, ymax = `90%`), alpha = 0.2, color = NA) + 
    geom_line(aes(linetype = group),
              linewidth = 1) + 
    theme_classic(base_size = 18) +
    scale_color_manual(values = c("#6CD3ADFF","#0B0405FF")) + #scales::viridis_pal(option = "G")(12)
    scale_fill_manual(values = c("#6CD3ADFF","#0B0405FF")) +
    # scale_color_manual(values = c("#fde725","#3b528b")) + #https://waldyrious.net/viridis-palette-generator/
    # scale_fill_manual(values = c("#fde725","#3b528b")) +
    labs(color = "",
         fill = "",
         linetype="",
         x = "Local-level change in mean percent tree cover",
         y = "Probability of prescribed burning") +
    scale_x_continuous(expand = expansion(mult = c(0,0.02))) +
    scale_y_continuous(expand = expansion(mult = c(0,0.02)))+
    theme(legend.position = "bottom", axis.text=element_text(color="black"), legend.text=element_text(size=13))+
    guides(color=guide_legend(nrow=2, byrow=T),
           linetype=guide_legend(nrow=2, byrow=T))

# legend<-cowplot::get_legend(p_legend +
#                                 theme(legend.box.margin = margin(100,100,100,100))
# ) #doesn't work with new update - try this instead below
legend<-cowplot::get_plot_component(p_legend, "guide-box-bottom", return_all = TRUE)

### create plot object without legend
prow<-cowplot::plot_grid(
    p1 + theme(legend.position = "none"),
    p2 + theme(legend.position = "none"), 
    align = 'vh',
    labels = c("A", "B"),
    nrow=1
)
prow

### create plot object with legend
prowlegend<-cowplot::plot_grid(
    prow,
    legend,
    ncol=1,
    rel_heights = c(1, 0.1)
)

### add x-axis label
cowplot::ggdraw(cowplot::add_sub(prowlegend, "Local-level (1 km radius) change in mean percent tree cover",
                                 size = 18,
                                 vpadding=grid::unit(0, "lines"),
                                 y=6, x=0.5))

#ggsave("figs/interaction_effect_bayesian.png", width = 14, height=7, units="in", dpi=300, bg="white")
