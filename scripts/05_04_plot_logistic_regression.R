#run 03_bayesian_burn_model.R first
library(ggnewscale)

# hold trees1_9020 constant
## generating data, only fixed effects variables
newdata3 <- expand_grid(trees100_9020 = seq(min(dat90_20_z$trees100_9020), max(dat90_20_z$trees100_9020), len = 198),
                        trees1_9020 = mean(dat90_20_z$trees1_9020),
                        group_involve2 = c(0,1)) 

## re_formula = NA allows us to average over observer-level effects
post_mean3 <- tidybayes::epred_draws(burn, newdata3, re_formula = NA)

## Summarize samples of posterior mean
post_ci_all3 <- post_mean3 %>% 
    group_by(group_involve2, trees100_9020) %>% 
    summarize(
        mean = mean(.epred), 
        `2.5%` = quantile(.epred, 0.025), 
        `97.5%` = quantile(.epred, 0.975), 
    ) %>%
    mutate(group = if_else(group_involve2 == 0, "Not involved in local groups", "Involved in local groups"))

## plot
ggplot() +
    geom_line(aes(x = trees100_9020, y = mean, color = group), #linetype = group,
              data = post_ci_all3, 
              linewidth = 1) +
    scale_color_manual(name = "", values = c("red", "black")) +
    geom_ribbon(aes(x = trees100_9020, ymin = `2.5%`, ymax = `97.5%`, fill = group), #fill = "95% CI"),
                data = post_ci_all3, 
                alpha = 0.2) +
    scale_fill_manual(name = "", values = c("red", "black")) +
    ggnewscale::new_scale_color() +
    # geom_jitter(aes(x = trees100_9020, y = b_burn01, color = as_factor(group_involve2)),
    #             data = dat90_20_z,
    #             height = 0.04,
    #             width = 0.1,
    #             alpha = 0.5) +
    ggdist::geom_dots(aes(side = ifelse(b_burn01 == 1, "top", "bottom"),
                          x = trees100_9020, y = b_burn01,
                          color = as_factor(group_involve2)),
                      data = dat90_20_z,
                      scale = 0.3,
                      layout = "hex",
                      pch = 19) +
    ggdist::scale_side_mirrored(guide = "none") +
    scale_color_manual(name = "", values = c("black", "red"), guide = "none") +
    coord_cartesian(ylim = c(0, 1)) +
    theme_classic(base_size = 18) +
    labs(linetype="",
         fill = "",
         color = "",
         x = "Regional-level change in mean % tree cover\n(standardized)",
         y = "Probability of prescribed burning") +
    theme(legend.position = "bottom",
        axis.text=element_text(color="black"),
        legend.text=element_text(size=12))

#ggsave("figs/logistic100_bayesian_burn.png", width = 7, height=7, units="in", dpi=300, bg="white")  

# hold trees100_9020 constant
## generating data, only fixed effects variables
newdata4 <- expand_grid(trees100_9020 = mean(dat90_20_z$trees100_9020),
                        trees1_9020 = seq(min(dat90_20_z$trees1_9020), max(dat90_20_z$trees1_9020), len = 198),
                        group_involve2 = c(0,1)) 

## re_formula = NA allows us to average over observer-level effects
post_mean4 <- tidybayes::epred_draws(burn, newdata4, re_formula = NA)

## Summarize samples of posterior mean
post_ci_all4 <- post_mean4 %>% 
    group_by(group_involve2, trees1_9020) %>% 
    summarize(
        mean = mean(.epred), 
        `2.5%` = quantile(.epred, 0.025), 
        `97.5%` = quantile(.epred, 0.975), 
    ) %>%
    mutate(group = if_else(group_involve2 == 0, "Not involved in local groups", "Involved in local groups"))

## plot
ggplot() +
    geom_line(aes(x = trees1_9020, y = mean, color = group), #linetype = group,
              data = post_ci_all4, 
              linewidth = 1) +
    scale_color_manual(name = "", values = c("red", "black")) +
    geom_ribbon(aes(x = trees1_9020, ymin = `2.5%`, ymax = `97.5%`, fill = group), #fill = "95% CI"),
                data = post_ci_all4, 
                alpha = 0.2) +
    scale_fill_manual(name = "", values = c("red", "black")) +
    ggnewscale::new_scale_color() +
    # geom_jitter(aes(x = trees1_9020, y = b_burn01, color = as_factor(group_involve2)),
    #             data = dat90_20_z,
    #             height = 0.04,
    #             width = 0.1,
    #             alpha = 0.5) +
    ggdist::geom_dots(aes(side = ifelse(b_burn01 == 1, "top", "bottom"),
                          x = trees1_9020, y = b_burn01,
                          color = as_factor(group_involve2)),
                      data = dat90_20_z,
                      scale = 0.4,
                      layout = "hex",
                      pch = 19) +
    ggdist::scale_side_mirrored(guide = "none") +
    scale_color_manual(name = "", values = c("black", "red"), guide = "none") +
    coord_cartesian(ylim = c(0, 1)) +
    theme_classic(base_size = 18) +
    labs(linetype="",
         fill = "",
         color = "",
         x = "Local-level change in mean % tree cover\n(standardized)",
         y = "Probability of prescribed burning") +
    theme(legend.position = "bottom",
          axis.text=element_text(color="black"),
          legend.text=element_text(size=12))

#ggsave("figs/logistic1_bayesian_burn.png", width = 7, height=7, units="in", dpi=300, bg="white")  


# dead code, legend battle not worth it
# ggplot() +
#     geom_line(aes(x = trees100_9020, y = mean, color = group), #linetype = group,
#               data = post_ci_all3,
#               linewidth = 1) +
#     scale_color_manual(name = "", values = c("black", "red"), labels = c("Observed", "Predicted")) +
#     geom_ribbon(aes(x = trees100_9020, ymin = `2.5%`, ymax = `97.5%`, fill = group), #fill = "95% CI"),
#                 data = post_ci_all3,
#                 alpha = 0.2) +
#     scale_fill_manual(name = "", values = c("black", "red"), labels = c("Observed", "Predicted")) +
#     new_scale_color() +
#     geom_point(aes(x = trees100_9020, y = b_burn01, color = as_factor(group_involve2)),
#                data = dat90_20_z,
#                position = position_jitter(height = 0.05, width = 0, seed = 1),
#                alpha = 0.2) +
#     scale_color_manual(name = "", values = c("red", "black"), labels = c("Not involved in local groups", "Involved in local groups")) +
#     theme_classic(base_size = 18) +
#     labs(linetype="",
#          fill = "",
#          color = "",
#          x = "Regional change",
#          y = "Probability of prescribed burning") +
#     theme(#legend.position = "bottom",
#         axis.text=element_text(color="black"),
#         legend.text=element_text(size=9)) +
#     guides(color_new = guide_legend(override.aes = list(fill = c("black", "black"), #this should be changing the points, which  the new layer
#                                                         shape = c(21,0), # can't get shape to work
#                                                         linetype = c(0,1),
#                                                         color_new = c("black", "black"))))

# with no group effect
# generating data, only fixed effects variables
# newdata2 <- expand_grid(trees100_9020 = seq(min(dat90_20_z$trees100_9020), max(dat90_20_z$trees100_9020), len = 100),
#                         trees1_9020 = mean(dat90_20_z$trees1_9020),
#                         group_involve2 = 0)#hold group constant c(0,1)) 
# 
# # re_formula = NA allows us to average over observer-level effects
# post_mean <- tidybayes::epred_draws(burn, newdata2, re_formula = NA)
# 
# # Summarize samples of posterior mean
# post_ci_all <- post_mean %>% 
#     #group_by(group_involve2, trees100_9020) %>% 
#     summarize(
#         mean = mean(.epred), 
#         `2.5%` = quantile(.epred, 0.025), 
#         `97.5%` = quantile(.epred, 0.975), 
#     ) %>%
#     mutate(group = if_else(group_involve2 == 0, "Not involved in local groups", "Involved in local groups"))
# 
# ggplot() +
#     geom_line(aes(x = trees100_9020, y = mean, color = "Predicted"),
#               data = post_ci_all, linewidth = 1) +
#     geom_ribbon(aes(x = trees100_9020, ymin = `2.5%`, ymax = `97.5%`, color = "Predicted"),
#                 data = post_ci_all, alpha = 0.2) +
#     geom_jitter(aes(x = trees100_9020, y = b_burn01, color = "Observed"),
#                 data = dat90_20_z, height = 0.05, width = 0, alpha = 0.5) +
#     scale_color_manual(name = NULL,
#                        values = c("black", "black"),
#                        breaks = c("Observed", "Predicted"),
#                        guide = guide_legend(override.aes = list(linetype = c(0, 1),
#                                                                 shape = c(16, NA),
#                                                                 color = "black") ) ) +
#     theme_classic(base_size = 18) +
#     labs(x = "Regional change",
#          y = "Probability of prescribed burning") +
#     theme(legend.position = "bottom", 
#           axis.text=element_text(color="black"), 
#           legend.text=element_text(size=9))