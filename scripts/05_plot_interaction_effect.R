library(tidyr)

# read in data
dat90_20_nas<-read_csv("data/processed/ProducerDF_TreeCoverChangeCounty_NAsRemoved.csv")

# models, not standardized for plots
burn_glm1 <- glm(b_burn ~ trees100_9020*trees1_9020 + group_involve2,
                 data = dat90_20_nas, 
                 family = "binomial")

remo_glm1 <- glm(b_remo ~ trees100_9020*trees1_9020 + group_involve2,
                   data = dat90_20_nas, 
                   family = "binomial")

# logistic regression function
inv_logit <- function (x) exp(x) / (1 + exp(x))

# prediction function
pred_CI <- function(model, newdata=NULL, alpha=0.05) {
    X <- model.matrix(formula(model, fixed.only=TRUE)[-2], newdata)
    pred0 <- as.vector(X %*% fixef(model))
    V <- vcov(model)    
    pred_se <- sqrt(rowSums((X %*% V) * X))
    crit <- -qnorm(alpha/2)
    as_tibble(cbind(conf_low=inv_logit(pred0-crit*pred_se),
                    conf_high=inv_logit(pred0+crit*pred_se),
                    predict=inv_logit(pred0)))
}

# generating data
new_data <- expand_grid(trees100_9020 = c(0, 0.102725),
                        trees1_9020 = seq(min(dat90_20_nas$trees1_9020), max(dat90_20_nas$trees1_9020), len = 100),
                        group_involve2 = c(0,1))

# plots with local-scale on x-axis
p_b3<-new_data %>% 
    mutate(predict = predict(burn_glm1, new_data, type = "response"),
           conf_low = inv_logit(predict(burn_glm1, new_data) - (predict(burn_glm1, new_data, se.fit = T)$se.fit)*1.96),
           conf_high = inv_logit(predict(burn_glm1, new_data) + (predict(burn_glm1, new_data, se.fit = T)$se.fit)*1.96)) %>% 
    mutate(group = if_else(group_involve2 == 0, "Not involved in local groups", "Involved in local groups"), 
           trees_level = if_else(trees100_9020 == 0, "Low change in regional-level mean percent tree cover", "High change in regional-level mean percent tree cover"),
           trees100_group = paste0(trees_level, "-", group)) %>% 
    ggplot(., aes(x = trees1_9020*100, y = predict,
                  group = trees100_group,
                  fill = trees_level, 
                  color = trees_level)) +
    geom_ribbon(aes(ymin=conf_low, ymax=conf_high), color=NA, alpha=0.2) +
    geom_line(aes(linetype=group), linewidth=1)+
    theme_classic(base_size = 18) +
    scale_color_manual(values = c("#40bd72","#3d4d8a")) + #https://waldyrious.net/viridis-palette-generator/
    scale_fill_manual(values = c("#40bd72","#3d4d8a")) +
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
p_b3

p_m3<-new_data %>% 
    mutate(predict = predict(remo_glm1, new_data, type = "response"),
           conf_low = inv_logit(predict(remo_glm1, new_data) - (predict(remo_glm1, new_data, se.fit = T)$se.fit)*1.96),
           conf_high = inv_logit(predict(remo_glm1, new_data) + (predict(remo_glm1, new_data, se.fit = T)$se.fit)*1.96)) %>% 
    mutate(group = if_else(group_involve2 == 0, "Not involved in local groups", "Involved in local groups"), 
           trees_level = if_else(trees100_9020 == 0, "Low change in regional-level mean percent tree cover", "High change in regional-level mean percent tree cover"),
           trees100_group = paste0(trees_level, "-", group)) %>% 
    ggplot(., aes(x = trees1_9020*100, y = predict,
                  group = trees100_group,
                  fill = trees_level, 
                  color = trees_level)) +
    geom_ribbon(aes(ymin=conf_low, ymax=conf_high), color=NA, alpha=0.2) +
    geom_line(aes(linetype=group), linewidth=1)+
    theme_classic(base_size = 18) +
    scale_color_manual(values = c("#40bd72","#3d4d8a")) +
    scale_fill_manual(values = c("#40bd72","#3d4d8a")) +
    labs(color = "",
         fill = "",
         linetype="",
         x = "",
         y = "Probability of mechanical removal") +
    scale_x_continuous(expand = expansion(mult = c(0,0.02))) +
    scale_y_continuous(expand = expansion(mult = c(0,0.02)))+
    theme(legend.position="none",  axis.text=element_text(color="black"))
#theme(legend.position = "bottom", axis.text=element_text(color="black"))+
#guides(color=guide_legend(nrow=2, byrow=T),
#       linetype=guide_legend(nrow=2, byrow=T))
p_m3

cowplot::ggdraw()+
    cowplot::draw_plot(p_m3, x=0, y=0.54, width=1, height=.46)+
    cowplot::draw_plot(p_b3, x=0, y=0, width=1, height=0.54)+
    cowplot::draw_plot_label(
        c("A", "B"),
        c(0, 0),
        c(1, 0.56),
        size = 20)
#ggsave("figs/interaction_effect.png", width = 9, height=16, units="in", dpi=300, bg="white")
