#run 03_bayesian_burn_model.R first
burn <- readRDS("data/processed/brm_output_250905.rds") 

# generating data, only fixed effects variables
min(dat90_20_z$trees100_9020)
max(dat90_20_z$trees1_9020)

newdata <- expand_grid(trees100_9020 = c(-0.665, 3.289),
                       trees1_9020 = seq(min(dat90_20_z$trees1_9020), max(dat90_20_z$trees1_9020), len = 100),
                       group_involve2 = c(0,1))

# re_formula = NA allows us to average over observer-level effects
post_mean <- tidybayes::epred_draws(burn, newdata, re_formula = NA)

# Summarize samples of posterior mean
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
           trees_level = if_else(trees100_9020 == -0.665, "Low regional-level encroachment", "High regional-level encroachment"),
           trees100_group = paste0(trees_level, "-", group)) %>%
    mutate(trees1_unstand = trees1_9020*2*sd(dat90_20_nas$trees1_9020)*100)

# load svgs
library(magick)
library(rsvg)
svg_grass <- image_read_svg("figs/grass.svg")
svg_tree <- image_read_svg("figs/tree.svg")

# convert to rasters: https://docs.ropensci.org/magick/reference/image_ggplot.html#arguments
image1 <- image_fill(svg_grass, 'none')
raster1 <- as.raster(image1)

image2 <- image_fill(svg_tree, 'none')
raster2 <- as.raster(image2)

# Plot posterior mean and shaded 95% credible intervals
post_ci %>% 
    ggplot(aes(trees1_unstand, mean,
               group = trees100_group,
               fill = trees_level,
               color = trees_level)) + 
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.2, color = NA) +
    #geom_ribbon(aes(ymin = `10%`, ymax = `90%`), alpha = 0.2, color = NA) + 
    geom_line(aes(linetype = group),
              linewidth = 1) + 
    theme_classic(base_size = 18) +
    scale_color_manual(values = c("#6CD3ADFF","#0B0405FF")) + #scales::show_col(viridis::viridis(10))
    scale_fill_manual(values = c("#6CD3ADFF","#0B0405FF")) +
    labs(color = "",
         fill = "",
         linetype="",
         x = "Local-level encroachment",
         y = "Probability of prescribed burning") +
    scale_x_continuous(expand = expansion(mult = c(0,0.02))) +
    scale_y_continuous(expand = expansion(mult = c(0,0.02)))+
    theme(legend.position = "bottom", 
          axis.text=element_text(color="black"), 
          legend.text=element_text(size=10))+
    guides(color=guide_legend(nrow=2, byrow=T),
           linetype=guide_legend(nrow=2, byrow=T)) +
    annotation_raster(raster1, -11,-8,0.07,0.17) + #bottom-left grass
    annotation_raster(raster2, 21,23, 0.08, 0.15) + #bottom-right trees
    annotation_raster(raster2, 22,24, 0.08, 0.15) +
    annotation_raster(raster2, 23,25, 0.08, 0.15) +
    annotation_raster(raster1, -10,-7, 0.86, 0.96) + #top-left tree/grass/tree
    annotation_raster(raster2, -11,-9, 0.88, 0.95) +
    annotation_raster(raster2, -8,-6, 0.88, 0.95) +
    annotation_raster(raster1,  22,25, 0.86, 0.96) + #top-right grass/tree/grass
    annotation_raster(raster2, 22.5,24.5, 0.88, 0.95)
    # annotation_raster(raster1, -9,-6,0.07,0.17) + #bottom-left grass
    # annotation_raster(raster2, 25,27, 0.08, 0.15) + #bottom-right trees
    #     annotation_raster(raster2, 26,28, 0.08, 0.15) +
    #     annotation_raster(raster2, 27,29, 0.08, 0.15) +
    # annotation_raster(raster1, -7,-4, 0.86, 0.96) + #top-left tree/grass/tree
    #     annotation_raster(raster2, -8,-6, 0.88, 0.95) +
    #     annotation_raster(raster2, -5,-3, 0.88, 0.95) +
    # annotation_raster(raster1,  24,27, 0.86, 0.96) + #top-right grass/tree/grass
    # annotation_raster(raster2, 24.5,26.5, 0.88, 0.95)

#ggsave("figs/interaction_effect_bayesian_burn_ES2.png", width = 7, height=7, units="in", dpi=300, bg="white")    
    



