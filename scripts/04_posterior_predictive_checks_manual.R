# manually generating posterior predictive checking so I can color the plots
pp = pp_check(burn, "scatter_avg")  # Calls bayesplot::ppc_scatter_avg to generate the plot

## Get average of predicted draws and join to original data. 
# predicted_draws uses posterior_predict under the hood and returns a tidy data frame
#https://discourse.mc-stan.org/t/using-average-of-the-posterior-draws-for-posterior-predictive-checking/29610/3
pred = tidybayes::predicted_draws(burn, newdata=select(dat90_20_z, trees1_9020, trees100_9020, group_involve2, nrd, b_burn01))
pred = pred %>% 
    group_by(trees1_9020, trees100_9020, group_involve2, nrd) %>% 
    summarise(.prediction = mean(.prediction)) %>% 
    left_join(dat90_20_z)

## Superimpose manual predictions on ppc_scatter_avg to show they are the same
pp + 
    geom_point(data=pred, aes(.prediction, b_burn01), 
               colour="red", size=0.5, alpha=0.5)

# ## get yrep
# #https://cran.r-project.org/web/packages/bayesplot/vignettes/graphical-ppcs.html#defining-y-and-yrep
# yrep <- posterior_predict(burn)
# dim(yrep) #a matrix yrep of draws from the posterior predictive distribution

# trees1_9020
## predictive check
pp_check(burn, type = "error_scatter_avg_vs_x", size = 1.1, 
         x = "trees1_9020")

## is it the same as the manual option?
pred %>%
    ggplot() +
    geom_point(aes(x = trees1_9020, y = b_burn01 - .prediction, color = as_factor(b_burn01)), alpha = 0.5) +
    labs(y = "Yobs - Predicted Yavg") +
    guides(color=guide_legend(title="Obs presc burning"))

# trees100_9020
## predictive check
pp_check(burn, type = "error_scatter_avg_vs_x", size = 1.1, 
         x = "trees100_9020")

## is it the same as the manual option?
pred %>%
    ggplot() +
    geom_point(aes(x = trees100_9020, y = b_burn01 - .prediction, color = as_factor(b_burn01)), alpha = 0.5) +
    labs(y = "Yobs - Predicted Yavg")+
    guides(color=guide_legend(title="Obs presc burning"))


# group_involve2
## predictive check
pp_check(burn, type = "error_scatter_avg_vs_x", size = 1.1, 
         x = "group_involve2")

## is it the same as the manual option?
pred %>%
    ggplot() +
    geom_point(aes(x = group_involve2, y = b_burn01 - .prediction, color = as_factor(b_burn01)), alpha = 0.5) +
    labs(y = "Yobs - Predicted Yavg")+
    guides(color=guide_legend(title="Obs presc burning"))

# interaction
dat90_20_z$interaction <- dat90_20_z$trees100_9020*dat90_20_z$trees1_9020

prior_int_col <- c(set_prior("normal(0,1)", class = "b", coef = "group_involve2"), # expect that larger numbers would be less likely
                   set_prior("normal(0,1)", class = "b", coef = "trees1_9020"),
                   set_prior("normal(0,1)", class = "b", coef = "trees100_9020"),
                   set_prior("normal(0,1)", class = "b", coef = "interaction"),
                   set_prior("student_t(7, 0, 1.5)", class = "b", coef = "Intercept"), # expect a narrow distribution
                   set_prior("exponential(1)", class = "sd"), #truncates above 0
                   set_prior("exponential(0.5)", class = "sd", coef = "Intercept", group = "nrd")) #truncates above zero, allows SD to be wider for NRD effect

burn_int_col <- brm(b_burn01 ~
                                                 0 + Intercept +
                                                 trees100_9020 + 
                                                 trees1_9020 +
                                                 interaction +
                                                 group_involve2 +
                                                 (1|nrd),
                                             data = dat90_20_z, 
                                             warmup = 1000, iter = 3000, 
                                             cores = 2, chains = 2, 
                                             control = list(adapt_delta = 0.96),
                                             prior = prior_int_col,
                                             seed = 123, 
                                             family = bernoulli,
                                             sample_prior = T)

## predictive check
pp_check(burn_int_col, type = "error_scatter_avg_vs_x", size = 1.1,
         x = "interaction")

## is it the same as the manual option?
pred %>%
    ggplot() +
    geom_point(aes(x = interaction, y = b_burn01 - .prediction, color = as_factor(b_burn01)), alpha = 0.5) +
    labs(y = "Yobs - Predicted Yavg") +
    guides(color=guide_legend(title="Obs presc burning"))
