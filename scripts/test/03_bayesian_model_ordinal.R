library(brms)
library(rstan)
library(tidyverse)
library(ggmcmc)

# read in data and prep for model
dat90_20 <- 
    read_csv("data/processed/ProducerDF_TreeCoverChangeCounty.csv")

## removing NAs, making b_burn4 an ordinal variable
dat90_20_nas <- 
    na.omit(dat90_20 %>%
                dplyr::select(uniqueID, trees1_9020, trees100_9020, b_burn4, group_involve2, county2)) %>% #leaves 383 observations
    mutate(b_burn4 = ordered(b_burn4)) %>%
    filter(!trees1_9020< -0.1) # removing outlier
    

# model
burn1 <- brm(b_burn4 ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 I(trees1_9020^2) +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|county2),
             data = dat90_20_nas,
             family = cumulative,
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123)

summary(burn1)

## caterpillar plots
burn1_output <- 
    ggs(burn1)

ggplot(
    filter(burn1_output, 
           Parameter %in% c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_group_involve2", "b_Itrees1_9020E2", "b_trees100_9020:trees1_9020")),
    aes(x   = Iteration,
        y   = value, 
        col = as.factor(Chain)))+
    geom_line() +
    geom_vline(xintercept = 1000)+
    facet_grid(Parameter ~ . ,
               scale  = 'free_y',
               switch = 'y')+
    labs(title = "Caterpillar Plots", 
         col   = "Chains")

# posterior probability plots
tidybayes::get_variables(burn1)

burn1_output %>%
    dplyr::filter(Iteration > 1000,
                  Parameter %in% c("b_Intercept[1]", "b_Intercept[2]","b_Intercept[3]",
                                   "b_trees100_9020", "b_trees1_9020", "b_group_involve2",
                                   "b_Itrees1_9020E2", "b_trees100_9020:trees1_9020")) %>%
    ggplot(aes(x = value)) +
    geom_density() +
    facet_wrap(Parameter ~., scales="free") +
    geom_vline(xintercept = 0, col = "red", linewidth = 1) + 
    geom_vline(data = burn1_summary, aes(xintercept = as.numeric(lower)), col = "blue", linetype = 2) +
    geom_vline(data = burn1_summary, aes(xintercept = as.numeric(upper)), col = "blue", linetype = 2) 

# plots
## data and fit
data_plot = dat90_20_nas %>%
    ggplot(aes(x = trees1_9020, y = b_burn4, color = b_burn4)) +
    geom_point() +
    scale_color_brewer(palette = "Dark2", name = "b_burn4")

fit_plot = dat90_20_nas %>%
    modelr::data_grid(trees100_9020 = c(0.05),
                      county2 = "CUSTER",
                      group_involve2 = 0,
                      trees1_9020 = modelr::seq_range(trees1_9020, n = 101)) %>%
    add_epred_draws(burn1, value = "P(burn|trees1)", category = "b_burn4") %>%
    ggplot(aes(x = trees1_9020, y = `P(burn|trees1)`, color = b_burn4)) +
    stat_lineribbon(aes(fill = b_burn4), alpha = 1/5) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2")

cowplot::plot_grid(ncol = 1, align = "v",
          data_plot,
          fit_plot
)

## expected value grey lines "E[b_burn4 | trees1_9020]" and fit
data_plot_with_mean = dat90_20_nas %>%
    modelr::data_grid(trees100_9020 = c(0.05),
                      county2 = "CUSTER",
                      group_involve2 = 0,
                      trees1_9020 = modelr::seq_range(trees1_9020, n = 101)) %>%
    # NOTE: this shows the use of ndraws to subsample within add_epred_draws()
    # ONLY do this IF you are planning to make spaghetti plots, etc.
    # NEVER subsample to a small sample to plot intervals, densities, etc.
    add_epred_draws(burn1, value = "P(b_burn4 | trees1_9020)", category = "b_burn4", ndraws = 100) %>%
    group_by(trees1_9020, .draw) %>%
    # calculate expected cylinder value
    mutate(b_burn4 = as.numeric(as.character(b_burn4))) %>%
    summarise(b_burn4 = sum(b_burn4 * `P(b_burn4 | trees1_9020)`), .groups = "drop") %>%
    ggplot(aes(x = trees1_9020, y = b_burn4)) +
    geom_line(aes(group = .draw), alpha = 5/100) +
    geom_point(aes(y = as.numeric(as.character(b_burn4)), fill = b_burn4), data = dat90_20_nas, shape = 21, size = 2) +
    scale_fill_brewer(palette = "Set2", name = "b_burn4")

cowplot::plot_grid(ncol = 1, align = "v",
          data_plot_with_mean,
          fit_plot
)

# posterior predictive checks: do posterior predictions look like the data?
dat90_20_nas %>%
    # we use `select` instead of `data_grid` here because we want to make posterior predictions
    # for exactly the same set of observations we have in the original data
    select(trees1_9020, trees100_9020, group_involve2, county2) %>%
    add_predicted_draws(burn1, seed = 1234) %>%
    # recover original factor labels
    mutate(b_burn4 = levels(dat90_20_nas$b_burn4)[.prediction]) %>%
    ggplot(aes(x = trees1_9020, y = b_burn4)) +
    geom_count(color = "gray75") +
    geom_point(aes(fill = b_burn4), data = dat90_20_nas, shape = 21, size = 2) +
    scale_fill_brewer(palette = "Dark2") #+
    # geom_label_repel(
    #     data = . %>% ungroup() %>% filter(b_burn4 == "8") %>% filter(trees1_9020 == max(trees1_9020)) %>% dplyr::slice(1),
    #     label = "posterior predictions", xlim = c(26, NA), ylim = c(NA, 2.8), point.padding = 0.3,
    #     label.size = NA, color = "gray50", segment.color = "gray75"
    # ) +
    # geom_label_repel(
    #     data = dat90_20_nas %>% filter(b_burn4 == "6") %>% filter(trees1_9020 == max(trees1_9020)) %>% dplyr::slice(1),
    #     label = "observed data", xlim = c(26, NA), ylim = c(2.2, NA), point.padding = 0.2,
    #     label.size = NA, segment.color = "gray35"
    # )

# simulated dist of b_burn
dat90_20_nas %>%
    select(trees1_9020, trees100_9020, group_involve2, county2) %>%
    add_predicted_draws(burn1, ndraws = 100, seed = 12345) %>%
    # recover original factor labels
    mutate(b_burn4 = levels(dat90_20_nas$b_burn4)[.prediction]) %>%
    ggplot(aes(x = b_burn4)) +
    stat_count(aes(group = NA), geom = "line", data = dat90_20_nas, color = "red", linewidth = 3, alpha = .5) +
    stat_count(aes(group = .draw), geom = "line", position = "identity", alpha = .05) +
    geom_label(data = data.frame(b_burn4 = "1"), y = 9.5, label = "posterior\npredictions",
               hjust = 1, color = "gray50", lineheight = 1, label.size = NA) +
    geom_label(data = data.frame(b_burn4 = "4"), y = 14, label = "observed\ndata",
               hjust = 0, color = "red", lineheight = 1, label.size = NA)

# scatterplot matrix
set.seed(12345)

dat90_20_nas %>%
    select(trees1_9020, trees100_9020, group_involve2, county2) %>%
    add_predicted_draws(burn1) %>%
    # recover original factor labels. Must ungroup first so that the
    # factor is created in the same way in all groups; this is a workaround
    # because brms no longer returns labelled predictions (hopefully that
    # is fixed then this will no longer be necessary)
    ungroup() %>%
    mutate(b_burn4 = ordered(levels(dat90_20_nas$b_burn4)[.prediction], levels(dat90_20_nas$b_burn4))) %>%
    # need .drop = FALSE to ensure 0 counts are not dropped
    group_by(.draw, .drop = FALSE) %>%
    count(b_burn4) %>%
    gather_pairs(b_burn4, n) %>%
    ggplot(aes(.x, .y)) +
    geom_count(color = "gray75") +
    geom_point(data = dat90_20_nas %>% count(b_burn4) %>% gather_pairs(b_burn4, n), color = "red") +
    facet_grid(vars(.row), vars(.col)) +
    xlab("Number of observations with b_burn4 = col") +
    ylab("Number of observations with b_burn4 = row") 

# deviance
## log likelihoods
ll <-
    burn1 %>%
    log_lik() %>%
    as_tibble()

## Bayesian deviance
dfmean <-
    ll %>%
    exp() %>%
    summarise_all(mean) %>%
    gather(key, means) %>%
    select(means) %>%
    log()

(
    lppd <-
        dfmean %>%
        sum()
)

## effective number of parameters
dfvar <-
    ll %>%
    summarise_all(var) %>%
    gather(key, vars) %>%
    select(vars) 

pwaic <-
    dfvar %>%
    sum()

pwaic

## WAIC
-2 * (lppd - pwaic)
waic(burn1)

## WAIC standard error
dfmean %>%
    mutate(waic_vec   = -2 * (means - dfvar$vars)) %>%
    summarise(waic_se = (var(waic_vec) * nrow(dfmean)) %>% sqrt())

# r2
bayes_R2(burn1)
bayes_R2(remo1)
