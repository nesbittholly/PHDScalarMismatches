library(brms)
library(rstan)
library(tidyverse)
library(ggmcmc)

# read in data and prep for model
dat90_20 <- 
    read_csv("data/processed/ProducerDF_TreeCoverChangeCounty_nrd.csv")

## removing NAs
dat90_20_nas <- 
    na.omit(dat90_20 %>%
                dplyr::select(uniqueID, trees1_9020, trees100_9020, b_remo, b_burn, group_involve2, county2, nrd, eco, no_share, X, Y)) #leaves 383 observations

## contingency tables
dat90_20_nas %>%
    select(b_burn, group_involve2) %>%
    group_by(b_burn) %>%
    mutate(n = n()) %>%
    group_by(b_burn, group_involve2) %>%
    mutate(n2 = n(),
           percent = n2/n) %>%
    unique() %>%
    arrange(b_burn)

dat90_20_nas %>%
    select(b_burn, nrd) %>%
    group_by(b_burn) %>%
    mutate(n = n()) %>%
    group_by(b_burn, nrd) %>%
    mutate(n2 = n(),
           percent = n2/n) %>%
    unique() %>%
    arrange(b_burn, nrd)
    

## random intercept by some type of regional variable?
dat90_20_nas %>%
    group_by(nrd) %>%
    summarize(n = n())

dat90_20_nas %>%
    group_by(county2) %>%
    summarize(n = n())%>%
    print(n = Inf) #many counties with only one observation

dat90_20_nas %>%
    group_by(eco) %>%
    summarize(n = n()) #two ecoregions with only one observation

## standardizing # don't really need to standardize because it's just 0s and 1s and percent tree cover change
# dat90_20_z <-as_tibble(scale(dat90_20_nas%>%dplyr::select(trees1_9020, trees100_9020)))#%>%st_drop_geometry))
# dat90_20_z <-dat90_20_nas%>%dplyr::select(-c(trees1_9020, trees100_9020))%>%bind_cols(dat90_20_z)

# burn models
## burn
burn1 <- brm(b_burn ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                 group_involve2,
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123, 
             family = bernoulli)

burn1.1 <- brm(b_burn ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                   I(trees1_9020^2) +
                 group_involve2,
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123, 
             family = bernoulli)

burn1.2 <- brm(b_burn ~
                   1 +
                   trees100_9020 + 
                   trees1_9020 +
                   trees100_9020*trees1_9020 +
                   X +
                   group_involve2,
               data = dat90_20_nas, 
               warmup = 1000, iter = 3000, 
               cores = 2, chains = 2, 
               seed = 123, 
               family = bernoulli)

burn2 <- brm(b_burn ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|nrd),
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123, 
             family = bernoulli)

burn2.2 <- brm(b_burn ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1 + group_involve2|nrd),
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123, 
             family = bernoulli)

burn2.3 <- brm(b_burn ~ 
                   1 +
                   trees100_9020 + 
                   trees1_9020 +
                   trees100_9020*trees1_9020 +
                   group_involve2 +
                   X +
                   (1 + group_involve2|nrd),
               data = dat90_20_nas, 
               warmup = 1000, iter = 3000, 
               cores = 2, chains = 2, 
               seed = 123, 
               family = bernoulli)

burn3 <- brm(b_burn ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|county2),
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123, 
             family = bernoulli)

# outputs
summary(burn1) #CI far from 0
summary(burn1.1) #CI far from 0
summary(burn1.2)
summary(burn2) #CI crossing 0
summary(burn2.2)
summary(burn3) #CI almost touching 0

# R2
rbind(bayes_R2(burn1),#worst
      bayes_R2(burn1.1),
      bayes_R2(burn1.2),
      bayes_R2(burn2), #very close second best
      bayes_R2(burn2.2), #best
      bayes_R2(burn3)) #close second best

# WAIC
waic(burn1) #worst
waic(burn1.1) 
waic(burn1.2) #new best
waic(burn2) #best (old)
waic(burn2.2) #second best (old)
waic(burn3) #close second worst

# loo
loo(burn1) #worst
loo(burn1.1)
loo(burn1.2) #new best
loo(burn2) #best (old)
loo(burn2.2)
loo(burn3) #worst
#expected log predictive density - "a measure of the predictive accuracy of a model on a new dataset generated by the same data generating process as the data used for model fitting"

loo_compare(loo(burn1), loo(burn2), loo(burn2.2)) #rule of thumb diff of |4| or less is not meaningful 
# https://users.aalto.fi/%7Eave/CV-FAQ.html

#https://cran.r-project.org/web/packages/loo/vignettes/loo2-example.html
plot(loo(burn2.2))


# model checks
plot(burn1)
plot(burn1.1)
plot(burn1.2)

bayesplot::mcmc_acf(burn1.1)
bayesplot::mcmc_acf(burn1.1)
bayesplot::mcmc_acf(burn1.2)


# diagnostics #https://bookdown.org/marklhc/notes_bookdown/model-diagnostics.html
## posterior predictive check
pp_check(burn1, ndraws = 100) 
pp_check(burn1.1, ndraws = 100)

## posterior predictive checks with mean, max, min of outcome
pp_check(burn1, type = "stat_grouped", stat = "mean", group = "group_involve2")
pp_check(burn1, type = "stat_2d", stat = c("max", "min"))

pp_check(burn1.1, type = "stat_grouped", stat = "mean", group = "group_involve2")
pp_check(burn1.1, type = "stat_2d", stat = c("max", "min"))

pp_check(burn1.2, type = "stat_grouped", stat = "mean", group = "group_involve2")
pp_check(burn1.2, type = "stat_2d", stat = c("max", "min"))

## check for outliers
pp_check(burn1, type = "ribbon_grouped", x = "trees1_9020", group = "group_involve2")
pp_check(burn1.1, type = "ribbon_grouped", x = "trees1_9020", group = "group_involve2")
pp_check(burn1.2, type = "ribbon_grouped", x = "trees1_9020", group = "group_involve2")

## linearity with marginal model plots
source("scripts/mmp_brm.R")
mmp_brm(burn1, x = "trees1_9020", prob = .95)
mmp_brm(burn1, x = "trees100_9020", prob = .95)

mmp_brm(burn1.1, x = "trees1_9020", prob = .95)
mmp_brm(burn1.1, x = "trees100_9020", prob = .95)

mmp_brm(burn1.2, x = "trees1_9020", prob = .95)
mmp_brm(burn1.2, x = "trees100_9020", prob = .95)
mmp_brm(burn1.2, x = "X", prob = .95)

## residual plots
pp_check(burn1, type = "error_scatter_avg_vs_x", size = 1.1, 
         x = "trees1_9020")
pp_check(burn1, type = "error_scatter_avg_vs_x", size = 1.1, 
         x = "trees100_9020")

pp_check(burn1.1, type = "error_scatter_avg_vs_x", size = 1.1, 
         x = "trees1_9020")
pp_check(burn1.1, type = "error_scatter_avg_vs_x", size = 1.1, 
         x = "trees100_9020")

pp_check(burn1.2, type = "error_scatter_avg_vs_x", size = 1.1, 
         x = "trees1_9020")
pp_check(burn1.2, type = "error_scatter_avg_vs_x", size = 1.1, 
         x = "trees100_9020")
pp_check(burn1.2, type = "error_scatter_avg_vs_x", size = 1.1, 
         x = "X")

### standardized residuals
burn1$data %>% 
    mutate(predict_y = predict(burn1)[ , "Estimate"], 
           std_resid = residuals(burn1, type = "pearson")[ , "Estimate"]) %>%
    ggplot(aes(predict_y, std_resid)) + 
    geom_point(size = 0.8) + stat_smooth(se = FALSE)

## multicollinearity
pairs(burn1, pars = "b", 
      off_diag_args =  # arguments of the scatterplots
          list(size = 0.5,  # point size
               alpha = 0.25))  # transparency

pairs(burn1.2, pars = "b", # X is perfectly(?) correlated w intercept, and verrry correlated with trees100
      off_diag_args =  # arguments of the scatterplots
          list(size = 0.5,  # point size
               alpha = 0.25))  # transparency



# cross-validation
#burn1 - 74%
#burn2 - 75%
#burn3 - some of the counties aren't in the training set causing the testing set to not work I think?

#remo1 - 28%
#remo2 - 41%
#remo3 - 42%

#next - do remo

# rmse and mae
kcv <- kfold(burn1)
predicts <- rstantools::loo_linpred(burn1, kcv)
rmse(data$y, colMeans(predicts))
mae(data$y, apply(predicts, 2, median))
