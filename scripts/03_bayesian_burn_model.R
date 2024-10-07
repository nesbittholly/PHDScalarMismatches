library(brms)
library(rstan)
library(tidyverse)

# read in data and prep for model
dat90_20 <- 
    read_csv("data/processed/ProducerDF_TreeCoverChangeCounty_nrd.csv") 

## removing NAs
dat90_20_nas <- 
    na.omit(dat90_20 %>%
                dplyr::select(uniqueID, trees1_9020, trees100_9020, b_burn01, b_remo01, group_involve2, nrd
                              #county2, eco, no_share, X, Y
                )) #leaves 396 observations

## rescale
#divide continuous variables by 2 sds instead of 1 so that they are comparable on the same scale (gelman paper)
dat90_20_z<-dat90_20_nas %>% 
    mutate(trees1_9020 = arm::rescale(trees1_9020),
           trees100_9020 = arm::rescale(trees100_9020))

# set new priors
brms::get_prior(b_burn01 ~
                    0 + Intercept +
                    trees100_9020 + 
                    trees1_9020 +
                    trees100_9020*trees1_9020 +
                    group_involve2 +
                    (1|nrd),
                data = dat90_20_z, family = bernoulli)

prior1 <- c(set_prior("normal(0,1)", class = "b", coef = "group_involve2"), #expect that larger numbers would be less likely
            set_prior("normal(0,1)", class = "b", coef = "trees1_9020"),
            set_prior("normal(0,1)", class = "b", coef = "trees100_9020"),
            set_prior("normal(0,1)", class = "b", coef = "trees100_9020:trees1_9020"),
            set_prior("student_t(7, 0, 1.5)", class = "b", coef = "Intercept"), #expect a narrow distribution
            set_prior("exponential(1)", class = "sd"), #truncates above 0
            set_prior("exponential(0.5)", class = "sd", coef = "Intercept", group = "nrd")) #truncates above zero, allows SD to be wider for NRD effect

# model
burn <- brm(b_burn01 ~
                 0 + Intercept +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|nrd),
             data = dat90_20_z, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 4, 
             control = list(adapt_delta = 0.96),
             prior = prior1,
             seed = 123, 
             family = bernoulli,
             sample_prior = T) #saves priors for plots later

prior_summary(burn)

summary(burn)
