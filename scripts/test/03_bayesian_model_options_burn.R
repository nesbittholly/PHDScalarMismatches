library(brms)
library(rstan)
library(tidyverse)
library(ggmcmc)

# read in data and prep for model
dat90_20 <- 
    read_csv("data/processed/ProducerDF_TreeCoverChangeCounty_NRD.csv")

## removing NAs
dat90_20_nas <- 
    na.omit(dat90_20 %>%
                dplyr::select(uniqueID, trees1_9020, trees100_9020, group_involve2,
                              #X, # not including X, interested in trees more than longitude
                              b_burn01, b_remo01, 
                              nrd
                              #county2, eco, no_share, Y
                )) #leaves 396 observations

## rescale
#divide continuous variables by 2 sds instead of 1 so that they are comparable on the same scale (gelman paper)
dat90_20_z<-dat90_20_nas %>% 
    mutate(trees1_9020 = arm::rescale(trees1_9020),
           trees100_9020 = arm::rescale(trees100_9020))
           #X = arm::rescale(X))

# models
burn1 <- brm(b_burn01 ~
                 #0 + Intercept +
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                 group_involve2,
             data = dat90_20_z, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             control = list(adapt_delta = 0.96),
             #prior = prior1,
             seed = 123, 
             family = bernoulli,
             sample_prior = T) #saves priors for plots later
summary(burn1)

burn2 <- brm(b_burn01 ~
                 #0 + Intercept +
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|nrd),
             data = dat90_20_z, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             control = list(adapt_delta = 0.96),
             #prior = prior1,
             seed = 123, 
             family = bernoulli,
             sample_prior = T) #saves priors for plots later
summary(burn2)

burn3 <- brm(b_burn01 ~
                 #0 + Intercept +
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                 group_involve2 + 
                 (1 + group_involve2|nrd),
             data = dat90_20_z, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             control = list(adapt_delta = 0.96),
             #prior = prior1,
             seed = 123, 
             family = bernoulli,
             sample_prior = T) #saves priors for plots later
summary(burn3) #random slope is not adding much to the model?

loo_compare(loo(burn1), loo(burn2), loo(burn3)) #rule of thumb diff of |4| or less is not meaningful - https://users.aalto.fi/%7Eave/CV-FAQ.html

# set priors
## find out what priors were set to automatically
get_prior(b_burn01 ~
              1 +
              trees100_9020 + 
              trees1_9020 +
              trees100_9020*trees1_9020 +
              group_involve2 +
              (1 + group_involve2|nrd),
          data = dat90_20_nas)

## view distributions for priors
hist(rnorm(n = 1000, mean = 0, sd = 1)) #use normal(0,1) for prior
hist(rt(n = 1000, df = 7)) #can't change mean and sd with rt though
hist(ggdist::rstudent_t(n = 1000, df = 7, mu = 0, sigma = 1.5)) #use student_t(7, 0, 1.5) for prior
hist(rexp(n = 1000,rate = 1)) #use exponential(1) for sigma prior?
hist(rcauchy(n = 1000, location = 0, scale = 2)) #use cauchy(0,2) for sd prior?
hist(MCMCpack::rinvgamma(n = 1000, shape = 0.5, 0.5)) #last number is rate; use inv_gamma(0.5, 0.5)

## set new priors - https://www.rensvandeschoot.com/brms-wambs/
prior1 <- c(set_prior("normal(0,1)", class = "b", coef = "group_involve2"), # expect that larger numbers would be less likely
            set_prior("normal(0,1)", class = "b", coef = "trees1_9020"),
            set_prior("normal(0,1)", class = "b", coef = "trees100_9020"),
            set_prior("normal(0,1)", class = "b", coef = "trees100_9020:trees1_9020"),
            set_prior("student_t(7, 0, 1.5)", class = "b", coef = "Intercept")) # expect a narrow distribution

prior2 <- c(set_prior("normal(0,1)", class = "b", coef = "group_involve2"), # expect that larger numbers would be less likely
            set_prior("normal(0,1)", class = "b", coef = "trees1_9020"),
            set_prior("normal(0,1)", class = "b", coef = "trees100_9020"),
            set_prior("normal(0,1)", class = "b", coef = "trees100_9020:trees1_9020"),
            set_prior("student_t(7, 0, 1.5)", class = "b", coef = "Intercept"), # expect a narrow distribution
            set_prior("cauchy(0,2)", class = "sd")) # a half cauchy distribution (truncated at 0) for the sd (special case of student t)
            # set_prior("exponential(1)", class = "sd")) #truncates above 0

prior3 <- c(set_prior("normal(0,1)", class = "b", coef = "group_involve2"), # expect that larger numbers would be less likely
            set_prior("normal(0,1)", class = "b", coef = "trees1_9020"),
            set_prior("normal(0,1)", class = "b", coef = "trees100_9020"),
            set_prior("normal(0,1)", class = "b", coef = "trees100_9020:trees1_9020"),
            set_prior("student_t(7, 0, 1.5)", class = "b", coef = "Intercept"), # expect a narrow distribution
            set_prior("cauchy(0,2)", class = "sd"), # a half cauchy distribution (truncated at 0) for the sd (special case of student t)
            set_prior("lkj(2)", class = "cor"), # a Cholesky of 2 for the correlation
            # set_prior("exponential(1)", class = "sigma")) #truncates above 0
            set_prior("inv_gamma(.5,.5)", class = "sigma")) # an uniformative inverse gamma for the sigma. 

## new models with priors
burn1_p1 <- brm(b_burn01 ~
                 0 + Intercept +
                 #1 +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                 group_involve2,
             data = dat90_20_z, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             control = list(adapt_delta = 0.96),
             prior = prior1,
             seed = 123, 
             family = bernoulli,
             sample_prior = T) #saves priors for plots later
summary(burn1_p1)
round(100*((summary(burn1_p1)$fixed - summary(burn1)$fixed) / summary(burn1)$fixed), 3)[,"Estimate"]

burn2_p2 <- brm(b_burn01 ~
                 0 + Intercept +
                 #1 +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|nrd),
             data = dat90_20_z, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             control = list(adapt_delta = 0.96),
             prior = prior2,
             seed = 123, 
             family = bernoulli,
             sample_prior = T) #saves priors for plots later
summary(burn2_p2)
round(100*((summary(burn2_p2)$fixed - summary(burn2)$fixed) / summary(burn2)$fixed), 3)[,"Estimate"]

burn3_p3 <- brm(b_burn01 ~
                 0 + Intercept +
                 #1 +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                 group_involve2 + 
                 (1 + group_involve2|nrd),
             data = dat90_20_z, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             control = list(adapt_delta = 0.96),
             prior = prior3,
             seed = 123, 
             family = bernoulli,
             sample_prior = T) #saves priors for plots later
summary(burn3_p3) #random slope is not adding much to the model?

## plots of priors
#plot(hypothesis(burn2, "group_involve2 = 1"))
plot(hypothesis(burn2_p2, "group_involve2 = 1"))

#plot(hypothesis(burn2, "trees100_9020 > 0"))
plot(hypothesis(burn2_p2, "trees100_9020 > 0"))

#plot(hypothesis(burn2, "trees1_9020 > 0"))
plot(hypothesis(burn2_p2, "trees1_9020 > 0"))

#plot(hypothesis(burn2, "Intercept > 0"))
plot(hypothesis(burn2_p2, "Intercept > 0"))

# choose model to do further diagnostics with
loo_compare(loo(burn1_p1), loo(burn1), loo(burn2_p2), loo(burn2)) #burn2_p2 
