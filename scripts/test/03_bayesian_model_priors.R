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

# find out what priors were set to automatically
get_prior(b_burn01 ~
              1 +
              trees100_9020 + 
              trees1_9020 +
              trees100_9020*trees1_9020 +
              group_involve2 +
              (1|nrd),
          data = dat90_20_nas)

# view distributions for priors
hist(rnorm(n = 1000, mean = 0, sd = 1)) #use normal(0,1) for prior
hist(rt(n = 1000, df = 7)) #can't change mean and sd with rt though
hist(ggdist::rstudent_t(n = 1000, df = 7, mu = 0, sigma = 1.5)) #use student_t(7, 0, 1.5) for prior
hist(rexp(n = 1000,rate = 1)) #use exponential(1) for prior
hist(rcauchy(n = 1000, location = 0, scale = 2)) #use cauchy(0,2) for prior

# set new priors

prior1 <- c(set_prior("normal(0,1)", class = "b", coef = "group_involve2"), # expect that larger numbers would be less likely
            set_prior("normal(0,1)", class = "b", coef = "trees1_9020"),
            set_prior("normal(0,1)", class = "b", coef = "trees100_9020"),
            set_prior("normal(0,1)", class = "b", coef = "trees100_9020:trees1_9020"),
            set_prior("student_t(7, 0, 1.5)", class = "b", coef = "Intercept"), # expect a narrow distribution
            set_prior("exponential(1)", class = "sd")) #truncates above 0
            #set_prior("cauchy(0,2)", class = "sd"))          # a half cauchy distribution (truncuated at 0) for the sd (special case of student t)
            #set_prior("lkj(2)", class = "cor"))              # a Cholesky of 2 for the correlation  
            #set_prior("inv_gamma(.5,.5)", class = "sigma")) # an uniformative inverse gamma for the sigma. 

# model
burn2 <- brm(b_burn01 ~
                 0 + Intercept +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|nrd),
             data = dat90_20_z, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             control = list(adapt_delta = 0.96),
             prior = prior1,
             seed = 123, 
             family = bernoulli,
             sample_prior = T) #saves priors for plots later

prior_summary(burn2)

plot(hypothesis(burn2, "group_involve2 = 1"))
plot(hypothesis(burn2, "trees100_9020 > 0"))
plot(hypothesis(burn2, "trees1_9020 > 0"))
plot(hypothesis(burn2, "Intercept > 0"))

remo2 <- brm(b_remo01 ~
                 0 + Intercept +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|nrd),
             data = dat90_20_z, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             prior = prior1,
             seed = 123, 
             family = bernoulli,
             sample_prior = T) #saves priors for plots later

prior_summary(remo2)

plot(hypothesis(remo2, "group_involve2 = 1"))
plot(hypothesis(remo2, "trees100_9020 > 0"))
plot(hypothesis(remo2, "trees1_9020 > 0"))
plot(hypothesis(remo2, "Intercept > 0"))

# step 2 - check trace plots for convergence
stanplot(burn2, type = "trace")

modelposterior <- as.mcmc(burn2) # with the as.mcmc() command we can use all the CODA package convergence statistics and plotting options
coda::gelman.diag(modelposterior[, 1:5]) #also given with summary command. Check that Rhats are all close to 1
summary(burn2) 
coda::gelman.plot(modelposterior[, 1:5])
coda::geweke.diag(modelposterior[, 1:5])
coda::geweke.plot(modelposterior[, 1:5])

# step 3 - does convergence remain after doubling the number of iterations?
burn2_iter2 <- brm(b_burn01 ~
                 0 + Intercept +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|nrd),
             data = dat90_20_z, 
             warmup = 1000, iter = 6000, 
             cores = 2, chains = 2, 
             control = list(adapt_delta = 0.96),
             prior = prior1,
             seed = 123, 
             family = bernoulli,
             sample_prior = T) #saves priors for plots later

modelposterior_iter2<- as.mcmc(burn2_iter2)
coda::gelman.diag(modelposterior_iter2[, 1:5])
coda::gelman.plot(modelposterior_iter2[, 1:5])

round(100*((summary(burn2_iter2)$fixed - summary(burn2)$fixed) / summary(burn2)$fixed), 3)[,"Estimate"]

# step 4 - does the posterior distribution histogram have enough information?
mcmc_plot(burn2, type = "hist")
mcmc_plot(burn2_iter2, type = "hist")

# step 5 - do the chains exhibit a strong degree of autocorrelation?
coda::autocorr.diag(modelposterior[,1:5], lags = c(0, 1,2,3,4, 5, 10, 50))

# step 6 - do the posterior distributions make substantive sense?
burn2 %>%
    tidybayes::gather_draws(b_Intercept, b_trees100_9020, b_trees1_9020, b_group_involve2, `b_trees100_9020:trees1_9020`) %>%
    ggplot(aes(y = reorder(.variable, -.value), x = .value)) +
    ggdist::stat_halfeye() +
    #facet_wrap(.variable ~., scales="free")+
    geom_vline(xintercept = 0, col = "red", linewidth = 1)

burn2 %>% # should sort this by longitude
    tidybayes::spread_draws(b_Intercept, r_nrd[nrd,]) %>%
    mutate(nrd_mean = b_Intercept + r_nrd) %>%
    ggplot(aes(y = nrd, x = nrd_mean)) +
    ggdist::stat_halfeye() + # Upper Niobrara and Upper Loup are in the NW corner
    geom_vline(xintercept = 0, col = "red", linewidth = 1) + #Lower Loup is pretty central, lower big blue is SE, and Central Platte is just south of Lower Loup
    lims(x = c(-2.5, 2.5)) #Twin Platte and Upper Loup have eliminated juniper cost share programs

# step 7 - do different specifications of the multivariate variance priors influence the results?
#specified sd above

# step 8 - is there a notable effect of the prior when compared with non-informative priors?
prior_uninf <- c(set_prior("normal(0,100)", class = "b", coef = "group_involve2"), # expect that larger numbers would be less likely
            set_prior("normal(0,100)", class = "b", coef = "trees1_9020"),
            set_prior("normal(0,100)", class = "b", coef = "trees100_9020"),
            set_prior("normal(0,100)", class = "b", coef = "trees100_9020:trees1_9020"),
            set_prior("student_t(1, 0, 2)", class = "b", coef = "Intercept"), # expect a narrow distribution
            set_prior("exponential(100)", class = "sd")) #truncates above 0
#set_prior("cauchy(0,2)", class = "sd"))          # a half cauchy distribution (truncuated at 0) for the sd (special case of student t)
#set_prior("lkj(2)", class = "cor"))              # a Cholesky of 2 for the correlation  
#set_prior("inv_gamma(.5,.5)", class = "sigma")) # an uniformative inverse gamma for the sigma. 

# model
burn2_uninf <- brm(b_burn01 ~
                 0 + Intercept +
                 trees100_9020 + 
                 trees1_9020 +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|nrd),
             data = dat90_20_z, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             control = list(adapt_delta = 0.96),
             prior = prior_uninf,
             seed = 123, 
             family = bernoulli,
             sample_prior = T) #saves priors for plots later

summary(burn2)
summary(burn2_uninf)

round(100*((summary(burn2_uninf)$fixed - summary(burn2)$fixed) / summary(burn2)$fixed), 3)[,"Estimate"] # gigantic effects

# step 9 - sensitivity analysis of priors?
# step 10 - report model
