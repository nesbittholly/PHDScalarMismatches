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
                dplyr::select(uniqueID, trees1_9020, trees100_9020, b_remo, b_burn, group_involve2, county2, nrd, eco, no_share)) #leaves 387 observations

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

# intercept only models
## burn
burn0 <- brm(b_burn ~ 1 + (1|nrd),  
                data = dat90_20_nas, 
                warmup = 1000, iter = 3000, 
                cores = 2, chains = 2, 
                seed = 123, 
                family = bernoulli)

summary(burn0)
bayes_R2(burn0)

## remo
remo0 <- brm(b_remo ~ 1 + (1|nrd),  
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123, 
             family = bernoulli) 
summary(remo0)
bayes_R2(remo0)

# adding first order predictors
## burn
burn1 <- brm(b_burn ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 group_involve2 +
                 (1|nrd),
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123,
             family = bernoulli)

summary(burn1)
bayes_R2(burn1)

### caterpillar plots
ggs(burn1) %>%
    filter(Parameter %in% c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_group_involve2")) %>%
    ggplot(aes(x = Iteration,
               y = value, 
               col = as.factor(Chain)))+
    geom_line() +
    geom_vline(xintercept = 1000)+
    facet_grid(Parameter ~ . ,
               scale  = 'free_y',
               switch = 'y')+
    labs(title = "Caterpillar Plots", 
         col   = "Chains")

burn1_summary <- summary(burn1)$fixed[,3:4] %>%
    tibble::rownames_to_column("Parameter1") %>%
    dplyr::mutate(Parameter = c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_group_involve2")) %>%
    arrange(factor(Parameter, levels = c("b_group_involve2", "b_Intercept","b_trees100_9020", "b_trees1_9020")))
colnames(burn1_summary)[2:3] <- c("lower", "upper")
burn1_summary    

ggs(burn1) %>%
    dplyr::filter(Iteration > 1000,
                  Parameter %in% c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_group_involve2")) %>%
    ggplot(aes(x = value)) +
    geom_density() +
    facet_wrap(Parameter ~., scales="free") +
    geom_vline(xintercept = 0, col = "red", linewidth = 1) + 
    geom_vline(data = burn1_summary, aes(xintercept = as.numeric(lower)), col = "blue", linetype = 2) +
    geom_vline(data = burn1_summary, aes(xintercept = as.numeric(upper)), col = "blue", linetype = 2) 

## remo
remo1 <- brm(b_remo ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 group_involve2 +
                 (1|nrd),
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123,
             family = bernoulli)

summary(remo1)
bayes_R2(remo1)

### caterpillar plots
ggs(remo1) %>%
    dplyr::filter(Parameter %in% c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_group_involve2")) %>%
    ggplot(aes(x = Iteration,
               y = value, 
               col = as.factor(Chain))) +
    geom_line() +
    geom_vline(xintercept = 1000) +
    facet_grid(Parameter ~ . ,
               scale  = 'free_y',
               switch = 'y') +
    labs(title = "Caterpillar Plots", 
         col   = "Chains")

remo1_summary <- summary(remo1)$fixed[,3:4] %>%
    tibble::rownames_to_column("Parameter1") %>%
    dplyr::mutate(Parameter = c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_group_involve2")) %>%
    arrange(factor(Parameter, levels = c("b_group_involve2", "b_Intercept","b_trees100_9020", "b_trees1_9020")))
colnames(remo1_summary)[2:3] <- c("lower", "upper")
remo1_summary    

ggs(remo1) %>%
    dplyr::filter(Iteration > 1000,
                  Parameter %in% c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_group_involve2")) %>%
    ggplot(aes(x = value)) +
    geom_density() +
    facet_wrap(Parameter ~., scales="free") +
    geom_vline(xintercept = 0, col = "red", linewidth = 1) + 
    geom_vline(data = remo1_summary, aes(xintercept = as.numeric(lower)), col = "blue", linetype = 2) +
    geom_vline(data = remo1_summary, aes(xintercept = as.numeric(upper)), col = "blue", linetype = 2) 

# adding second order predictors
## burn
burn2 <- brm(b_burn ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 #I(trees1_9020^2) +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|nrd),
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123,
             family = bernoulli)

summary(burn2)
bayes_R2(burn2)

### caterpillar plots
ggs(burn2) %>%
    filter(Parameter %in% c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_group_involve2", #"b_Itrees1_9020E2", 
                            "b_trees100_9020:trees1_9020")) %>%
    ggplot(aes(x   = Iteration,
        y   = value, 
        col = as.factor(Chain)))+
    geom_line() +
    geom_vline(xintercept = 1000)+
    facet_grid(Parameter ~ . ,
               scale  = 'free_y',
               switch = 'y')+
    labs(title = "Caterpillar Plots", 
         col   = "Chains")

burn2_summary <- summary(burn2)$fixed[,3:4] %>%
    tibble::rownames_to_column("Parameter1") %>%
    dplyr::mutate(Parameter = c("b_Intercept", "b_trees100_9020", "b_trees1_9020", #"b_Itrees1_9020E2",
                                "b_group_involve2",  "b_trees100_9020:trees1_9020")) %>%
    arrange(factor(Parameter, levels = c("b_group_involve2", "b_Intercept",#"b_Itrees1_9020E2", 
                                         "b_trees1_9020",
                                         "b_trees100_9020", "b_trees100_9020:trees1_9020")))
colnames(burn2_summary)[2:3] <- c("lower", "upper")
burn2_summary    

ggs(burn2) %>%
    dplyr::filter(Iteration > 1000,
                  Parameter %in% c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_group_involve2", #"b_Itrees1_9020E2", 
                                   "b_trees100_9020:trees1_9020")) %>%
    ggplot(aes(x = value)) +
    geom_density() +
    facet_wrap(Parameter ~., scales="free") +
    geom_vline(xintercept = 0, col = "red", linewidth = 1) + 
    geom_vline(data = burn2_summary, aes(xintercept = as.numeric(lower)), col = "blue", linetype = 2) +
    geom_vline(data = burn2_summary, aes(xintercept = as.numeric(upper)), col = "blue", linetype = 2) 

## remo
remo2 <- brm(b_remo ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 #I(trees1_9020^2) +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|nrd),
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123,
             family = bernoulli)

summary(remo2)
bayes_R2(remo2)

### caterpillar plots
ggs(remo2) %>%
    filter(Parameter %in% c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_group_involve2", #"b_Itrees1_9020E2", 
                            "b_trees100_9020:trees1_9020")) %>%
    ggplot(aes(x   = Iteration,
        y   = value, 
        col = as.factor(Chain)))+
    geom_line() +
    geom_vline(xintercept = 1000)+
    facet_grid(Parameter ~ . ,
               scale  = 'free_y',
               switch = 'y')+
    labs(title = "Caterpillar Plots", 
         col   = "Chains")

remo2_summary <- summary(remo2)$fixed[,3:4] %>%
    tibble::rownames_to_column("Parameter1") %>%
    dplyr::mutate(Parameter = c("b_Intercept", "b_trees100_9020", "b_trees1_9020", #"b_Itrees1_9020E2",
                                "b_group_involve2",  "b_trees100_9020:trees1_9020")) %>%
    arrange(factor(Parameter, levels = c("b_group_involve2", "b_Intercept",#"b_Itrees1_9020E2", 
                                         "b_trees1_9020",
                                         "b_trees100_9020", "b_trees100_9020:trees1_9020")))
colnames(remo2_summary)[2:3] <- c("lower", "upper")
remo2_summary    

ggs(remo2) %>%
    dplyr::filter(Iteration > 1000,
                  Parameter %in% c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_group_involve2", #"b_Itrees1_9020E2", 
                                   "b_trees100_9020:trees1_9020")) %>%
    ggplot(aes(x = value)) +
    geom_density() +
    facet_wrap(Parameter ~., scales="free") +
    geom_vline(xintercept = 0, col = "red", linewidth = 1) + 
    geom_vline(data = remo2_summary, aes(xintercept = as.numeric(lower)), col = "blue", linetype = 2) +
    geom_vline(data = remo2_summary, aes(xintercept = as.numeric(upper)), col = "blue", linetype = 2) 

# adding square term for local-level change
## burn
burn3 <- brm(b_burn ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 I(trees1_9020^2) +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|nrd),
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123,
             family = bernoulli)

summary(burn3)
bayes_R2(burn3)

### caterpillar plots
ggs(burn3) %>%
    filter(Parameter %in% c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_group_involve2", "b_Itrees1_9020E2", 
                            "b_trees100_9020:trees1_9020")) %>%
    ggplot(aes(x   = Iteration,
               y   = value, 
               col = as.factor(Chain)))+
    geom_line() +
    geom_vline(xintercept = 1000)+
    facet_grid(Parameter ~ . ,
               scale  = 'free_y',
               switch = 'y')+
    labs(title = "Caterpillar Plots", 
         col   = "Chains")

burn3_summary <- summary(burn3)$fixed[,3:4] %>%
    tibble::rownames_to_column("Parameter1") %>%
    dplyr::mutate(Parameter = c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_Itrees1_9020E2",
                                "b_group_involve2",  "b_trees100_9020:trees1_9020")) %>%
    arrange(factor(Parameter, levels = c("b_group_involve2", "b_Intercept","b_Itrees1_9020E2", 
                                         "b_trees1_9020",
                                         "b_trees100_9020", "b_trees100_9020:trees1_9020")))
colnames(burn2_summary)[2:3] <- c("lower", "upper")
burn3_summary    

ggs(burn3) %>%
    dplyr::filter(Iteration > 1000,
                  Parameter %in% c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_group_involve2", "b_Itrees1_9020E2", "b_trees100_9020:trees1_9020")) %>%
    ggplot(aes(x = value)) +
    geom_density() +
    facet_wrap(Parameter ~., scales="free") +
    geom_vline(xintercept = 0, col = "red", linewidth = 1) + 
    geom_vline(data = burn2_summary, aes(xintercept = as.numeric(lower)), col = "blue", linetype = 2) +
    geom_vline(data = burn2_summary, aes(xintercept = as.numeric(upper)), col = "blue", linetype = 2) 

tidybayes::get_variables(burn3)
posterior::summarize_draws(tidybayes::tidy_draws(burn3))

burn3 %>%
    gather_draws(b_Intercept, b_trees100_9020, b_trees1_9020, b_group_involve2, b_Itrees1_9020E2, `b_trees100_9020:trees1_9020`) %>%
    median_qi(.width = c(.95, .66)) %>%
    ggplot(aes(y = .variable, x = .value, xmin = .lower, xmax = .upper)) +
    geom_pointinterval() +
    facet_wrap(.variable ~., scales="free")+
    geom_vline(xintercept = 0, col = "red", linewidth = 1)

## remo
remo3 <- brm(b_remo ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 I(trees1_9020^2) +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 (1|nrd),
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123,
             family = bernoulli)

summary(remo3)
bayes_R2(remo3)

### caterpillar plots
ggs(remo3) %>%
    filter(Parameter %in% c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_group_involve2", "b_Itrees1_9020E2", 
                            "b_trees100_9020:trees1_9020")) %>%
    ggplot(aes(x   = Iteration,
               y   = value, 
               col = as.factor(Chain)))+
    geom_line() +
    geom_vline(xintercept = 1000)+
    facet_grid(Parameter ~ . ,
               scale  = 'free_y',
               switch = 'y')+
    labs(title = "Caterpillar Plots", 
         col   = "Chains")

remo3_summary <- summary(remo3)$fixed[,3:4] %>%
    tibble::rownames_to_column("Parameter1") %>%
    dplyr::mutate(Parameter = c("b_Intercept", "b_trees100_9020", "b_trees1_9020", #"b_Itrees1_9020E2",
                                "b_group_involve2",  "b_trees100_9020:trees1_9020")) %>%
    arrange(factor(Parameter, levels = c("b_group_involve2", "b_Intercept",#"b_Itrees1_9020E2", 
                                         "b_trees1_9020",
                                         "b_trees100_9020", "b_trees100_9020:trees1_9020")))
colnames(remo3_summary)[2:3] <- c("lower", "upper")
remo3_summary    

ggs(remo3) %>%
    dplyr::filter(Iteration > 1000,
                  Parameter %in% c("b_Intercept", "b_trees100_9020", "b_trees1_9020", "b_group_involve2", #"b_Itrees1_9020E2", 
                                   "b_trees100_9020:trees1_9020")) %>%
    ggplot(aes(x = value)) +
    geom_density() +
    facet_wrap(Parameter ~., scales="free") +
    geom_vline(xintercept = 0, col = "red", linewidth = 1) + 
    geom_vline(data = remo2_summary, aes(xintercept = as.numeric(lower)), col = "blue", linetype = 2) +
    geom_vline(data = remo2_summary, aes(xintercept = as.numeric(upper)), col = "blue", linetype = 2) 


## adding variable for nrd cost-sharing - doesn't really make sense because I am already controlling for nrd-level effect (Twin PLatte, Upper Loup)
### burn
burn4 <- brm(b_burn ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 I(trees1_9020^2) +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 no_share +
                 (1|nrd),
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123,
             family = bernoulli)

### remo
remo4 <- brm(b_remo ~
                 1 +
                 trees100_9020 + 
                 trees1_9020 +
                 I(trees1_9020^2) +
                 trees100_9020*trees1_9020 +
                 group_involve2 +
                 no_share +
                 (1|nrd),
             data = dat90_20_nas, 
             warmup = 1000, iter = 3000, 
             cores = 2, chains = 2, 
             seed = 123,
             family = bernoulli)


#https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/overfitting-regularization-and-information-criteria.html#information-criteria
# R2
rbind(bayes_R2(burn0), #worst
      bayes_R2(burn1), 
      bayes_R2(burn2),
      bayes_R2(burn3),
      bayes_R2(burn4),#best
      bayes_R2(remo0), #worst
      bayes_R2(remo1),
      bayes_R2(remo2),
      bayes_R2(remo3),
      bayes_R2(remo4)) #best

# WAIC
waic(burn0) #null is worst
waic(burn1)
waic(burn2) 
waic(burn3) #burn3 is best
waic(burn4)

waic(remo0) #null is best
waic(remo1)
waic(remo2) 
waic(remo3)
waic(remo4) #remo4 is worst

#loo package - https://cran.r-project.org/web/packages/loo/vignettes/loo2-example.html - no just used manual cross-validation

# looking more closely at nrd-level effect
ggplot(data = dat90_20_nas, 
       aes(x   = trees100_9020, #swap trees1_9020
           y   = b_burn, #swap b_remo 
           col = nrd))+
    viridis::scale_color_viridis(discrete = TRUE)+
    geom_point(size     = .7,
               alpha    = .8,
               position = position_jitter(width=0.1, height=0.1))+
    geom_smooth(method = lm,
                se     = FALSE, 
                size   = 2,
                alpha  = .8)+
    theme_minimal()

# posterior probabilities of nrd level effect - https://mjskay.github.io/tidybayes/articles/tidy-brms.html
tidybayes::get_variables(burn3)
burn3 %>%
    spread_draws(r_nrd[nrd, term]) %>%
    median_qi()

## intervals
burn3 %>% 
    spread_draws(b_Intercept, r_nrd[nrd,]) %>%
    median_qi(nrd_mean = b_Intercept + r_nrd, .width = c(.95, .66)) %>%
    ggplot(aes(y = nrd, x = nrd_mean, xmin = .lower, xmax = .upper)) +
    geom_pointinterval() 

remo3 %>% 
    spread_draws(b_Intercept, r_nrd[nrd,]) %>%
    median_qi(nrd_mean = b_Intercept + r_nrd, .width = c(.95, .66)) %>%
    ggplot(aes(y = nrd, x = nrd_mean, xmin = .lower, xmax = .upper)) +
    geom_pointinterval() 

## intervals with density plots
burn3 %>%
    spread_draws(b_Intercept, r_nrd[nrd,]) %>%
    mutate(nrd_mean = b_Intercept + r_nrd) %>%
    ggplot(aes(y = nrd, x = nrd_mean)) +
    ggdist::stat_halfeye()

remo3 %>%
    spread_draws(b_Intercept, r_nrd[nrd,]) %>%
    mutate(nrd_mean = b_Intercept + r_nrd) %>%
    ggplot(aes(y = nrd, x = nrd_mean)) +
    ggdist::stat_halfeye()



