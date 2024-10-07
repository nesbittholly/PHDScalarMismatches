#run 03_bayesian_burn_model.R first

# diagnostics #https://bookdown.org/marklhc/notes_bookdown/model-diagnostics.html
## posterior predictive check
pp_check(burn, ndraws = 100) #generated data matches real data quite closely - indicates model is correctly specified (partly assessed w posterior predictive checks

# ## posterior predictive checks with mean, max, min of outcome
# pp_check(burn, type = "stat_grouped", stat = "mean", group = "group_involve2") #fits
# pp_check(burn, type = "stat_2d", stat = c("max", "min")) #not that informative with only 0s and 1s.
# 
# ## check for outliers
# pp_check(burn, type = "ribbon_grouped", x = "trees1_9020", group = "group_involve2") #not that informative with only 0s and 1s

## linearity with marginal model plots
#not necessary - logistic regression does not assume a linear relationship
# source("scripts/mmp_brm.R")
# mmp_brm(burn, x = "trees1_9020", prob = .95)
# mmp_brm(burn, x = "trees100_9020", prob = .95) 

## residual plots
pp_check(burn, type = "error_scatter_avg_vs_x", size = 1.1, 
         x = "trees1_9020") #bands due to 0/1s, more error variation at lower values of x.
pp_check(burn, type = "error_scatter_avg_vs_x", size = 1.1, 
         x = "trees100_9020") #bands due to 0/1s, error declines as x increases

### standardized residuals
# burn$data %>% 
#     mutate(predict_y = predict(burn)[ , "Estimate"], 
#            std_resid = residuals(burn, type = "pearson")[ , "Estimate"]) %>%
#     ggplot(aes(predict_y, std_resid)) + 
#     geom_point(size = 0.8) + stat_smooth(se = FALSE) #looks bad but i believe this is because of the 0/1s

## multicollinearity
pairs(burn, pars = "b", # no problems
      off_diag_args =  # arguments of the scatterplots
          list(size = 0.5,  # point size
               alpha = 0.25))[1:5,1:5]

# model checks
plot(burn) # chains converge
bayesplot::mcmc_acf(burn) # no issues with autocorrelation? see below for more

# bayes R2
bayes_R2(burn)

# loo
loo1<-loo(burn, save_psis = T)

burn0 <- update(burn, formula = b_burn01 ~ 1, refresh = 0, newdata = dat90_20_z)

loo0<-loo(burn0)
loo_compare(loo0, loo1)

# waic
waic(burn)
waic(burn0)

# classification error - https://avehtari.github.io/modelselection/diabetes.html
## Predicted probabilities
linpred <- posterior_linpred(burn)
preds <- posterior_epred(burn)
pred <- colMeans(preds)
pr <- as.integer(pred >= 0.5)

## posterior classification accuracy
dat90_20_z %>% 
    dplyr::select(b_burn01) %>%
    bind_cols(pr) %>%
    rename(predicted = `...2`) %>%
    mutate(correct = if_else(b_burn01 == predicted, 1, 0)) %>%
    summarize(mean_correct = mean(correct))

# LOO predictive probabilities
ploo <- loo::E_loo(preds, loo1$psis_object, type="mean", log_ratios = -log_lik(burn))$value
ploo_01 <- as.integer(ploo >= 0.5)

# LOO classification accuracy
dat90_20_z %>% 
    dplyr::select(b_burn01) %>%
    bind_cols(ploo_01) %>%
    rename(predicted = `...2`) %>%
    mutate(correct = if_else(b_burn01 == predicted, 1, 0)) %>%
    summarize(mean_correct = mean(correct))

qplot(pred, ploo) #only small differences in the posterior predictive probabilities and the LOO probabilities

# dat90_20_z %>% 
#     bind_cols(pred) %>%
#     rename(predicted = `...8`) %>%
#     ggplot(aes(x = trees100_9020, y = predicted)) +
#     geom_smooth() + #not right because don't want to use this smoother, but will refine in another script
#     geom_jitter(aes(x = trees100_9020, y = b_burn01), height = 0.02, width = 0, alpha = 0.2)

# calibrate predictions
y = as_factor(dat90_20_z$b_burn01)
calPlotData<-calibration(y ~ pred + loopred, 
                         data = data.frame(pred=pred,loopred=ploo,y=y), 
                         cuts=10, class="1")
ggplot(calPlotData, auto.key = list(columns = 2))+
    scale_colour_brewer(palette = "Set1") #well calibrated on averaged but a lot of error at the lowest and highest predicted probabilities

library(splines)
library(MASS)
ggplot(data = data.frame(pred=pred,loopred=ploo,y=as.numeric(y)-1), aes(x=loopred, y=y)) +
    stat_smooth(method='glm', formula = y ~ ns(x, 5), fullrange=TRUE) +
    geom_abline(linetype = 'dashed') +
    labs(x = "Predicted (LOO)", y = "Observed") +
    geom_jitter(height=0.02, width=0, alpha=0.3) +
    scale_y_continuous(breaks=seq(0,1,by=0.1)) +
    xlim(c(0,1))

library(reliabilitydiag)
rd=reliabilitydiag(EMOS = ploo, y = as.numeric(y)-1)
autoplot(rd)+
    labs(x="Predicted (LOO)",
         y="Conditional event probabilities")+
    bayesplot::theme_default(base_family = "sans")

#####
# rmse and mae
kcv <- kfold(burn)
# predicts <- rstantools::loo_linpred(burn, kcv) #doesn't work here down
# rmse(data$y, colMeans(predicts))
# mae(data$y, apply(predicts, 2, median))

#####
# prior checks - https://www.rensvandeschoot.com/brms-wambs/
# step 2 - check trace plots for convergence - yes converged
# mcmc_plot(burn, type = "trace") #same as plot(burn above but without the posterior dist)

modelposterior <- as.mcmc(burn)
summary(burn) #check that Rhats are all close to 1
coda::gelman.plot(modelposterior[, 1:5]) #shrinks to 1, used enough iterations
coda::geweke.diag(modelposterior[, 1:5])
coda::geweke.plot(modelposterior[, 1:5]) #Geweke diagnostic not > 1.96

# step 3 - does convergence remain after doubling the number of iterations? yes, relative bias is less than 5%
burn_iter2 <- brm(b_burn01 ~
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

modelposterior_iter2<- as.mcmc(burn_iter2)
coda::gelman.diag(modelposterior_iter2[, 1:5])
coda::gelman.plot(modelposterior_iter2[, 1:5])

round(100*((summary(burn_iter2)$fixed - summary(burn)$fixed) / summary(burn)$fixed), 3)[,"Estimate"] #relative bias is <5%

# step 4 - does the posterior distribution histogram have enough information? - yes, peaked with smooth slopes
mcmc_plot(burn, type = "hist")
mcmc_plot(burn_iter2, type = "hist")

# step 5 - do the chains exhibit a strong degree of autocorrelation? trees100 does, but it dissipates
coda::autocorr.diag(modelposterior[,1:5], lags = c(0, 1,2,3,4, 5, 10, 50)) # same as mcmc_acf above 
#"These results show that autocorrelation is quite stong after a few lags. 
#This means it is important to make sure we ran the analysis with a lot of samples,
#because with a high autocorrelation it will take longer until the whole parameter space has been identified."

# step 6 - do the posterior distributions make substantive sense?
#yes see 05_plots for posterior distributions

# step 7 - do different specifications of the multivariate variance priors influence the results?
#already specified sd

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

burn_uninf <- brm(b_burn01 ~
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

summary(burn)
summary(burn_uninf)

round(100*((summary(burn_uninf)$fixed - summary(burn)$fixed) / summary(burn)$fixed), 3)[,"Estimate"] #gigantic effects

