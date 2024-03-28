library(tidyverse)
library(ResourceSelection) #for hosmer-lemeshow test
library(lmtest) #for lrtest
library(broom) #for augment

# read in data and prep for model
dat90_20<-read_csv("data/processed/ProducerDF_TreeCoverChangeCounty.csv")

## removing NAs
dat90_20_nas<-na.omit(dat90_20%>%
                          select(uniqueID,trees1_9020, trees100_9020, group_involve2, b_burn, b_remo))#-c(b_chem4:log_dist_pb_km, trees10_9020:trees50_9020)))

## standardizing
dat90_20_z <-as_tibble(scale(dat90_20_nas%>%dplyr::select(trees1_9020, trees100_9020)))#%>%st_drop_geometry))
dat90_20_z <-dat90_20_nas%>%dplyr::select(-c(trees1_9020, trees100_9020))%>%bind_cols(dat90_20_z)

# models
burn_glm1_z <- glm(b_burn ~ trees100_9020*trees1_9020 + group_involve2,
                   data = dat90_20_z, 
                   family = "binomial")

burn_glm2_z <- glm(b_burn ~ trees100_9020*trees1_9020,
                   data = dat90_20_z, 
                   family = "binomial")
burn_glm3_z <- glm(b_burn ~ trees100_9020+trees1_9020 + group_involve2,
                   data = dat90_20_z, 
                   family = "binomial")
burn_glm4_z <- glm(b_burn ~ trees100_9020 + group_involve2,
                   data = dat90_20_z, 
                   family = "binomial")
burn_glm5_z <- glm(b_burn ~ trees1_9020 + group_involve2,
                   data = dat90_20_z, 
                   family = "binomial")
burn_glm6_z <- glm(b_burn ~ trees1_9020,
                   data = dat90_20_z, 
                   family = "binomial")
burn_glm7_z <- glm(b_burn ~ group_involve2,
                   data = dat90_20_z, 
                   family = "binomial")
burn_glm8_z <- glm(b_burn ~ trees100_9020,
                   data = dat90_20_z, 
                   family = "binomial")
burn_glm9_z <- glm(b_burn ~ 1,
                   data = dat90_20_z, 
                   family = "binomial")

AIC(burn_glm1_z,burn_glm2_z,burn_glm3_z,burn_glm4_z,burn_glm5_z,burn_glm6_z,burn_glm7_z,burn_glm8_z,burn_glm9_z)

summary(burn_glm1_z) 

## mechanical removal
remo_glm1_z <- glm(b_remo ~ trees100_9020*trees1_9020 + group_involve2,
                   data = dat90_20_z, 
                   family = "binomial")

remo_glm2_z <- glm(b_remo ~ trees100_9020*trees1_9020,
                   data = dat90_20_z, 
                   family = "binomial")
remo_glm3_z <- glm(b_remo ~ trees100_9020+trees1_9020 + group_involve2,
                   data = dat90_20_z, 
                   family = "binomial")
remo_glm4_z <- glm(b_remo ~ trees100_9020 + group_involve2,
                   data = dat90_20_z, 
                   family = "binomial")
remo_glm5_z <- glm(b_remo ~ trees1_9020 + group_involve2,
                   data = dat90_20_z, 
                   family = "binomial")
remo_glm6_z <- glm(b_remo ~ trees1_9020,
                   data = dat90_20_z, 
                   family = "binomial")
remo_glm7_z <- glm(b_remo ~ group_involve2,
                   data = dat90_20_z, 
                   family = "binomial")
remo_glm8_z <- glm(b_remo ~ trees100_9020,
                   data = dat90_20_z, 
                   family = "binomial")
remo_glm9_z <- glm(b_remo ~ 1,
                   data = dat90_20_z, 
                   family = "binomial")

AIC(remo_glm1_z,remo_glm2_z,remo_glm3_z,remo_glm4_z,remo_glm5_z,remo_glm6_z,remo_glm7_z,remo_glm8_z,remo_glm9_z)

summary(remo_glm1_z) 

# model significance
lrtest(remo_glm1_z) #sig diff from null model
lrtest(burn_glm1_z) #sig diff from null model

# model gof - Hosmer-Lemeshow GOF test
hoslem.test(dat90_20_z$b_burn, fitted(burn_glm1_z), g=length(burn_glm1_z$coefficients)+1)#chose groups based on g>p+1 --> 11
for (i in 4:15) {
    print(hoslem.test(dat90_20_z$b_burn, fitted(burn_glm1_z), g=i) $p.value)#can also choose a range of groups to see if any of them are significant
} 

hoslem.test(dat90_20_z$b_remo, fitted(remo_glm1_z), g=length(remo_glm1_z$coefficients)+1)#chose groups based on g>p+1 --> 11
for (i in 4:15) {
    print(hoslem.test(dat90_20_z$b_remo, fitted(remo_glm1_z), g=i) $p.value)#can also choose a range of groups to see if any of them are significant
}

# model diagnostics
## linearity
### burn
behav_probabilities<-predict(burn_glm1_z, type="response") #predicts probability of each ind doing behaviour
behav_predictors<-dat90_20_z%>%select(trees100_9020, trees1_9020, group_involve2, uniqueID)#%>%st_drop_geometry() #subsetting data for only predictors
predictors<-colnames(behav_predictors)
behav_predictors <- behav_predictors %>%
    mutate(logit = log(behav_probabilities/(1-behav_probabilities))) %>%
    gather(key = "predictors", value = "predictor.value", -c(logit, uniqueID))
ggplot(behav_predictors, aes(logit, predictor.value))+
    geom_point(size = 0.5, alpha = 0.5) +
    geom_smooth(method = "loess") + 
    theme_bw() + 
    facet_wrap(~predictors, scales = "free_y") # probably two outliers I need to remove
behav_predictors%>%filter(logit < -2 & predictor.value>5)#uniqueID=2021_0921, 2021_4153
behav_predictors%>%filter(logit < -4 & predictor.value>2 & predictors=="trees100_9020")#uniqueID=2021_0921, 2021_4153
dat90_20_nas%>%filter(uniqueID=="2021_0921"|uniqueID=="2021_4153")

### remo
behav_probabilities<-predict(remo_glm1_z, type="response") #predicts probability of each ind doing behaviour
behav_predictors<-dat90_20_z%>%select(trees100_9020, trees1_9020, group_involve2, uniqueID)#%>%st_drop_geometry() #subsetting data for only predictors
predictors<-colnames(behav_predictors)
behav_predictors <- behav_predictors %>%
    mutate(logit = log(behav_probabilities/(1-behav_probabilities))) %>%
    gather(key = "predictors", value = "predictor.value", -c(logit, uniqueID))
ggplot(behav_predictors, aes(logit, predictor.value))+
    geom_point(size = 0.5, alpha = 0.5) +
    geom_smooth(method = "loess") + 
    theme_bw() + 
    facet_wrap(~predictors, scales = "free_y") # trees1_9020 isn't good

## influential values
### burn
plot(burn_glm1_z, which=4, id.n=3)
model.data <- augment(burn_glm1_z) %>% 
    mutate(index = 1:n()) 
ggplot(model.data, aes(index, .resid)) + 
    geom_point(aes(color = b_burn), alpha = .5) +
    theme_bw() #Data points with an absolute standardized residuals above 3 represent possible outliers and may deserve closer attention.
model.data %>% 
    filter(abs(.resid) > 3) #no influential values

### remo
plot(remo_glm1_z, which=4, id.n=3)
model.data <- augment(remo_glm1_z) %>% 
    mutate(index = 1:n()) 
ggplot(model.data, aes(index, .resid)) + 
    geom_point(aes(color = b_remo), alpha = .5) +
    theme_bw() #Data points with an absolute standardized residuals above 3 represent possible outliers and may deserve closer attention.
model.data %>% 
    filter(abs(.resid) > 3) #no influential values

## cook's d
### burn
cooks<-cooks.distance(burn_glm1_z) 
cooks[which(cooks>(4/383))]
model.data <- augment(burn_glm1_z) %>% 
    mutate(index = 1:n()) 
print(model.data %>% filter(.cooksd> 0.01044386),
      #top_n(13, .hat), 
      width=Inf)
model.data %>% top_n(10, .cooksd) %>%
    dplyr::select(b_burn:.cooksd,index)# 3-4 values that probably should be removed: 34, 259, 328
dat90_20_z%>%filter(county2=="YORK" & b_burn==0 & trees1_9020>4.5) #uniqueID = 2021_0594
dat90_20_z%>%filter(county2=="NEMAHA" & b_burn==0 & trees1_9020>2.5) #uniqueID = 2021_4155
dat90_20_z%>%filter(county2=="CUSTER" & b_burn==1 & trees1_9020>2.5) #uniqueID = 2021_5511
dat90_20_nas%>%filter(uniqueID=="2021_0594"|uniqueID=="2021_4155"|uniqueID=="2021_5511")

## check this website when I go to publish to redo diagnostics:
#https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#model-diagnostics

### remo
cooks<-cooks.distance(remo_glm1_z) 
cooks[which(cooks>(4/383))]
model.data <- augment(remo_glm1_z) %>% 
    mutate(index = 1:n()) 
print(model.data %>% filter(.cooksd> 0.01044386),
      #top_n(13, .hat), 
      width=Inf)
model.data %>% top_n(10, .cooksd) %>%
    dplyr::select(b_remo:.cooksd,index)# so many that have high cooksd, 34, 237, 233 are the worst

## remove influential points to compare Betas
### burn
fit_outliers<-update(burn_glm1_z, subset=c(-233))
car::compareCoefs(burn_glm1_z, fit_outliers)#changes to the trees1 coefficient mostly. Don't see a good reason to remove

### remo
fit_outliers<-update(remo_glm1_z, subset=c(-233))
car::compareCoefs(remo_glm1_z, fit_outliers)#changes to the trees1 and group coefficients mostly. Don't see a good reason to remove

## multicollinearity
car::vif(burn_glm1_z)
car::vif(remo_glm1_z)
