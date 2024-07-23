library(caret)
library(glmmTMB)
library(tidyverse)

# read in data and prep for model
dat90_20 <- 
    read_csv("data/processed/ProducerDF_TreeCoverChangeCounty_nrd.csv")

## removing NAs
dat90_20_nas <- 
    na.omit(dat90_20 %>%
                dplyr::select(uniqueID, trees1_9020, trees100_9020, b_remo, b_burn, group_involve2, county2, nrd, eco, no_share)) #leaves 387 observations

# models
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

# Randomly partition data into 10 test folds
set.seed(1234)

dat_folds <-
    dat90_20_nas %>% 
    mutate(fold = factor(sample(rep(1:10, length.out=nrow(.))),
                         labels = 1:10)) 

# Get model formula of your model of interest
model_formula <- formula(remo3) #burn3 or remo3

# Make a list, where each element is a separate test fold
test_list <- list()
for (i in levels(dat_folds$fold)) {
    test_list[[i]] <- filter(dat_folds, fold == i) 
}

# Do the same thing, but for training folds
train_list <- list()
for (i in levels(dat_folds$fold)) {
    train_list[[i]] <- filter(dat_folds, fold != i) 
}

# Then put the lists into a dataframe, so the first column is the fold number,
# and the next to columns are list-columns
folds <- tibble(fold_group = levels(dat_folds$fold),
                train_data = train_list,
                test_data = test_list)

# Then you can apply your model to each of the 10 training folds
folds_models <- folds %>%
    mutate(model = map(train_data, ~brm(model_formula, data=.))) 

# And then make predictions from each of those 10 models using the corresponding test data as new data
test_predict <-
    folds_models %>% 
    mutate(predict = map2(model, test_data, 
                          ~predict(.x, newdata=.y, allow.new.levels=T, re.form=NA, type = "response")))

# Then you can pull out the stuff you want (folds, test data, and predictions) and unnest it (no more list-columns)
test_predict_df <-   
    test_predict %>% 
    dplyr::select(fold_group, test_data, predict) %>% 
    unnest(c(test_data, predict))

# Classify each prediction that's above 0.5 as a 1, and all others as a 0
# Classify whether or not the predictions are correct
# Then get the proportion that are correct (same as a mean, since they're all binary) for each fold
test_predict_df %>% 
    mutate(pred_01 = if_else(predict[,1] > 0.5, 1, 0),
           correct = if_else(b_burn == pred_01, 1, 0)) %>% 
    group_by(fold_group) %>% 
    summarize(prop_correct = mean(correct)) %>% 
    summarize(mean = mean(prop_correct),
              sd = sd(prop_correct),
              min = min(prop_correct),
              max = max(prop_correct)) #75.2 correct for burn3, #41.5 for remo3 HA
