library(caret)
library(glmmTMB)
library(tidyverse)
library(brms)

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
model_formula <- formula(burn3) #burn3 or remo3

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
    mutate(model = purrr::map(train_data, ~brm(model_formula, data=.))) 

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
    mutate(pred_01 = if_else(predict[,1] > 0.5, 1, 0), # play with threshold
           correct = if_else(b_burn == pred_01, 1, 0)) %>% 
    group_by(fold_group) %>% 
    summarize(prop_correct = mean(correct)) %>% 
    summarize(mean = mean(prop_correct),
              sd = sd(prop_correct),
              min = min(prop_correct),
              max = max(prop_correct)) #75.2 correct for burn3, #41.5 for remo3 HA

# is the model better at predicting 0s or 1s? --> 0s, by a lot, though the complicated models do slightly better
test_predict_df %>% 
    mutate(pred_01 = if_else(predict[,1] > 0.3, 1, 0),
           correct = if_else(b_burn == pred_01, 1, 0)) %>% 
    group_by(fold_group) %>%
    ggplot(aes(x = predict[,1], y = correct)) +
    geom_point(alpha = 0.7, aes(col = as.factor(b_burn))) +
    annotate("text",label = "true negative\ncorrect, no burn", x = 0.2, y = 0.8) +
    annotate("text", label = "true positive\ncorrect, burn", x = 0.7, y = 0.8) +
    annotate("text", label = "false negative\nincorrect\npredicted no burn, but burned", x = 0.2, y = 0.2) +
    annotate("text", label = "false positive\nincorrect\npredicted burn, but didn't", x = 0.7, y = 0.2) #+
    #annotate("text", label = "gets 95% of nonburners correct", x = 0.2, y = 0.7) +
    #annotate("text", label = "but only 10% of burners correct", x = 0.7, y = 0.7) +
    #annotate("text", label = "of the incorrect predictions, 86% are burners", x = 0.5, y = 0.1)
    
test_predict_df %>% 
    mutate(pred_01 = if_else(predict[,1] > 0.5, 1, 0),
           correct = if_else(b_burn == pred_01, 1, 0)) %>% 
    group_by(b_burn, correct) %>%
    summarize(n = n()) %>%
    mutate(prop_overall = n/383) %>%
    group_by(b_burn) %>%
    mutate(total_behavior = sum(n)) %>%
    mutate(prop_behavior = n/total_behavior) %>%
    group_by(correct) %>%
    mutate(total_rightwrong = sum(n)) %>%
    mutate(prop_rightwrong = n/total_rightwrong)

test_predict_df %>% 
    mutate(pred_01 = if_else(predict[,1] > 0.3, 1, 0)) %>% 
    group_by(b_burn, pred_01) %>%
    summarize(n = n()) %>%
    mutate(false_true = case_when(b_burn == 0 & pred_01 == 0 ~ "true_neg",
                                  b_burn == 1 & pred_01 == 1 ~ "true_pos",
                                  b_burn == 0 & pred_01 == 1 ~ "false_pos",
                                  b_burn == 1 & pred_01 == 0 ~ "false_neg"))
    

# ROC curve #https://www.digitalocean.com/community/tutorials/plot-roc-curve-r-programming
library(verification)
roc.plot(test_predict_df$b_burn, test_predict_df$predict[,1])

library(pROC)
plot(roc(test_predict_df$b_burn, test_predict_df$predict[,1]), print.auc = T)
# true positive rate is the proportion of observations that were correctly predicted to be positive out of all positive observations (true positive / (true positive + false positive))
# false positive rate is the proportion of observations that are incorrectly predicted to be positive out of all negative observations (FP / (TN + FP))

# y axis is the true positive rate (sensitivity)
# x axis is the false positive rate (1 - specificity)

# "Classifiers that give curves closer to the top-left corner indicate a better performance. 
# As a baseline, a random classifier is expected to give points lying along the diagonal (FPR = TPR).
# The closer the curve comes to the 45-degree diagonal of the ROC space, the less accurate the test."

#https://developers.google.com/machine-learning/crash-course/classification/roc-and-auc 

# root mean square error
library(Metrics)
Metrics::rmse(test_predict_df$b_burn, test_predict_df$predict[,1]) # compare different models
