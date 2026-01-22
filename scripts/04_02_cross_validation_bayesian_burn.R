#run 03_bayesian_burn_model.R first
library(caret)
library(glmmTMB)
library(verification) #for roc.plot()
library(pROC) #for roc()

# Randomly partition data into 10 test folds
set.seed(1234)

dat_folds <-
    dat90_20_nas %>% 
    mutate(fold = factor(sample(rep(1:10, length.out=nrow(.))),
                         labels = 1:10)) 

# Get model formula of your model of interest
model_formula <- formula(burn)

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
    mutate(predict = purrr::map2(model, test_data, 
                          ~predict(.x, newdata=.y, allow.new.levels=T, re.form=NA, type = "response")))

# Then you can pull out the stuff you want (folds, test data, and predictions) and unnest it (no more list-columns)
test_predict_df <-   
    test_predict %>% 
    dplyr::select(fold_group, test_data, predict) %>% 
    unnest(c(test_data, predict))

# determine appropriate classification threshold
# ROC curve #https://www.digitalocean.com/community/tutorials/plot-roc-curve-r-programming
roc.plot(test_predict_df$b_burn01, test_predict_df$predict[,1])


plot(roc(test_predict_df$b_burn01, test_predict_df$predict[,1]), print.auc = T)
# true positive rate is the proportion of observations that were correctly predicted to be positive out of all positive observations (true positive / (true positive + false positive))
# false positive rate is the proportion of observations that are incorrectly predicted to be positive out of all negative observations (FP / (TN + FP))

# y axis is the true positive rate (sensitivity)
# x axis is the false positive rate (1 - specificity)

# "Classifiers that give curves closer to the top-left corner indicate a better performance. 
# As a baseline, a random classifier is expected to give points lying along the diagonal (FPR = TPR).
# The closer the curve comes to the 45-degree diagonal of the ROC space, the less accurate the test."

#https://developers.google.com/machine-learning/crash-course/classification/roc-and-auc 

# Classify each prediction that's above a threshold as a 1, and all others as a 0
threshold <- 0.5

# Classify whether or not the predictions are correct
# Then get the proportion that are correct (same as a mean, since they're all binary) for each fold
test_predict_df %>% 
    mutate(pred_01 = if_else(predict[,1] > threshold, 1, 0), # play with threshold
           correct = if_else(b_burn01 == pred_01, 1, 0)) %>% 
    group_by(fold_group) %>% 
    summarize(prop_correct = mean(correct)) %>% 
    summarize(mean = mean(prop_correct),
              sd = sd(prop_correct),
              min = min(prop_correct),
              max = max(prop_correct))

# is the model better at predicting 0s or 1s? not really, but depends on threshold
test_predict_df %>% 
    mutate(pred_01 = if_else(predict[,1] > threshold, 1, 0),
           correct = if_else(b_burn01 == pred_01, 1, 0)) %>% 
    group_by(fold_group) %>%
    ggplot(aes(x = predict[,1], y = correct)) +
    geom_point(alpha = 0.7, aes(col = as.factor(b_burn01))) +
    annotate("text",label = "true negative\ncorrect, no burn", x = 0.2, y = 0.8) +
    annotate("text", label = "true positive\ncorrect, burn", x = 0.7, y = 0.8) +
    annotate("text", label = "false negative\nincorrect\npredicted no burn, but burned", x = 0.2, y = 0.2) +
    annotate("text", label = "false positive\nincorrect\npredicted burn, but didn't", x = 0.7, y = 0.2) 

# test_predict_df %>% 
#     mutate(pred_01 = if_else(predict[,1] > threshold, 1, 0),
#            correct = if_else(b_burn01 == pred_01, 1, 0)) %>% 
#     group_by(b_burn01, correct) %>%
#     summarize(n = n()) %>%
#     mutate(prop_overall = n/383) %>%
#     group_by(b_burn01) %>%
#     mutate(total_behavior = sum(n)) %>%
#     mutate(prop_behavior = n/total_behavior) %>%
#     group_by(correct) %>%
#     mutate(total_rightwrong = sum(n)) %>%
#     mutate(prop_rightwrong = n/total_rightwrong)

rates <-
    test_predict_df %>% 
    mutate(pred_01 = if_else(predict[,1] > threshold, 1, 0)) %>% 
    group_by(b_burn01, pred_01) %>%
    summarize(n = n()) %>%
    mutate(false_true = case_when(b_burn01 == 0 & pred_01 == 0 ~ "true_neg",
                                  b_burn01 == 1 & pred_01 == 1 ~ "true_pos",
                                  b_burn01 == 0 & pred_01 == 1 ~ "false_pos",
                                  b_burn01 == 1 & pred_01 == 0 ~ "false_neg"))

# true positive rate - (true positive / (true positive + false positive))
#96/(96+50)
rates[4,3]/(rates[4,3]+rates[2,3])

# true negative rate - (true negative / (true negative + false negative))
#166/(166+84)
rates[1,3]/(rates[1,3]+rates[3,3])

# false positive rate (FP / (TN + FP))
#50/(50+166)
rates[2,3]/(rates[2,3]+rates[1,3])

# false negative rate (FN / FN + TP)
#84/(84+96)
rates[3,3]/(rates[3,3]+rates[4,3])

# the question is do you care more about missing burners (false negatives) or wasting resources on non-burners (false positives)?
# i think you'd care more about missing burners, so you'd want to minimize false negatives

# plot a sensitivity analysis for false negative rate and threshold for classification?
threshold <-seq(0,1,0.05)
fn<-c(NA,NA, 0.006, 0.04, 0.06,0.06,0.1,0.14, 0.27, 0.37,0.42,0.47,0.55,0.63,0.71,0.82,0.88,0.94, NA,NA,NA)
fp<-c(NA,NA, 0.97, 0.80, 0.67,0.65,0.63, 0.58, 0.43, 0.32, 0.26,0.23,0.17,0.13,0.08,0.05,0.01,0.009,NA,NA, NA)
acc_avg<-c(0.46, 0.46, 0.47, 0.55, 0.61,0.61,0.61,0.62, 0.64, 0.65, 0.67,0.66, 0.65, 0.65, 0.64, 0.59,0.59, 0.56, 0.55, 0.55, 0.55)

sensitivity_df <- tibble(threshold, fn, fp, acc_avg)

sensitivity_df %>%
    pivot_longer(!threshold, 
                 names_to = "stat",
                 values_to = "rate") %>%
    ggplot(aes(x = threshold, y = rate, group = stat)) +
    geom_line(aes(col = stat))

#average model accuracy peaks at a threshold of 0.5
#fn=fp=0.36 at about 0.44 threshold
