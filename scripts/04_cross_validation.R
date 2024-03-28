library(tidyverse)

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

remo_glm1_z <- glm(b_remo ~ trees100_9020*trees1_9020 + group_involve2,
                   data = dat90_20_z, 
                   family = "binomial")

# spatial partitioning
## reading in spatial data
range_pts <- sf::read_sf("data/original/RangelandSurvey.gdb", layer = "SurveySampleFrame") %>% 
    dplyr::select(X:Ymax, uniqueID, Shape) %>%
    sf::st_transform(4326)

set.seed(999)
dat_folds <-
    dat90_20_nas %>% 
    inner_join(., range_pts %>% 
                   sf::st_drop_geometry() %>% 
                   dplyr::select(uniqueID, X, Y)) %>% 
    dplyr::mutate(fold = factor(kmeans(cbind(.$X, .$Y), 10)$cluster,
                                labels = 1:10))


## Get model formula of your model of interest
model_formula <- formula(remo_glm1_z) #change to remo_glm1_z or burn_glm1_z to generate figure

## Make a list, where each element is a separate test fold
test_list <- list()
for (i in levels(dat_folds$fold)) {
    test_list[[i]] <- filter(dat_folds, fold == i) 
}

## Do the same thing, but for training folds
train_list <- list()
for (i in levels(dat_folds$fold)) {
    train_list[[i]] <- filter(dat_folds, fold != i) 
}

## Then put the lists into a dataframe, so the first column is the fold number, and the next to columns are list-columns
folds <- tibble(fold_group = levels(dat_folds$fold),
                train_data = train_list,
                test_data = test_list)

## Then you can apply your model to each of the 10 training folds
#folds_models <- folds %>%
#  mutate(model = map(train_data, ~glmer(model_formula, data=., family="binomial"))) 

folds_models <- folds %>%
    mutate(model = map(train_data, ~glm(model_formula, data=., family="binomial"))) # use this code instead for glm

## And then make predictions from each of those 10 models using the corresponding test data as new data
test_predict <-
    folds_models %>% 
    mutate(predict = map2(model, test_data, 
                          ~predict(.x, newdata=.y, allow.new.levels=T, re.form=NA, type = "response")))

## Then you can pull out the stuff you want (folds, test data, and predictions) and unnest it (no more list-columns)
test_predict_df <-   
    test_predict %>% 
    dplyr::select(fold_group, test_data, predict) %>% 
    unnest(c(test_data, predict))

## Classify each prediction that's above 0.5 as a 1, and all others as a 0
## Classify whether or not the predictions are correct
## Then get the proportion that are correct (same as a mean, since they're all binary) for each fold
test_predict_df %>% 
    mutate(pred_01 = if_else(predict > 0.5, 1, 0),
           correct = if_else(b_burn == pred_01, 1, 0)) %>% 
    group_by(fold) %>% 
    summarize(prop_correct = mean(correct)) %>%
    mutate(mean=mean(prop_correct),
           sd = sd(prop_correct))
#summarize(mean = mean(prop_correct),
#          sd = sd(prop_correct),
#          min = min(prop_correct),
#          max = max(prop_correct))

#test_predict_df$fold<-as.integer(test_predict_df$fold)
pred_acc<-test_predict_df %>% 
    mutate(pred_01 = if_else(predict > 0.5, 1, 0),
           correct = if_else(b_burn == pred_01, 1, 0))%>%
    dplyr::select(uniqueID, correct)

## visualizing cross-validation on the map
set.seed(999)
map<-dat90_20_nas %>% 
    inner_join(., range_pts %>% 
                   sf::st_drop_geometry() %>% 
                   dplyr::select(uniqueID, X, Y)) %>% 
    dplyr::mutate(fold = kmeans(cbind(.$X, .$Y), 10)$cluster)%>%
    sf::st_drop_geometry()
#map<-map %>% mutate(fold=as.factor(fold))
#scv_remo 
scv_remo <-map%>% inner_join(.,pred_acc)%>% #switch out scv_pb and scv_remo to generate images
    ggplot(., aes(X, Y)) +
    geom_point(aes(color = as.factor(fold), size=as.factor(correct)), alpha=0.8) +
    scale_size_manual("correct", values=c(.5,2), labels = c("Incorrect", "Correct"))+
    scale_color_brewer(palette = "Paired")+
    theme_bw(base_size=20)+
    theme(axis.text=element_text(size=15, color="black"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          #legend.position = "bottom",
          #legend.direction = "vertical",#)+
          legend.position="none")+
    labs(x="Longitude", y = "Latitude")#+
    #guides(color="none", size=guide_legend(title="Prediction accuracy"))
scv_remo
scv_pb

cowplot::ggdraw()+
    cowplot::draw_plot(scv_remo, x=0, y=0.6, width=1, height=.4)+
    cowplot::draw_plot(scv_pb, x=0, y=0, width=1, height=0.6)+
    cowplot::draw_plot_label(
        c("A", "B"),
        c(0, 0),
        c(1, 0.6),
        size = 25)

#ggsave("figs/spatial_crossvalidation.png", width = 8, height=8, units="in", dpi=300, bg="white")