library(tidyverse)

# read in data and prep for model
dat90_20 <- 
    read_csv("data/processed/ProducerDF_TreeCoverChangeCounty_nrd.csv") 

## removing NAs
dat90_20_nas <- 
    na.omit(dat90_20 %>%
                dplyr::select(trees1_9020, trees100_9020, b_burn01, b_remo01, group_involve2, nrd
                              #county2, eco, no_share, X, Y
                )) #leaves 396 observations
dat90_20_nas <-
    dat90_20_nas %>% 
    dplyr::select(-b_remo01)

#write.csv(dat90_20_nas,"data/processed/NesbittEtAl_ScaleMismatch_Zenodo.csv", row.names=F)
