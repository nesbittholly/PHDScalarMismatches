library(tidyverse)

# read in data and prep 
dat<-read_csv("data/processed/ProducerDF_TreeCoverChangeCounty_NRD.csv") %>%
    dplyr::select(uniqueID, trees1_9020, trees100_9020, b_burn01, b_remo01, group_involve2) %>%
    na.omit()

dat

# quick boxplot check
boxplot(dat$trees1_9020, dat$trees100_9020)

dat %>% filter(trees1_9020>0)

dat %>%
    mutate(local_increase = case_when(trees1_9020 > 0 ~ 1,
                                      trees1_9020 <= 0 ~ 0)) %>%
    dplyr::select(local_increase) %>%
    group_by(local_increase) %>%
    summarize(n = n()) %>%
    mutate(percent = n/396*100) #86% saw an increase

dat %>%
    mutate(local_increase = case_when(trees1_9020 > 0 ~ 1,
                                      trees1_9020 <= 0 ~ 0)) %>%
    dplyr::select(local_increase, b_burn01) %>%
    pivot_longer(!b_burn01, names_to = "variable", values_to = "value") %>%
    group_by(b_burn01, value) %>%
    summarize(n = n()) %>%
    mutate(percent = n/396*100)

# of those that burned (n = 23+157 = 180), 157 saw an increase in local-level encroachment --> 87%    

burn <- 
    dat %>% 
    filter(b_burn01 == 1)

summary(burn)

no_burn <-
    dat %>%
    filter(b_burn01 == 0)

summary(no_burn)

boxplot(burn$trees1_9020, no_burn$trees1_9020)
t.test(burn$trees1_9020, no_burn$trees1_9020) # no statistical difference between burners and non-burners and their level of encroachment
# non-burners might not be burning because they don't have encroachment (though we see that this is not true - they do have encroachment) 

lm1 <- glm(b_burn01 ~ trees1_9020, data = dat)
summary(lm1)

# compare to 1990-2017
## reading in vegetation cover data for each person in sample
datv_1km<-read_csv("data/original/R3_NLCD2019_NE_EPSCoR_Tree_Percent_Mean_Buff_1km.csv")
datv_10km<-read_csv("data/original/R3_NLCD2019_NE_EPSCoR_Tree_Percent_Mean_Buff_10km.csv")
datv_50km<-read_csv("data/original/R3_NLCD2019_NE_EPSCoR_Tree_Percent_Mean_Buff_50km.csv")
datv_100kma<-read_csv("data/original/R3_NLCD2019_NE_EPSCoR_Tree_Percent_Mean_Buff_100km_a.csv")
datv_100kmb<-read_csv("data/original/R3_NLCD2019_NE_EPSCoR_Tree_Percent_Mean_Buff_100km_b.csv")
datv_100kmc<-read_csv("data/original/R3_NLCD2019_NE_EPSCoR_Tree_Percent_Mean_Buff_100km_c.csv")

## merging veg dataframes and filtering years, buffers, respondents
datv<-bind_rows(datv_1km, datv_10km, datv_50km, datv_100kma, datv_100kmb, datv_100kmc)%>%
    filter(uniqueID %in% dat$uniqueID)%>%
    mutate(Buffer=recode_factor(Buffer, 
                                "1.0 km" = "1 km",
                                "10.0 km" = "10 km",
                                "50.0 km" = "50 km",
                                "100.0 km" = "100 km"))%>%
    filter(Year>1989, Year<2021,
           Buffer == "1 km" | Buffer =="100 km")

datv2 <-
    datv %>%
    filter(Year %in% c(1990, 2017))

dat_2017 <-
    datv2 %>%
    pivot_wider(names_from = Year, values_from = Tree_Perc_Mean) %>%
    filter(Buffer == "1 km") %>%
    mutate(trees1_9017 = `2017`-`1990`)

dat2 <-
    dat %>%
    left_join(dat_2017 %>% dplyr::select(uniqueID, trees1_9017), by = "uniqueID")

cor(dat2$trees1_9020, dat2$trees1_9017)
plot(dat2$trees1_9020, dat2$trees1_9017)

dat2 %>%
    ggplot() +
    geom_point(aes(x = trees1_9017, y = trees1_9020, col = as_factor(b_burn01)), alpha = 0.6) +
    labs(x = "Change in mean % tree cover 1990-2017 (1 km)",
         y = "Change in mean % tree cover 1990-2020 (1 km)",
         col = "Prescribed burning") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed")

dat2 %>%
    mutate(local_increase = case_when(trees1_9017 > 0 ~ 1,
                                      trees1_9017 <= 0 ~ 0)) %>%
    dplyr::select(local_increase) %>%
    group_by(local_increase) %>%
    summarize(n = n()) %>%
    mutate(percent = n/396*100) #89% saw an increase

dat2 %>%
    mutate(local_increase = case_when(trees1_9017 > 0 ~ 1,
                                      trees1_9017 <= 0 ~ 0)) %>%
    dplyr::select(local_increase, b_burn01) %>%
    pivot_longer(!b_burn01, names_to = "variable", values_to = "value") %>%
    group_by(b_burn01, value) %>%
    summarize(n = n()) %>%
    mutate(percent = n/396*100)

# of those that burned (n = 20+160 = 180), 160 saw an increase in local-level encroachment --> 89%
