library(tidyverse)

# data prep
## reading in survey data
dat <- read_csv("data/original/HollysDF.csv")
dat_county <- read_csv("data/original/NebraskaProducerSurvey_AllPrompts_General_clean.csv") %>%
    dplyr::select(uniqueID, q4) %>%
    rename("county" = "q4")
dat <- dat %>% 
    left_join(dat_county, by = "uniqueID")

# ## reading in prescribed burn distances
# pb_km<-read_csv("data/original/PresBurnAssoc_distance.csv")%>%
#     select(uniqueID, dist_poly)%>%
#     rename("dist_pb_km" = "dist_poly")
# dat<-dat%>%left_join(pb_km, by = "uniqueID")
# dat<-dat%>%
#     mutate(log_dist_pb_km = log(dist_pb_km+0.01))

## reading in vegetation cover data for each person in sample
datv_1km<-read_csv("data/original/R3_NLCD2019_NE_EPSCoR_Tree_Percent_Mean_Buff_1km.csv")
datv_10km<-read_csv("data/original/R3_NLCD2019_NE_EPSCoR_Tree_Percent_Mean_Buff_10km.csv")
datv_50km<-read_csv("data/original/R3_NLCD2019_NE_EPSCoR_Tree_Percent_Mean_Buff_50km.csv")
datv_100kma<-read_csv("data/original/R3_NLCD2019_NE_EPSCoR_Tree_Percent_Mean_Buff_100km_a.csv")
datv_100kmb<-read_csv("data/original/R3_NLCD2019_NE_EPSCoR_Tree_Percent_Mean_Buff_100km_b.csv")
datv_100kmc<-read_csv("data/original/R3_NLCD2019_NE_EPSCoR_Tree_Percent_Mean_Buff_100km_c.csv")

## merging veg dataframes
datv<-bind_rows(datv_1km, datv_10km, datv_50km, datv_100kma, datv_100kmb, datv_100kmc)
datv2<-datv%>%
    filter(uniqueID %in% dat$uniqueID)%>%
    mutate(Buffer=recode_factor(Buffer, 
                                "1.0 km" = "1 km",
                                "10.0 km" = "10 km",
                                "50.0 km" = "50 km",
                                "100.0 km" = "100 km"))

## reformatting veg dataframe
datv_wide<-datv%>%pivot_wider(names_from=Buffer, values_from=Tree_Perc_Mean)%>%
    rename("year" = "Year",
           "trees1km" = `1.0 km`,
           "trees10km" = `10.0 km`,
           "trees50km" = `50.0 km`,
           "trees100km" = `100.0 km`)

## combining survey and veg cover dataframes
datgv<-left_join(dat, datv_wide, by="uniqueID")

## creating 1990 to 2020 change dataframe
dat1990_2020<-datgv%>%
    dplyr::filter(year%in%c(1990,2020))%>%
    dplyr::select(uniqueID, q7a, q7b, q7d, sc_change_obs_veg, efficacy, risks, edu, sc_norms, sc_trust, org_group_member, freq_avg, info_avg, size, occup_Iqv, homophily,density,
           county, #dist_pb_km,log_dist_pb_km,
           year, trees1km:trees100km)%>%
    rename("b_chem4" = "q7a",
           "b_burn4" = "q7b",
           "b_remo4" = "q7d",
           "change_obs" = "sc_change_obs_veg",
           "trust" = "sc_trust",
           "norms" = "sc_norms",
           "group_involve" = "org_group_member")

dat1990_2020_wide<-dat1990_2020%>%
    pivot_wider(names_from = year,
                values_from = c(trees1km, trees10km, trees50km, trees100km))
summary(dat1990_2020_wide%>%dplyr::select(uniqueID, trees1km_1990, trees1km_2020, trees100km_1990, trees100km_2020))

dat90_20<-dat1990_2020_wide%>%
    mutate(trees1_9020 = trees1km_2020-trees1km_1990,
           trees10_9020 = trees10km_2020-trees10km_1990,
           trees50_9020 = trees50km_2020-trees50km_1990,
           trees100_9020 = trees100km_2020-trees100km_1990)%>%
    dplyr::select(-c(#uniqueID, 
        trees1km_1990:trees100km_2020, efficacy, risks, edu))

## making binary response variable
dat90_20<-dat90_20 %>%
    mutate(b_remo = case_when(b_remo4 <= 2 ~ 0,
                              b_remo4 >= 3 ~ 1),
           b_burn = case_when(b_burn4 <= 2 ~ 0,
                              b_burn4 >= 3 ~ 1))
print(dat90_20%>%dplyr::select(b_remo4, b_remo, b_burn4, b_burn))

dat90_20 <- dat90_20 %>%
    mutate(b_burn01 = case_when(b_burn4 == 1 ~ 0,
                                b_burn4 > 1 ~ 1),
           b_remo01 = case_when(b_remo4 == 1 ~ 0,
                                b_remo4 > 1 ~ 1))

print(dat90_20%>%dplyr::select(b_burn4, b_burn01, b_remo4, b_remo01), n = 20)

## making binary group involvement variable
dat90_20<-dat90_20%>%
    mutate(group_involve2 = case_when(group_involve <= 1 ~ 0,
                                      group_involve >= 2 ~ 1))
print(dat90_20%>%dplyr::select(group_involve, group_involve2), n=60)

## making new county variable
dat90_20<-dat90_20%>%
    mutate(county2 = case_when(county == "CHERRY/THOMAS" ~ "THOMAS",
                               county == "CUSTER/GOSPER" ~ "CUSTER",
                               county == "GAGE/LANCASTER" ~ "LANCASTER",
                               county == "NANCE/BOONE" ~ "NANCE",
                               county == "BOX BUTTE/SIOUX" ~ "SIOUX",
                               county == "GRANT/CUSTER" ~ "GRANT",
                               county == "GAGE/SALINE" ~ "SALINE",
                               TRUE ~ as.character(county)))
dat90_20 %>% 
    dplyr::select(county, county2) %>%
    print(n = Inf)
table(dat90_20$county2)

#write.csv(dat90_20,"data/processed/ProducerDF_TreeCoverChangeCounty.csv", row.names=F)

## creating NRD and ecoregion variables
range_pts <- sf::read_sf("data/original/RangelandSurvey.gdb", layer = "SurveySampleFrame") %>%
    dplyr::select(X:Ymax, uniqueID, Shape) %>%
    sf::st_transform(4326)
ecos <- sf::read_sf("data/original/NE_ecoregions/ne_eco_l4.shp") %>%
    sf::st_transform(4326)
nrds <- sf::read_sf("data/original/nrd_boundaries/BND_NaturalResourceDistricts_DNR.shp") %>%
    sf::st_transform(4326)

range_pts_nrd_eco <-
    range_pts %>%
    sf::st_join(., nrds %>%
                dplyr::select(nrd = NRD_Name)) %>%
    sf::st_join(., ecos %>%
                dplyr::select(eco = US_L4NAME))

nrd_eco<-range_pts_nrd_eco%>%dplyr::select(uniqueID, nrd, eco, X, Y) #%>% st_drop_geometry()
dat90_20_nrd<-inner_join(nrd_eco, dat90_20, by="uniqueID")

## creating NRD variable for those that have ended juniper cost-share
dat90_20_nrd<-dat90_20_nrd %>%
    mutate(no_share = case_when(nrd == "Twin Platte" ~ 0,
                                nrd == "Upper Loup" ~ 0,
                                TRUE ~ 1)) %>% 
    sf::st_drop_geometry()

dat90_20_nrd %>% 
    dplyr::select(nrd, no_share) %>%
    print(n = Inf)

write.csv(dat90_20_nrd,"data/processed/ProducerDF_TreeCoverChangeCounty_NRD.csv", row.names=F)


## removing NAs
# dat90_20_nas<-na.omit(dat90_20%>%
#                           select(uniqueID,trees1_9020, trees100_9020, group_involve2, b_burn, b_remo))

#write.csv(dat90_20_nas,"data/processed/ProducerDF_TreeCoverChangeCounty_NAsRemoved.csv", row.names=F)