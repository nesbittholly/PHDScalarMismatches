library(tidyverse)

# read in data and prep 
## reading in survey data
dat<-read_csv("data/processed/ProducerDF_TreeCoverChangeCounty_NAsRemoved.csv")

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

# plot
mean_tc<-datv%>%
    group_by(Year, Buffer)%>%
    summarise(Tree_Perc_Mean=mean(Tree_Perc_Mean, na.rm=T))
facetlabs<-c("Local-level\n(1 km radius)", "Regional-level\n(100 km radius)")
names(facetlabs) <- c("1 km", "100 km")

p<-ggplot(datv, aes(x=Year, y=Tree_Perc_Mean*100))+
    geom_line(aes(group=uniqueID, color = "Individual"), alpha=0.25, )+
    geom_line(data=mean_tc, aes(x=Year, y=Tree_Perc_Mean*100, group=Buffer, color="Sample mean",), linewidth=2)+
    scale_color_manual(values = c("darkolivegreen4", "black"), name ="")+
    facet_wrap(.~Buffer, scales="free", labeller = labeller(Buffer=facetlabs))+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(lim=c(0,50), expand = c(0,0))+
    labs(x = "Year", y = "Mean percent tree cover (%)")+
    theme_classic(base_size=20)+
    theme(axis.text=element_text(size=15, color="black"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.text=element_text(size=15),
          strip.text.y.right = element_text(angle = 0, hjust=0),
          strip.background = element_blank(),
          panel.spacing = unit(2, "lines"),
          plot.margin = unit(c(1,1,1,1), "cm"),
          legend.position = c(0.9,0.85))
#legend.position = "bottom")#+
#guides(color=guide_legend(nrow=2))

cowplot::ggdraw()+
    cowplot::draw_plot(p)+
    cowplot::draw_plot_label(
        c("A", "B"),
        c(0, 0.5),
        c(0.9, 0.9),
        size = 18)
#ggsave("figs/time_series.png", width = 12, height=6, units="in", dpi=300, bg="white")