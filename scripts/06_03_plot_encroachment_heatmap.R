# read in data and prep 
## reading in survey data
# dat<-read_csv("data/processed/ProducerDF_TreeCoverChangeCounty_NAsRemoved.csv")
dat<-read_csv("data/processed/ProducerDF_TreeCoverChangeCounty_NRD.csv")

dat <- 
    na.omit(dat %>%
                dplyr::select(uniqueID, trees1_9020, trees100_9020, b_burn01, b_remo01, group_involve2#, nrd
                              #county2, eco, no_share, X, Y
                )) #leaves 396 observations

# scatter plot
dat %>%
    ggplot(aes(x = trees1_9020*100, y = trees100_9020*100)) +
    geom_point(alpha = 0.5) +
    labs(x = "Local-level encroachment",
         y = "Regional-level encroachment") +
    theme_classic()

# geom hex heat plot
dat %>%
    ggplot(aes(x = trees1_9020*100, y = trees100_9020*100)) +
    geom_hex()+
    labs(x = "Local-level encroachment",
         y = "Regional-level encroachment",
         fill = "# of\nrespondents") +
    theme_classic(base_size = 10) +
    theme(axis.text = element_text(color="black"))

ggsave("figs/encroachment_heatmap_ES2.png", width = 4, height=2.7, units="in", dpi=300, bg="white")
