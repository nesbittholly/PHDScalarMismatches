library(tidyverse)

# read in data and prep for model
dat90_20_nas<-read_csv("data/processed/ProducerDF_TreeCoverChangeCounty_NAsRemoved.csv")

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

# parameter estimate dataframe
burn_df<-tidy(burn_glm1_z, conf.int=T) %>% mutate(Model = "Prescribed burning")
remo_df<-tidy(remo_glm1_z, conf.int=T) %>% mutate(Model = "Mechanical removal")

mixed_df<- bind_rows(
    burn_df, remo_df) %>% 
    #filter(!term=="(Intercept)") %>%
    mutate(term=recode_factor(term,
                              "trees100_9020:trees1_9020" = "Local- and regional-level\nchange in mean % tree\ncover interaction",
                              "trees100_9020" = "Regional-level (100 km)\nchange in mean % tree\ncover from 1990 to 2020",
                              "trees1_9020" = "Local-level (1 km)\nchange in mean % tree\ncover from 1990 to 2020",
                              "group_involve2"="Group involvement",
                              "(Intercept)" = "Intercept"))

ggplot(mixed_df, aes(x=term, y=estimate, group=Model))+
    geom_pointrange(aes(ymin=conf.low, ymax=conf.high,
                        shape=Model, color=Model), 
                    position=position_dodge(.5), size=3, fatten=2,
                    fill="white")+
    coord_flip()+
    geom_hline(yintercept=0, linetype="dashed", linewidth=1)+
    scale_shape_manual(values=c(22,23),
                       breaks=c("Mechanical removal", "Prescribed burning"))+
    scale_color_manual(values = c("orange", "red"),
                       breaks=c("Mechanical removal", "Prescribed burning"))+
    ylab("Beta (log odds) estimate") +
    xlab("")+
    theme_minimal(base_size = 20) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linetype="dotted", color="darkgray", size=.5),
          #strip.text=element_text(size=12, face="bold.italic", color="black"),
          #strip.text.y.right = element_text(angle = 0, hjust=0),
          panel.background = element_rect(colour = "gray", size=1, fill=NA),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text=element_text(size=19),
          axis.text=element_text(size=19, color="black"))+
    #guides(color = guide_legend(override.aes = list(linetype = 1, size=1)))
    guides(color=guide_legend(nrow=2))

#ggsave("9.Analysis/1.HollysCode/figs/Ch3_LogOddsEstimates2.png", width = 12, height=7, units="in", dpi=300, bg="white")
