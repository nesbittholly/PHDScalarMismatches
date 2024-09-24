library(tidyr)
library(sf)
library(maps)
library(ggplot2)

# US inset
usa <- 
    st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

ne <-
    tigris::states(cb=TRUE)%>%
    dplyr::filter(STUSPS %in% c("NE"))

ggplot() +
    geom_sf(data = usa, color = "#2b2b2b", fill = "white", lwd=0.3) +
    geom_sf(data = ne, col="#9f2305", fill="white", lwd=1) +
    coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          text = element_text("serif", size = 15))

#ggsave("US_inset.png", width = 5, height = 3, units="in", dpi=300)


# sample points
dat90_20_nas<-read_csv("data/processed/ProducerDF_TreeCoverChangeCounty_NAsRemoved.csv")
range_pts <- sf::read_sf("data/original/RangelandSurvey.gdb", layer = "SurveySampleFrame") %>%
    dplyr::select(X:Ymax, uniqueID, Shape) %>%
    sf::st_transform(4326) %>%
    right_join(.,dat90_20_nas)

# ecoregions and nrds
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

nrd_eco<-range_pts_nrd_eco%>%dplyr::select(uniqueID, nrd, eco) #%>% st_drop_geometry()
dat90_20_nrd<-inner_join(nrd_eco, dat90_20, by="uniqueID")


ggplot() +
    geom_sf(data = ne, col="#2b2b2b", fill="white", lwd=0.3) +
    geom_sf(data = nrds, col="#2b2b2b", fill="white", lwd=0.3) +
    #geom_sf(data = ecos, col="#2b2b2b", fill="white", lwd=0.3) +
    geom_sf(data = range_pts, alpha = 0.8,
            aes(shape = as.factor(group_involve2),
                fill = trees100_9020*100,
                size = as.factor(b_burn))) +
    coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
    scale_size_manual(values = c(1, 2.5), labels = c("Never/rarely", "Occasionally/frequently")) +
    scale_shape_manual(values = c(21,23), labels = c("Not involved", "Involved")) +
    viridis::scale_fill_viridis(option="mako", #mako or magma?
                                breaks = c(0, 2.5, 5.0, 7.5, 10), 
                                labels = c("0.0 (Low change)", "2.5", "5.0", "7.5", "10.0 (High change)"))+
    ggspatial::annotation_scale(location="bl", width_hint= 0.2,
                                pad_x=unit(0.1, "in"), #pad_y=unit(-0.01, "in"),
                                #text_family="serif", 
                                text_cex=.75) +
    ggspatial::annotation_north_arrow(location="tr", which_north="true",
                                      pad_x = unit(0.4, "in"),
                                      height = unit(0.4, "in"),
                                      width = unit(0.4, "in"),
                                      style=ggspatial::north_arrow_fancy_orienteering()#text_family="sansserif")
                                      ) +
    labs(shape = "Group involvement",
         fill = "Regional-level (100 km radius)\nchange in mean % tree cover\nfrom 1990 to 2020",
         size = "Prescribed burning"
         #x = NULL, y = NULL, alpha = NULL
         ) +
    guides(size = guide_legend(order = 1),
           shape = guide_legend(override.aes = list(size = 2.5, fill = "#00000080"), order = 2)) +#,
           #fill = guide_legend(order = 3)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(#"serif", 
                              size = 11))

#ggsave("figs/map_respondents_burn.png", width = 9, height=5, units="in", dpi=300, bg="white")

#scales::viridis_pal(option = "G")(12)

