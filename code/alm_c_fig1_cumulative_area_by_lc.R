#Adam Mahood

library(tidyverse)
library(sf)
library(funique)#install.packages("funique")
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- funique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

daily <- st_read("C:/Users/Adam.Mahood/data/fired/FIRED_CONUS_Daily_w_README/fired_conus_daily_nov2001-jan2019.gpkg")

d_lc_plot <- daily %>%
  st_set_geometry(NULL) %>%
  group_by(id) %>%
  mutate(lc_name = Mode(lc_name)) %>%
  ungroup() %>%
  filter(!lc_name %in% c("Permanent Snow and Ice", "Barren", "Permanent Wetlands",
                         "Water Bodies", "Croplands"),
         !str_detect(lc_name,"Cropland"),
         !str_detect(lc_name,"Urban")) %>%
  mutate(lc_name = case_match(lc_name,
                              "Woody Savannas" ~ "Savanna",
                              "Closed Shrublands" ~ "Shrubland",
                              "Savannas" ~ "Savanna",
                              "Deciduous Broadleaf Forests" ~ "Broadleaf Forest",
                              "Open Shrublands" ~ "Shrubland",
                              "Mixed Forests" ~ "Mixed Forest",
                              "Evergreen Broadleaf Forests" ~ "Broadleaf Forest",
                              "Evergreen Needleleaf Forests" ~ "Conifer Forest",
                              "Grasslands" ~ "Grassland"))


lc_plot <- d_lc_plot %>%
  ggplot(aes(x=event_day, y=cum_area_km2, 
             group = as.factor(id))) +
  geom_line(alpha = 0.25) +
  scale_color_discrete(name = "Landcover") +
  xlab("Days Since Ignition") +
  ylab("Cumulative Burned Area (km2)") +
  facet_wrap(~lc_name) +
  theme_classic() +
  theme(panel.background = element_rect(fill=NA, color="black"))

ggsave(filename = "images/lc_cum_area.png", lc_plot, height = 7, width = 10 )
