#Makes Fig. S2 (ECD fire duration and max growth day)
#Requires FIRED data
pacman::p_load(tidyverse, ggpubr, ggthemes, sf, DescTools, scales, rgdal)

fire <- st_read('event_polys_albers_conus_w_eco_lc.gpkg')
fire <- as.data.frame(fire)
fire <- fire %>% 
  mutate(lc_majority = as.factor(lc_majority))

#Get landcover info
lc_rat <- st_read('event_poly_conus_albers_w_stats_lc_to2020.gpkg')
lc_rat <- as.data.frame(lc_rat)
lc_rat <- lc_rat[,c(23,24)]
lc_rat <- unique(lc_rat[,1:2])
lc_rat <- mutate(lc_rat, lc_majority = as.factor(lc_majority))
fire <- left_join(fire, lc_rat)

fire <- subset(fire, !lc_name %in% 'Croplands')#exclude croplands
fire <- fire %>% 
  mutate(lc_short = ifelse(lc_name == 'Barren' | lc_name == 'Water Bodies' | lc_name == 'Permanent Snow and Ice',
                           'Non vegetated', ifelse(lc_name == 'Savannas' | lc_name == 'Woody Savannas',
                                                   'Savannas', ifelse(lc_name == 'Deciduous Broadleaf Forests' | lc_name == 'Evergreen Broadleaf Forests',
                                                                      'Broadleaf Forests', ifelse(lc_name == 'Closed Shrublands' | lc_name == 'Open Shrublands',
                                                                                                  'Shrublands', ifelse(lc_name == 'Evergreen Needleleaf Forests',
                                                                                                                       'Conifer Forests', lc_name))))))#rename landcover types
fire <- fire %>% 
  mutate(lc_shortest = ifelse(lc_short == 'Broadleaf Forests', 'Broadleaf Forests',
                              ifelse(lc_short == 'Conifer Forests', 'Conifer Forests',
                                     ifelse(lc_short == 'Grasslands', 'Grasslands',
                                            ifelse(lc_short == 'Savannas', 'Savannas',
                                                   ifelse(lc_short == 'Mixed Forests', 'Mixed Forests',
                                                          ifelse(lc_short == 'Shrublands', 'Shrublands', NA)))))))#rename landcover types
fire <- fire %>% 
  mutate(lc_shortest = ifelse(lc_short == 'Broadleaf Forests', 'Broadleaf Forest',
                              ifelse(lc_short == 'Conifer Forests', 'Conifer Forest',
                                     ifelse(lc_short == 'Grasslands', 'Grassland',
                                            ifelse(lc_short == 'Savannas', 'Savanna',
                                                   ifelse(lc_short == 'Mixed Forests', 'Mixed Forest',
                                                          ifelse(lc_short == 'Shrublands', 'Shrubland', 'Grassland')))))))
fire <- fire %>% 
  mutate(lc_shortest = as.factor(lc_shortest))

levels(fire$lc_shortest)#reorder landcover types

cls <- c('darkgreen', 'azure4', 'burlywood', 'yellowgreen',  'brown', 'orange')# define colors for figure

b <- fire %>% 
  filter(!is.na(lc_shortest)) %>% 
  ggplot() +
  stat_ecdf(aes(max_growth_event_day, color = lc_shortest), geom = "line", pad = F) +
  scale_color_manual(values = cls, name = 'Landcover type') +
  theme_bw(base_size = 10) +
  xlab('Maximum Single-Day Growth Day') +
  ylab('Proportion of Fires') +
  labs(tag = '(B)') +
  xlim(0, 40) +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 8),
        plot.tag = element_text(face = 'bold')) +
  guides(color = guide_legend(title.theme = element_text(size = 10),
                              title.position = 'top',
                              frame.colour = NA,
                              frame.fill = NA, 
                              title.hjust = 0.5,
                              title.vjust = 1)) 
  
  

a <- fire %>% 
  filter(!is.na(lc_shortest)) %>% 
  ggplot() +
  stat_ecdf(aes(event_duration, color = lc_shortest), geom = "line", pad = F) +
  theme_bw(base_size = 10) +
  scale_color_manual(values = cls, name = 'Landcover type') +
  guides(color = guide_legend(title.theme = element_text(size = 10),
                              title.position = 'top',
                              frame.colour = NA,
                              frame.fill = NA, 
                              title.hjust = 0.5,
                              title.vjust = 1)) +
  xlab('Event Duration') +
  ylab('Proportion of Fires') +
  xlim(0, 40) +
  labs(tag = '(A)') +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 8),
        plot.tag = element_text(face = 'bold'))

ggpubr::ggarrange(a, b, 
                  nrow = 2, ncol = 1,
                  common.legend = T, legend = 'bottom')



