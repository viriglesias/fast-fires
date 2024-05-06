#Make Fig s1 (relationships between max growth rates and fire size per land-cover type)
# Requires FIRED data pre-filtered so that croplands are excluded
pacman::p_load(tidyverse)

fire_nocrops <- read.csv('fire_nocrops.csv')
fire_nocrops <- fire_nocrops %>% 
  mutate(lc_short = ifelse(lc_majority == 16 | lc_majority == 17 | lc_majority == 15 | lc_majority == 12 | lc_majority == 11,
                           'Non vegetated', ifelse(lc_majority == 9 | lc_majority == 8,
                                                   'Savannas', ifelse(lc_majority == 4 | lc_majority == 2,
                                                                      'Broadleaf Forests', ifelse(lc_majority == 6 | lc_majority == 7,
                                                                                                  'Shrublands', ifelse(lc_majority == 1,
                                                                                                                       'Conifer Forests', ifelse(lc_majority == '5',
                                                                                                                                                 'Mixed Forests', ifelse(lc_majority == 10,
                                                                                                                                                                         'Grasslands', ifelse(lc_majority == 13,
                                                                                                                                                                                              'Buil-up Lands','Cropland/Natural Vegetation Mosaics')))))))))

fire_nocrops <- fire_nocrops %>% 
  mutate(lc_short = ifelse(lc_name == 'Barren' | lc_name == 'Water Bodies' | lc_name == 'Permanent Snow and Ice',
                           'Non vegetated', ifelse(lc_name == 'Savannas' | lc_name == 'Woody Savannas',
                                                   'Savannas', ifelse(lc_name == 'Deciduous Broadleaf Forests' | lc_name == 'Evergreen Broadleaf Forests',
                                                                      'Broadleaf Forests', ifelse(lc_name == 'Closed Shrublands' | lc_name == 'Open Shrublands',
                                                                                                  'Shrublands', ifelse(lc_name == 'Evergreen Needleleaf Forests',
                                                                                                                       'Conifer Forests', lc_name))))))
fire_nocrops <- fire_nocrops %>% 
  mutate(lc_shortest = ifelse(lc_short == 'Broadleaf Forests', 'Broadleaf Forest',
                              ifelse(lc_short == 'Conifer Forests', 'Conifer Forest',
                                     ifelse(lc_short == 'Grasslands', 'Grassland',
                                            ifelse(lc_short == 'Savannas', 'Savanna',
                                                   ifelse(lc_short == 'Mixed Forests', 'Mixed Forest',
                                                          ifelse(lc_short == 'Shrublands', 'Shrubland', 'Grassland')))))))
fire_nocrops <- fire_nocrops %>% 
  mutate(lc_shortest = as.factor(lc_shortest))
levels(fire_nocrops$lc_shortest)
cls <- c('darkgreen', 'azure4', 'burlywood', 'yellowgreen',  'brown', 'orange')#define colors

ggplot(fire_nocrops) +
  geom_point(aes(log(max_growth_km2*100), log(total_area_km2*100)) , size = .05) +
  geom_smooth(aes(log(max_growth_km2*100), log(total_area_km2*100), col = lc_shortest, fill = lc_shortest), method = 'lm',  se = T, size = .5) +
  theme_bw(base_size = 10) +
  xlab('log(Maximum Single-Day Growth (ha/day))') +
  ylab('log(Fire Size (ha))') +
  scale_color_manual(values = cls, name = 'Landcover type') +
  scale_fill_manual(values = cls) +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 8,
                                   margin = margin(l = 1))) +
  guides(color = guide_legend(title.theme = element_text(size = 10),
                              title.position = 'top',
                              frame.colour = NA,
                              frame.fill = NA, 
                              title.hjust = 0.5,
                              title.vjust = 1,
                              keywidth = .5),
         fill = 'none')
