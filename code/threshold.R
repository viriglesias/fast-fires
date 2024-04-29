#This script defines fast fires a function of social-economic impacts and makes Fig 1
#Requires ics-209 + FIRED
library(tidyverse)
library(rgdal)
library(rsample)   # data splitting 
library(caret)     # automating the tuning process
library(sf)
library(ggthemes)
library(tree)
library(ggpubr)

ics <- read.csv('event_polys_inci209.csv')#Incident reports (ics-209) linked to FIRED. Make sure that complexes are not included

ics <- ics %>% 
  mutate(str_damaged_or_destroyed = STR_DAMAGED_TOTAL + STR_DESTROYED_TOTAL)

ics <- filter(ics, lc_majority != 12)#exclude crops

#Combine landcover classes
ics <- ics %>% 
  mutate(lc_short = ifelse(lc_majority == 16 | lc_majority == 17 | lc_majority == 15,
                           'Non vegetated', ifelse(lc_majority == 9 | lc_majority == 8,
                                                   'Savannas', ifelse(lc_majority == 4 | lc_majority == 2,
                                                                      'Broadleaf Forests', ifelse(lc_majority == 6 | lc_majority == 7,
                                                                                                  'Shrublands', ifelse(lc_majority == 1,
                                                                                                                       'Conifer Forests', ifelse(lc_majority == 5,
                                                                                                                                                 'Mixed Forests', lc_majority)))))))
ics <- ics %>% 
  mutate(lc_shortest = ifelse(lc_short == 'Broadleaf Forests', 'Broadleaf Forest',
                              ifelse(lc_short == 'Conifer Forests', 'Conifer Forest',
                                     ifelse(lc_short == 10, 'Grassland',
                                            ifelse(lc_short == 'Savannas', 'Savanna',
                                                   ifelse(lc_short == 'Mixed Forests', 'Mixed Forest',
                                                          ifelse(lc_short == 'Shrublands', 'Shrubland', 'Grassland')))))))
ics <- ics %>% 
  mutate(lc_shortest = as.factor(lc_shortest))

i1_damage <-filter(ics, str_damaged_or_destroyed>0)#Only keep fires that damaged or destroyed structures

fit <- tree(log(str_damaged_or_destroyed)~log(max_growth_km2), data = i1_damage)#Regression tree to predict damage as a function of max fire spread
fit_pruned <- prune.tree(fit, best = 2)#Prune tree
summary(fit_pruned)

#Plot (fig 1)
cls <- c('darkgreen', 'azure4', 'burlywood', 'yellowgreen',  'brown', 'orange')

a1 <- ggplot(i1_damage) +
  geom_point(aes(max_growth_km2*100, str_damaged_or_destroyed, col = lc_shortest), size = .5) +
  theme_classic(base_size = 10)  +
  geom_vline(aes(xintercept = 1620), linetype = 'dashed') +
  xlab('Maximum Single-Day Growth (ha/day)') +
  scale_x_continuous(trans = 'log',
                     breaks = c(1, 10, 100, 1000, 10000, 100000),
                     labels = c('1', '10', '100', '1,000', '10,000', '100,000')) +
  scale_y_continuous(trans = 'log',
                     breaks = c(1, 10, 100, 1000, 2000, 4000, 8000, 16000, 32000),
                     labels = c('1', '10', '100', '1,000', '2,000', '4,000', '8,000', '16,000', '32,000')) +
  ylab('Structures Damaged or Destroyed') +
  scale_color_manual(values = cls, name = 'Landcover type') +
  theme(legend.title = element_text(angle = 0, size=10),
        legend.justification = 'center',
        text = element_text(family = "Helvetica"),
        legend.position = 'bottom',
        legend.text = element_text(size = 8,
                                   margin = margin(l = -10))) +
  guides(color = guide_legend(title.theme = element_text(size = 10),
                              direction = 'horizontal',
                              label.theme = element_text(angle = 0, size = 8),
                              title.position = 'top',
                              frame.colour = NA,
                              frame.fill = NA, 
                              title.hjust = 0.5,
                              title.vjust = 1))

 
  
dens_fire <- ggplot(i1_damage) +
  geom_density(aes(x = max_growth_km2*100)) +
  theme_classic(base_size = 10) +
  scale_x_continuous(trans = 'log',
                     breaks = c(1, 10, 100, 1000, 10000, 100000),
                     labels = c('1', '10', '100', '1,000', '10,000', '100,000'), 
                     position = 'top') +
  ylab('Density') +
  xlab('') +
  scale_y_continuous(position = 'left') +
  geom_hline(aes(yintercept = 0), linetype = 'dotted')
  

i1_damage <- i1_damage %>% 
  mutate(lev = ifelse(max_growth_km2>16.2, 'Fast fires', 'Other fires'),
         dummy = 'a')
dens_str <- ggplot(i1_damage) +
  geom_density(aes(str_damaged_or_destroyed, col = lev)) +
  theme_classic(base_size = 10) +
  scale_color_manual(values = c('red', 'black')) +
  coord_flip() +
  scale_x_continuous(trans = 'log',
                     breaks = c(1, 10, 100, 1000, 2000, 4000, 8000, 16000, 32000),
                     labels = c('1', '10', '100', '1,000', '2,000', '4,000', '8,000', '16,000', '32,000'),
                     position = 'top') +
  scale_y_continuous(position = 'left',
                     breaks = c(.1, .3)) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted')+
  ylab('Density') +
  guides(color = 'none') +
  geom_text(aes(y = c(0.05), x = c(32000), label = c('Fast fires')), 
                col = c('red'), size = 2.5, hjust = 'left') +
  geom_text(aes(y = c(0.05), x = c(16000), label = c('Other fires')), 
                col = c('black'), size = 2.5, hjust = 'left') +
  xlab('')

dens_fire + patchwork::plot_spacer() + a1 + dens_str + 
  patchwork::plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
