#Makes figures 4 and S8 (Exposure to fast fires)
#Requires fast_zillow_complete.csv. 
pacman::p_load(raster, tidyverse, scales, cowplot)

fast_zill_complete <- read.csv('/fast_zill_complete.csv')

#Fig 4
a <- ggplot(fast_zill_complete) +
  geom_area(aes(year, structures_variable_year_4k), fill = 'gray65') +
  geom_area(aes(year, structures_variable_year_1k), fill = 'gray35') +
  geom_area(aes(year, structures_variable_year), fill = 'black') +
  theme_bw(base_size = 10) +
  scale_y_continuous(labels = comma) +
  ylab('Number of Structures') +
  xlab('Year') +
  labs(tag = '(C)') +
  theme(plot.tag = element_text(face = 'bold'))

b <- ggplot(fast_zill_complete) +
  geom_line(aes(year, burned_area_fast)) +
  theme_bw(base_size = 10) +
  scale_y_continuous(labels = comma) +
  ylab('Annual Area Burned \n by Fast Fires (ha)') +
  xlab('Year') +
  labs(tag = '(B)') +
  theme(plot.tag = element_text(face = 'bold'))

d <- ggplot(fast_zill_complete) +
  geom_line(aes(year, total_events)) +
  theme_bw(base_size = 10) +
  scale_y_continuous(labels = comma) +
  ylab('Number of Fast Fires') +
  xlab('Year') +
  labs(tag = '(A)') +
  theme(plot.tag = element_text(face = 'bold'))

plot_grid(d, b, a, nrow = 3, align = 'v')


#Fig S8
a <- ggplot(fast_zill_complete) +
  geom_area(aes(year, cum_structures_variable_year_4k), fill = 'gray65') +
  geom_area(aes(year, cum_structures_variable_year_1k), fill = 'gray35') +
  geom_area(aes(year, cum_structures_variable_year), fill = 'black') +
  theme_bw(base_size = 10) +
  scale_y_continuous(labels = comma) +
  ylab('Cumulative Number \n of Structures') +
  xlab('Year') +
  labs(tag = '(C)') +
  theme(plot.tag = element_text(face="bold"))

b <- ggplot(fast_zill_complete) +
  geom_line(aes(year, cum_burned_area_fast)) +
  theme_bw(base_size = 10) +
  scale_y_continuous(labels = comma) +
  ylab('Cumulative Area Burned \n by Fast Fires (ha)') +
  xlab('Year') +
  labs(tag = '(B)') +
  theme(plot.tag = element_text(face="bold"))

d <- ggplot(fast_zill_complete) +
  geom_line(aes(year, cum_events)) +
  theme_bw(base_size = 10) +
  scale_y_continuous(labels = comma) +
  ylab('Cumulative Number \n of Fast Fires') +
  xlab('Year') +
  labs(tag = '(A)') +
  theme(plot.tag = element_text(face="bold"))

plot_grid(d, b, a, nrow = 3, align = 'v')


