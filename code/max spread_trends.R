#This script calculates Theil-Sen estimators for temporal trends in maximum fire spread at the EPA ecoregion level 3
#It requires data frame with FIRED data excluding crops
library(tidyverse)
library(foreach)
library(doParallel)
library(mblm)

fire_nocrops <- read.csv('fire_nocrops.csv')#Dataframe with FIRED data, excluding crops
fire_nocrops <- fire_nocrops %>% 
  mutate(date = as.Date(ignition_date, format = '%m/%d/%y'))#Assign class date to ignition dates
fire_short <-filter(fire_nocrops, event_duration>4)#Remove fires shorter than 5 days
fire_short <- data.frame(fire_short) %>% 
  mutate(ignition_year = as.numeric(ignition_year),
         max_growth_ha = as.numeric(max_growth_km2*100))#Convert ha to km2
fire_short_sum <- fire_short %>% 
  group_by(US_L3CODE) %>% 
  summarize(fires = n())#Calculate number of fires per ecoregion 
fire_short_sum <- filter(fire_short_sum, fires>9)#Remove ecoregions with <10 fires
fire_short <- left_join(fire_short, fire_short_sum)
fire_short <- filter(fire_short, !is.na(fires))

fire_split <- split(fire_short, fire_short$US_L3CODE)#split data frame

registerDoParallel(detectCores()-1)#Run in parallel
m <- foreach(i = 1:length(fire_split),.combine=rbind)%dopar%{
  list(summary(mblm(max_growth_ha~ignition_year, dataframe = fire_split[[i]], repeated = F)))
}#Estimate Theil-Sen coeffs by ecoregion
m_coeff <- unname(sapply(m[,1], '[[', 4)[2,])#theil sen estimators
m_p_val <- unname(sapply(m[,1], '[[', 4)[8,])#p values
fire_theisen <- data.frame(coeff = m_coeff, p_val = m_p_val, us_l3 = names(fire_split))#create data frame or theil-sen estimators, p-values and ecoregion names
