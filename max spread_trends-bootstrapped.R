#This script calculates bootstrapped Theil-Sen estimators for temporal trends in maximum fire spread in the west and in California
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

fire_short_west <-filter(fire_short, STATE_NAME %in% c('Washington', 'Oregon', 'California', 'Idaho', 'Nevada',
                                                 'Utah', 'Arizona', 'Montana', 'Wyoming', 'Colorado', 'New Mexico'))#Only keep fires in the western US
fire_short_ca <- filter(fire_short, STATE_NAME %in% c('California'))#Only keep fires in CA

#Helper function to bootstrap the Theil-Sen protocol
boot_lm <- function(data, sample_size, replicates){#data is the data frame with fire data; sample_size is the number of samples that will be taken from 'data' to estimate the Theil-Sen coefficient, and replicates is the number of bootstrap replicates 
  a <- data %>%
    group_by(ignition_year) %>%
    slice_sample(n = sample_size, replace = T)
  m <- mblm(max_growth_km2~ignition_year, dataframe = a, repeated = F)
  fire_lm1 <- data.frame(coeff = unname(m$coefficients[2]), samples = sample_size, replicate = replicates)#Data frame with Theil-Sen coefficients from models fitted to max growth as a function of year, sample size, and bootstrap replicate number
  return(fire_lm1)
}

#Fit model to western US
registerDoParallel(detectCores()-1)#Run in parallel
replicates <- 1:1000#Bootstrap 1000 times
west <- foreach(i = 1:length(replicates))%dopar%{
  boot_lm(data = fire_short_west, sample_size = 250, cores = cores[[i]])
}

median_west <- boot(data = west$coeff,statistic = function(x,i) median(x[i], na.rm = T),R = 1000)#Estimate median Theil_Sen estimator for the west
boot.ci(median_west)#Estimate confidence intervals for the median
mean_west <- boot(data = west$coeff,statistic = function(x,i) mean(x[i], na.rm = T),R = 1000)#Estimate mean Theil_Sen estimator for the west
boot.ci(mean_west)#Estimate confidence intervals for the mean

#Fit model to California
registerDoParallel(detectCores()-1)#Run in parallel
replicates <- 1:1000#Bootstrap 1000 times
ca <- foreach(i = 1:length(replicates))%dopar%{
  boot_lm(data = fire_short_ca, sample_size = 250, cores = cores[[i]])
}

median_ca <- boot(data = ca$coeff,statistic = function(x,i) median(x[i], na.rm = T),R = 1000)#Estimate median Theil_Sen estimator for CA
boot.ci(median_ca)#Estimate confidence intervals for the median
mean_ca <- boot(data = ca$coeff,statistic = function(x,i) mean(x[i], na.rm = T),R = 1000)#Estimate mean Theil-Sen estimator for CA
boot.ci(mean_west)#Estimate confidence intervals for the mean

