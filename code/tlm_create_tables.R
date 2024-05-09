#Tyler L. McIntosh
#Create final data tables for fast fires publication

#Check the required libraries and download if needed, then load the libraries
list.of.packages <- c("tidyverse",
                      "here",
                      "googlesheets4",
                      "flextable")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE) #apply library function to all packages

options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)

#Set font parameters for tables
fontname <- "Times New Roman"
fontsize <- 8

# Ecoregion Fire Characteristic Summaries ----
efcsTableNum <- "S1"

efcs <- read_sheet("https://docs.google.com/spreadsheets/d/1KZ1WNwOBnQNlIx3-D-hb_QURGJc2typYzKwe81F3-TU/edit#gid=2134256498",
           sheet = NULL)

#clean up extra rows and digits
efcsClean <- efcs[1:13, ] %>%
  mutate_if(is.double, ~ round(., digits = 1))

#Fix names and add units
names(efcsClean) <- c("Region",
                    "Median Single Day Maximum Fire Growth Rate (ha / day)",
                    "Mean Single Day Maximum Fire Growth Rate (ha / day)",
                    "Maximum Single Day Maximum Fire Growth Rate (ha / day)",
                    "Median Fire Growth Rate (ha / day)",
                    "Mean Fire Growth Rate (ha / day)",
                    "Median Fire Duration (days)",
                    "Mean Fire Duration (days)",
                    "Median Fire Size (ha)",
                    "Mean Fires Size (ha)",
                    "Total Number of Fires",
                    "Number of Fast Fires",
                    "Fast Fires Percentage of Total Fires (%)")

#Re-order rows and take out L1 keys
efcsCleanEPA <- efcsClean[1:10,] %>%
  mutate(Region = str_trim(gsub("\\d+", "", Region))) %>%
  arrange(Region) %>%
  mutate(Region = paste(Region, "*"))

efcsCleanNonEPA <- efcsClean[11:13,]
efcsCleanNonEPA[3,1] <- 'CALIFORNIA'
efcsCleanNonEPA[2,1] <- 'WESTERN US'

efcsClean2 <- rbind(efcsCleanNonEPA, efcsCleanEPA)


efcs_ft1 <- flextable(efcsClean2) %>%
  bold(i = 1:3, bold = TRUE) %>%
  font(fontname = fontname, part = "all")

efcs_ft1

write_csv(efcsClean2, here::here('outputs', paste0('science.adk5757_table_', efcsTableNum, '.csv')))

# Land cover Fire Characteristic Summaries for CONUS and the western United States ----
lcfcSummsTableNum <- "S3"
lcfcSumms <- read_sheet("https://docs.google.com/spreadsheets/d/10D-wZtRMs9wP7tvBk0oR8FCt3n-sLMe59uxceUI8GSY/edit#gid=1717470864", 
                 sheet = NULL)

#clean up extra digits
lcfcSummsClean <- lcfcSumms %>% mutate_if(is.double, ~ round(., digits = 1))

write_csv(lcfcSummsClean, here::here('outputs', paste0('science.adk5757_table_', lcfcSummsTableNum, '.csv')))



# Statistics for area burned during the day of maximum fire growth for land cover types within ecoregions ----
dayOfMaxGrowthEcoStatsTableNum <- "S2"

dayOfMaxGrowthEcoStats <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1tOsoVVqjI-0_Dqu3zdNBt0caOBnu9TbDA9IHYwxZSkI/edit#gid=286868974",
                                sheet = NULL)

dayOfMaxGrowthEcoStatsClean <- dayOfMaxGrowthEcoStats[2:nrow(dayOfMaxGrowthEcoStats), ] %>%
  mutate(`Ecoregion_level-I` = str_trim(gsub("\\d+", "", `Ecoregion_level-I`)), .keep = "unused") %>%
  arrange(`Ecoregion_level-I`) %>%
  select(-`...10`) %>%
  mutate_at(vars(Events:Perc_burned_max_max), as.numeric) %>%
  mutate(Land_cover = ifelse(Land_cover == "Buil-up Lands", "Built-up Lands", Land_cover),
         Land_cover = ifelse(Land_cover == "Non vegetated*" | Land_cover == "Non vegetated", "Non-vegetated", Land_cover))

#Convert to ha for consistency w/ rest of paper
dayOfMaxGrowthEcoStatsCleanHa <- dayOfMaxGrowthEcoStatsClean %>%
  mutate(Area_burned_total = Area_burned_total * 100,
         Area_burned_max_growth_sum = Area_burned_max_growth_sum * 100,
         Area_burned_max_growth_max = Area_burned_max_growth_max * 100) %>%
  mutate(`Mean max growth` = Area_burned_max_growth_sum / Events) %>%
  mutate_if(is.double, ~ round(., digits = 1))

#Fix names and add units
names(dayOfMaxGrowthEcoStatsCleanHa) <- c("EPA Level I Ecoregion",
                    "Modis MCD12Q1 Land cover",
                    "Number of Fire Events",
                    "Total Area Burned (ha)",
                    "Area Burned During Day of Maximum Growth across all Events (ha)",
                    "Area Burned During Day of Maximum Growth for the Fastest Event (ha)",
                    "Percent of Total Area Burned During Day of Maximum Growth across all Events (%)",
                    "Percent of Total Area Burned During Day of Maximum Growth for the Fastest Event (%)",
                    "Average Area Burned During Day of Maximum Growth (ha)")

write_csv(dayOfMaxGrowthEcoStatsCleanHa, here::here('outputs', paste0('science.adk5757_table_', dayOfMaxGrowthEcoStatsTableNum, '.csv')))


# Top 100 fastest fires in the US - MAX ----


# Fire characteristic trend coefficients and p-values by EPA Level III Ecoregions ----

fcTrendsPEcoTableNum <- "S6"

fcTrendsPEco <-  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WBDStHwRYouqaSGyV8fDVuvjjUGXLm563k1DRGFcsPM/edit#gid=2014372530",
                                 sheet = NULL)

fcTrendsPEcoClean <- fcTrendsPEco %>%
  select(-order) %>%
  mutate(Level_3 = str_trim(gsub("\\d+", "", Level_3)),
         US_L1CODE = as.numeric(gsub("[^0-9.]", "", Level_1)),
         Level_1 = str_trim(gsub("\\d+", "", Level_1)))

fcTrendsPEcoCleanSig <- fcTrendsPEcoClean %>%
  mutate(fsr_p_sig = case_when(
      pval_fsr < 0.001 ~ "***",
      pval_fsr < 0.01 ~ "**",    
      pval_fsr < 0.05 ~ "*",
      pval_fsr >= 0.05 ~ "-"),
    growth_p_sig = case_when(
      pval_max_growth < 0.001 ~ "***",
      pval_max_growth < 0.01 ~ "**",    
      pval_max_growth < 0.05 ~ "*",
      pval_max_growth >= 0.05 ~ "-"),
    duration_p_sig = case_when(
      pval_duration < 0.001 ~ "***",
      pval_duration < 0.01 ~ "**",    
      pval_duration < 0.05 ~ "*",
      pval_duration >= 0.05 ~ "-")
    ) %>%
  select(
    Level_1,
    US_L1CODE,
    Level_3,
    US_L3CODE,
    `coeff_max_growth (ha/day/year) - this is what we are actually using`,
    pval_max_growth,
    growth_p_sig,
    `coeff_duration - fire duration`,
    pval_duration,
    duration_p_sig,
    `coeff_fsr (ha/day/year) - simple fire spread rate`,
    pval_fsr,
    fsr_p_sig
  ) %>%
  arrange(US_L1CODE, US_L3CODE) %>%
  mutate(`coeff_fsr (ha/day/year) - simple fire spread rate` = formatC(`coeff_fsr (ha/day/year) - simple fire spread rate`,
                                                                       format = "f",
                                                                       digits = 1),
         `coeff_duration - fire duration` = formatC(`coeff_duration - fire duration`,
                                                                       format = "f",
                                                                       digits = 1),
         `coeff_max_growth (ha/day/year) - this is what we are actually using` = formatC(`coeff_max_growth (ha/day/year) - this is what we are actually using`,
                                                    format = "f",
                                                    digits = 1),
         pval_max_growth = formatC(pval_max_growth,
                                                    format = "e",
                                                    digits = 1),
         pval_duration = formatC(pval_duration,
                                   format = "e",
                                   digits = 1),
         pval_fsr = formatC(pval_fsr,
                                   format = "e",
                                   digits = 1)
         )

names(fcTrendsPEcoCleanSig) <- c(
  "EPA Level I Ecoregion",
  "EPA Level I Ecoregion Code",
  "EPA Level III Ecoregion",
  "EPA Level III Ecoregion Code",
  "Single Day Maximum Fire Growth Rate Theil-Sen Coefficient (ha/day/year)",
  "Single Day Maximum Fire Growth Rate P-value",
  "Single Day Maximum Fire Growth Rate P-value Significance",
  "Fire Duration Theil-Sen Coefficient (day/year)",
  "Fire Duration P-value",
  "Fire Duration P-value Significance",
  "Simple Fire Spread Rate Theil-Sen Coefficient (ha/day/year)",
  "Simple Fire Spread Rate P-value",
  "Simple Fire Spread Rate P-value Significance"
)

fcTrendsPEcoCleanSig_ft <- flextable(fcTrendsPEcoCleanSig) %>%
  font(fontname = fontname, part = "all")

fcTrendsPEcoCleanSig_ft

write_csv(fcTrendsPEcoCleanSig, here::here('outputs', paste0('science.adk5757_table_', fcTrendsPEcoTableNum, '.csv')))



# Temporal changes in fire growth rate for California and the western United States ----
fgrTemporalChangesTableNum <- "S7"

fgrTemporalChanges <-  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1aIU_iH3niQHSz8Fs3Wkc_-Tujz54QlFa6K1-oehv0Ms/edit#gid=91047183",
                                 sheet = "formatted")

fgrTemporalChangesClean <- fgrTemporalChanges %>%
  mutate_if(is.double, ~ round(., digits = 1))

fgrTemporalChangesClean_ft <- flextable(fgrTemporalChangesClean) %>%
  font(fontname = fontname, part = "all") %>%
  fontsize(size = fontsize, part = "all")

fgrTemporalChangesClean_ft

print(fgrTemporalChangesClean_ft, preview = "docx")

# Percent increases in maximum fire growth rate between 2001 and 2020 for California and the western United States ----
percIncreasesFGRTableNum <- "S8"

percIncreasesFGR <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Pai9RK3H_o29LcTKEKZBIdq098p2-JPKPSGX3VItRPs/edit#gid=0",
                                 sheet = NULL)

percIncreasesFGRClean <- percIncreasesFGR %>%
  select(-`Modeled max growth_2001`,
         -`Modeled max growth_2020`,
         -`Modeled max growth_2020/Modeled max growth_2001`)
  
names(percIncreasesFGRClean) <- c("Region",
                    "Mean maximum fire growth rate in 2001 (ha/day)",
                    "Median maximum fire growth rate in 2001 (ha/day)",
                    "Mean maximum fire growth rate in 2020 (ha/day)",
                    "Median maximum fire growth rate in 2020 (ha/day)",
                    "Mean maximum fire growth rate in 2020 as a percentage of mean maximum fire growth rate in 2001 (%)",
                    "Median maximum fire growth rate in 2020 as a percentage of median maximum fire growth rate in 2001 (%)")

percIncreasesFGRClean <- percIncreasesFGRClean %>%
  mutate("Mean maximum fire growth rate in 2020 as a percentage of mean maximum fire growth rate in 2001 (%)" = `Mean maximum fire growth rate in 2020 as a percentage of mean maximum fire growth rate in 2001 (%)` * 100,
          "Median maximum fire growth rate in 2020 as a percentage of median maximum fire growth rate in 2001 (%)" = `Median maximum fire growth rate in 2020 as a percentage of median maximum fire growth rate in 2001 (%)` * 100) %>%
  mutate_if(is.double, ~ round(., digits = 1))

percIncreasesFGRClean_ft <- flextable(percIncreasesFGRClean) %>%
  font(fontname = fontname, part = "all") %>%
  fontsize(size = fontsize, part = "all") %>%
  set_table_properties(width = 1, layout = "autofit")

percIncreasesFGRClean_ft

print(percIncreasesFGRClean_ft, preview = "docx")


# States threshold table ----
sttTableNum <- "S5"
sttDat <-  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1k5g5oSTbgCgofHgcJzlA-uhSvoy_xkhDCHZ-9MD4XRw/edit#gid=1668931080",
                                     sheet = NULL)

sttClean <- sttDat |>
  dplyr::select(-`Threshold 2 (ha/day [raw data])`, -`Number of fires faster than threshold 2`, -`Number of structures damaged or destroyed by fast fires as defined by threshold 2`) |>
  dplyr::mutate(`Maximum daily fire growth rate (ha/day)` = round(`Maximum daily fire growth rate (ha/day)`),
                `Number of fast fires` = as.character(`Number of fast fires`),
                `Number of structures damaged or destroyed by fast fires` = as.character(`Number of structures damaged or destroyed by fast fires`),
                `Fast fire threshold (ha/day)` = as.character(`Fast fire threshold (ha/day)`)) |>
  dplyr::mutate(dplyr::across(`Number of fast fires`:`Number of structures damaged or destroyed by fast fires`, ~dplyr::na_if(.x, "NA")))

readr::write_csv(sttClean, here::here('outputs', paste0('science.adk5757_table_', sttTableNum, '.csv')))

