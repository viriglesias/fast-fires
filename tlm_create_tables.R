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

# Table 1 ----
t1 <- read_sheet("https://docs.google.com/spreadsheets/d/1KZ1WNwOBnQNlIx3-D-hb_QURGJc2typYzKwe81F3-TU/edit#gid=2134256498",
           sheet = NULL)

#clean up extra rows and digits
t1clean <- t1[1:13, ] %>%
  mutate_if(is.double, ~ round(., digits = 1))

#Fix names and add units
names(t1clean) <- c("Region",
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
t1cleanEPA <- t1clean[1:10,] %>%
  mutate(Region = str_trim(gsub("\\d+", "", Region))) %>%
  arrange(Region) %>%
  mutate(Region = paste(Region, "*"))

t1cleanNonEPA <- t1clean[11:13,]
t1cleanNonEPA[3,1] <- 'CALIFORNIA'
t1cleanNonEPA[2,1] <- 'WESTERN US'

t1clean2 <- rbind(t1cleanNonEPA, t1cleanEPA)


ft1 <- flextable(t1clean2) %>%
  bold(i = 1:3, bold = TRUE) %>%
  font(fontname = fontname, part = "all")

ft1

write_csv(t1clean2, here::here('outputs', 'science.adk5757_table_s1.csv'))

# Table 2 ----
t2 <- read_sheet("https://docs.google.com/spreadsheets/d/10D-wZtRMs9wP7tvBk0oR8FCt3n-sLMe59uxceUI8GSY/edit#gid=1717470864", 
                 sheet = NULL)

#clean up extra digits
t2clean <- t2 %>% mutate_if(is.double, ~ round(., digits = 1))

write_csv(t2clean, here::here('outputs', 'science.adk5757_table_s2.csv'))



# Table 3 ----
t3 <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1tOsoVVqjI-0_Dqu3zdNBt0caOBnu9TbDA9IHYwxZSkI/edit#gid=286868974",
                                sheet = NULL)

t3clean <- t3[2:nrow(t3), ] %>%
  mutate(`Ecoregion_level-I` = str_trim(gsub("\\d+", "", `Ecoregion_level-I`)), .keep = "unused") %>%
  arrange(`Ecoregion_level-I`) %>%
  select(-`...10`) %>%
  mutate_at(vars(Events:Perc_burned_max_max), as.numeric) %>%
  mutate(Land_cover = ifelse(Land_cover == "Buil-up Lands", "Built-up Lands", Land_cover),
         Land_cover = ifelse(Land_cover == "Non vegetated*" | Land_cover == "Non vegetated", "Non-vegetated", Land_cover))

#Convert to ha for consistency w/ rest of paper
t3cleanHa <- t3clean %>%
  mutate(Area_burned_total = Area_burned_total * 100,
         Area_burned_max_growth_sum = Area_burned_max_growth_sum * 100,
         Area_burned_max_growth_max = Area_burned_max_growth_max * 100) %>%
  mutate(`Mean max growth` = Area_burned_max_growth_sum / Events) %>%
  mutate_if(is.double, ~ round(., digits = 1))

#Fix names and add units
names(t3cleanHa) <- c("EPA Level I Ecoregion",
                    "Modis MCD12Q1 Landcover",
                    "Number of Fire Events",
                    "Total Area Burned (ha)",
                    "Area Burned During Day of Maximum Growth across all Events (ha)",
                    "Area Burned During Day of Maximum Growth for the Fastest Event (ha)",
                    "Percent of Total Area Burned During Day of Maximum Growth across all Events (%)",
                    "Percent of Total Area Burned During Day of Maximum Growth for the Fastest Event (%)",
                    "Average Area Burned During Day of Maximum Growth (ha)")

write_csv(t3cleanHa, here::here('outputs', 'science.adk5757_table_s3.csv'))


# Table 4 - MAX ----


# Table 5 ----

t5 <-  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WBDStHwRYouqaSGyV8fDVuvjjUGXLm563k1DRGFcsPM/edit#gid=2014372530",
                                 sheet = NULL)

t5clean <- t5 %>%
  select(-order) %>%
  mutate(Level_3 = str_trim(gsub("\\d+", "", Level_3)),
         US_L1CODE = as.numeric(gsub("[^0-9.]", "", Level_1)),
         Level_1 = str_trim(gsub("\\d+", "", Level_1)))

t5cleanSig <- t5clean %>%
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

names(t5cleanSig) <- c(
  "EPA Level I Ecoregion",
  "EPA Level I Ecoregion Code",
  "EPA Level III Ecoregion",
  "EPA Level III Ecoregion Code",
  "Single Day Maximum Fire Growth Rate Thiel-Sen Coefficient (ha/day/year)",
  "Single Day Maximum Fire Growth Rate P-value",
  "Single Day Maximum Fire Growth Rate P-value Significance",
  "Fire Duration Thiel-Sen Coefficient (day/year)",
  "Fire Duration P-value",
  "Fire Duration P-value Significance",
  "Simple Fire Spread Rate Thiel-Sen Coefficient (ha/day/year)",
  "Simple Fire Spread Rate P-value",
  "Simple Fire Spread Rate P-value Significance"
)

ft5 <- flextable(t5cleanSig) %>%
  font(fontname = fontname, part = "all")

ft5

write_csv(t5cleanSig, here::here('outputs', 'science.adk5757_table_s5.csv'))

# Table 6 ----

t6 <-  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1aIU_iH3niQHSz8Fs3Wkc_-Tujz54QlFa6K1-oehv0Ms/edit#gid=91047183",
                                 sheet = "formatted")

t6clean <- t6 %>%
  mutate_if(is.double, ~ round(., digits = 1))

ft6 <- flextable(t6clean) %>%
  font(fontname = fontname, part = "all") %>%
  fontsize(size = fontsize, part = "all")

ft6

print(ft6, preview = "docx")

# Table 7 ----

t7 <-  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Pai9RK3H_o29LcTKEKZBIdq098p2-JPKPSGX3VItRPs/edit#gid=0",
                                 sheet = NULL)

t7clean <- t7 %>%
  select(-`Modeled max growth_2001`,
         -`Modeled max growth_2020`,
         -`Modeled max growth_2020/Modeled max growth_2001`)
  
names(t7clean) <- c("Region",
                    "Mean maximum fire growth rate in 2001 (ha/day)",
                    "Median maximum fire growth rate in 2001 (ha/day)",
                    "Mean maximum fire growth rate in 2020 (ha/day)",
                    "Median maximum fire growth rate in 2020 (ha/day)",
                    "Mean maximum fire growth rate in 2020 as a percentage of mean maximum fire growth rate in 2001 (%)",
                    "Median maximum fire growth rate in 2020 as a percentage of median maximum fire growth rate in 2001 (%)")

t7clean <- t7clean %>%
  mutate("Mean maximum fire growth rate in 2020 as a percentage of mean maximum fire growth rate in 2001 (%)" = `Mean maximum fire growth rate in 2020 as a percentage of mean maximum fire growth rate in 2001 (%)` * 100,
          "Median maximum fire growth rate in 2020 as a percentage of median maximum fire growth rate in 2001 (%)" = `Median maximum fire growth rate in 2020 as a percentage of median maximum fire growth rate in 2001 (%)` * 100) %>%
  mutate_if(is.double, ~ round(., digits = 1))

ft7 <- flextable(t7clean) %>%
  font(fontname = fontname, part = "all") %>%
  fontsize(size = fontsize, part = "all") %>%
  set_table_properties(width = 1, layout = "autofit")

ft7

print(ft7, preview = "docx")


