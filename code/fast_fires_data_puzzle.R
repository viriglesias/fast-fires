# A script to compile a full dataset of fires and information used in fast fires that might be interesting to students,
# and then subset and clean it for use in a data puzzle created by Jonathan Griffith at CIRES CEEE & Tyler McIntosh

#Tyler L. McIntosh, Earth Lab, 4/26/24

library(tidyverse)
library(here)

# Load up and prep data ----

data <- sf::st_read(here::here('data', 'ics209plus_fired_events_combined.gpkg'))

#Write out to add LC data via code from Max: https://code.earthengine.google.com/0934abb3f31379133382c10b6b1c2a4d
#sf::st_write(data |> select(FIRED_ID, ig_year) |> sf::st_transform(4326), here::here('data', 'ics209plus_fired_events_combined.shp'), append = FALSE)

lcTypes <- cbind(
  c(seq(1:17)),
  c("Conifer Forest",
    "Broadleaf Forest",
    "Conifer Forest",
    "Broadleaf Forest",
    "Mixed Forest",
    "Shrubland",
    "Shrubland",
    "Savanna",
    "Savanna",
    "Grassland",
    "Wetland",
    "Cropland",
    "Urban",
    "Cropland",
    "Snow and Ice",
    "Barren",
    "Water")
) |>
  as.data.frame() |>
  `names<-`(c("LC_Type1", "LC_Type")) |>
  dplyr::mutate(LC_Type1 = as.double(LC_Type1))

firedLc <- read_csv(here::here('data', 'FIRED_qc_MCD12Q1.csv'))

unique(firedLc$LC_Type1) #SOME OF THESE VALUES AREN'T INTEGERS, NOT SURE WHY... FROM MAX GEE SCRIPT

# Clean up the new LC data
firedLc <- firedLc |>
  dplyr::left_join(lcTypes, by = "LC_Type1") |>
  dplyr::filter(!is.na(LC_Type)) |> #####REMOVE THE FUNKY ONES
  dplyr::select(FIRED_ID, LC_Type)

# Join the data and create a slimmed-down version for students ----
dataJoinedFastFiresPaper <- data |>
  dplyr::left_join(firedLc, by = "FIRED_ID") |>
  dplyr::filter(!is.na(LC_Type)) |>
  dplyr::filter(STUSPS != "AK" & LC_Type != "Cropland")

dataSlim <- dataJoinedFastFiresPaper |>
  dplyr::mutate(maximum_growth_rate_acres = mx_grw_km2 * 247.105,
                log10_maximum_growth_rate_acres = log10(maximum_growth_rate_acres)) |>
  dplyr::select(START_YEAR,
                FINAL_ACRES,
                log10_maximum_growth_rate_acres,
                STR_DESTROYED_TOTAL,
                STR_THREATENED_MAX,
                PROJECTED_FINAL_IM_COST,
                LC_Type,
                POO_LONGITUDE,
                POO_LATITUDE) |>
  sf::st_drop_geometry() |>
  dplyr::mutate(STR_THREATENED_MAX = dplyr::case_when(is.na(STR_THREATENED_MAX) ~ 0,
                                                      TRUE ~ STR_THREATENED_MAX)) |>
  dplyr::rename(longitude = POO_LONGITUDE,
                latitude = POO_LATITUDE,
                year = START_YEAR,
                acres_burned = FINAL_ACRES,
                estimated_management_cost = PROJECTED_FINAL_IM_COST,
                land_cover = LC_Type,
                structures_threatened = STR_THREATENED_MAX,
                structures_destroyed = STR_DESTROYED_TOTAL) |>
  dplyr::filter(land_cover != "Water" & land_cover != "Urban" & land_cover != "Barren" & land_cover != "Wetland" & land_cover != "Snow and Ice")
readr::write_csv(dataSlim, here::here('data', 'simplified_ics209plus_fired_events_combined_for_data_puzzle.csv'))


# Write out some subsets of the data ----
dataSlim20102020 <- dataSlim |>
  dplyr::filter(year >= 2010)
readr::write_csv(dataSlim20102020, here::here('data', 'simplified_ics209plus_fired_events_combined_for_data_puzzle_2010_2020.csv'))

dataSlim20152020 <- dataSlim |>
  dplyr::filter(year >= 2015)
readr::write_csv(dataSlim20152020, here::here('data', 'simplified_ics209plus_fired_events_combined_for_data_puzzle_2015_2020.csv'))


dataSlim20122020 <- dataSlim |>
  dplyr::filter(year >= 2012) |>
  dplyr::filter(!is.na(estimated_management_cost))
readr::write_csv(dataSlim20122020, here::here('data', 'simplified_ics209plus_fired_events_combined_for_data_puzzle_2012_2020_cost.csv'))



dataSlimImpact <- dataSlim |>
  dplyr::filter(structures_destroyed >= 1 | structures_destroyed >= 1)

dplyr::filter(structures_destroyed >= 1 & !is.na(estimated_management_cost))

#assess data cleaning and removal from paper quality data -> student quality data ----
removedCost <- dataJoinedFastFiresPaper |>
  dplyr::filter(START_YEAR >= 2012) |>
  dplyr::filter(is.na(PROJECTED_FINAL_IM_COST))

removedLandcover <- dataJoinedFastFiresPaper |>
  dplyr::filter(START_YEAR >= 2012) |>
  dplyr::filter(LC_Type == "Water" | LC_Type == "Urban" | LC_Type == "Barren" | LC_Type == "Wetland" | LC_Type == "Snow and Ice")

nrow(dataSlim20122020) + nrow(removedCost) + nrow(removedLandcover)




#Some quick plots ----

ggplot(dataSlim) +
  geom_point(aes(x = maximum_growth_rate_acres, y = acres_burned))


ggplot(dataSlim) +
  geom_point(aes(x = maximum_growth_rate_acres, y = structures_destroyed)) +
  scale_y_continuous(trans = 'log2') +
  scale_x_continuous(trans = 'log2')

ggplot(dataSlim |> mutate(maximum_growth_rate_acres = log(maximum_growth_rate_acres),
                          structures_destroyed = log(structures_destroyed))) +
  geom_point(aes(x = maximum_growth_rate_acres, y = structures_destroyed))


ggplot(dataSlim |> mutate(maximum_growth_rate_acres = log(maximum_growth_rate_acres))) +
  geom_point(aes(x = maximum_growth_rate_acres, y = structures_destroyed)) +
  ylim(c(0, 2000))

ggplot(dataSlim20152020) +
  geom_point(aes(x = log_maximum_growth_rate_ac, y =maximum_growth_rate_acres))


t <- data |> dplyr::mutate(mgrHa = mx_grw_km2 * 100)

ggplot(t) +
  geom_point(aes(x = mgrHa, y = STR_DESTROYED_TOTAL)) +
  theme_classic(base_size = 10)  +
  geom_vline(aes(xintercept = 1620), linetype = 'dashed') +
  xlab('Maximum Single-Day Growth (ha/day)') +
  scale_x_continuous(trans = 'log',
                     breaks = c(1, 10, 100, 1000, 10000, 100000),
                     labels = c('1', '10', '100', '1,000', '10,000', '100,000')) +
  scale_y_continuous(trans = 'log',
                     breaks = c(1, 10, 100, 1000, 2000, 4000, 8000, 16000, 32000),
                     labels = c('1', '10', '100', '1,000', '2,000', '4,000', '8,000', '16,000', '32,000')) +
  ylab('Structures Damaged or Destroyed')
