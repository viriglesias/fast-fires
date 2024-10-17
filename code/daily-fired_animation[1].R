
# Libraries
library(tidyverse)
library(sf)
library(scales)
library(ggmap)
library(ggspatial)
library(gganimate)
library(gifski)
library(transformr)
library(grid)

getwd()

target_crs <- 3857 # web mercator

# Function to adjust map aspect ratio
adjust_aspect_ratio <- function(bbox, target_ratio = 16/9, min_buffer = 0.001) {
 
 # Calculate current width and height
 current_width <- bbox$xmax - bbox$xmin
 current_height <- bbox$ymax - bbox$ymin
 print(paste("Width:", current_width, "Height:", current_height))
 # Calculate current aspect ratio
 current_ratio <- current_width / current_height
 print(paste("Current Aspect Ratio:", current_ratio))
 
 # Adjust the bounding box to match the target aspect ratio
 if (current_ratio > target_ratio) {
  print("Current aspect ratio is wider than target. Adjusting height.")
  new_height <- current_width / target_ratio
  height_diff <- (new_height - current_height) / 2
  bbox$ymin <- bbox$ymin - height_diff
  bbox$ymax <- bbox$ymax + height_diff
 } else {
  print("Current aspect ratio is taller than target. Adjusting width.")
  new_width <- current_height * target_ratio
  width_diff <- (new_width - current_width) / 2
  bbox$xmin <- bbox$xmin - width_diff
  bbox$xmax <- bbox$xmax + width_diff
 }
 
 # Print adjusted bounding box for debugging
 print(paste("Adjusted BBox:", paste(bbox, collapse = ", ")))
 
 return(bbox)
}

# Data

# Daily polygons (fast fires)
fired.daily <- st_read('data/spatial/mod/fired_daily_overlap_fast.gpkg')
fired.daily$date <- as.Date(fired.daily$date)  # Ensure Date format
fired.daily <- st_transform(fired.daily, target_crs)  # Ensure consistent projection

# Create a list of FIRED IDs to plot
fire_ids = c(
 132111, # NW Oklahoma Complex, OK (2017)
 159406, # August Complex, CA (2020)
 132267, # Perryton Fire, TX (2017)
 159426, # North Complex, CA (2020)
 159618 # Cold Springs Complex, WA (2020)
)

# Names for titles
fire_names <- c(
 "NW Oklahoma Complex, OK (2017)",
 "August Complex Fire, CA (2020)",
 "Perryton Fire, TX (2017)",
 "North Complex, CA (2020)",
 "Cold Springs Complex, WA (2020)"
)

# Subset to testing fire (August Complex)
daily.gdf <- fired.daily %>% filter(FIRED_ID == fire_ids[1] & ig_year == 2017)
rm(fired.daily)
gc()

# Create the bounding box and adjusted aspect ratio
fire_bbox <- st_bbox(daily.gdf)
buffer_distance <- 1000  # Add X degree buffer around the fire data
# Create a new bounding box with a buffer around the fire
fire_bbox_buffered <- list(
 xmin = fire_bbox["xmin"] - buffer_distance,
 xmax = fire_bbox["xmax"] + buffer_distance,
 ymin = fire_bbox["ymin"] - buffer_distance,
 ymax = fire_bbox["ymax"] + buffer_distance
)

print(fire_bbox)
print(fire_bbox_buffered)

# Adjust the aspect ratio
fire_bbox_ratio <- adjust_aspect_ratio(fire_bbox_buffered)


###################
# Create static map

static <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 10) +   # OpenStreetMap tiles
  geom_sf(data = daily.gdf, aes(fill=event_day), color=NA, alpha=0.8) +
 
  # Set the extent based on the adjusted bounding box
  coord_sf(
   xlim = c(as.numeric(fire_bbox_ratio["xmin"]), as.numeric(fire_bbox_ratio["xmax"])),
   ylim = c(as.numeric(fire_bbox_ratio["ymin"]), as.numeric(fire_bbox_ratio["ymax"]))
  ) +
 
  scale_fill_viridis_c(option = "inferno", direction=-1) + 
  guides(fill = guide_colourbar(direction = "horizontal", 
                                barwidth = 18, barheight = 2.5, 
                                ticks.colour = NA, title.position = "top"),
         label.theme = element_text(angle = 0, size = 18),
         size="none") +
  ggspatial::annotation_scale(height=unit(0.90, "cm"), text_cex = 1.5) +
  labs(fill="Day of Burn") +
  geom_label(aes(x = Inf, y = Inf, label = fire_names[1]), 
             hjust = 2.5, vjust = 1.5, fill = alpha("black", 0.5), 
             color = "white", size = 14, fontface = "bold", 
             label.padding = unit(0.5, "lines")) +
  theme_void() +
  theme(
   legend.title = element_text(angle = 0, size=20, face = "italic"),
   legend.text = element_text(size=18),
   legend.position=c(0.10, 0.82),
   plot.title = element_text(size = 26, face = "bold", hjust = 0.5, vjust = 1),
   plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )
static

ggsave(static, file = "figures/animations/NWOklahomaComplex_static.png",
       width=8, height=6.5, dpi=300, bg="transparent") # adjust dpi accordingly

# Grab number of frames = burn dates
daily.gdf <- daily.gdf %>%
 mutate(event_day = as.integer(event_day))
ndays <- length(unique((daily.gdf$event_day))) * 2

# Animate it !
animap <- static +
 transition_time(event_day) +
 
 labs(caption = "Burn Day: {round(frame_time)}") + # dynamic label
 
 gganimate::enter_fade() +
 gganimate::shadow_mark(past = TRUE, alpha=0.5) +
 
 theme(
  plot.caption = element_text(
   size = 22, face = "bold", 
   color = "black", 
   hjust = 0.90, vjust = 12)
 )

# Create the animation
animation <- gganimate::animate(
  animap, nframes = ndays, fps=2,
  start_pause = 0, end_pause = 12,
  renderer = gifski_renderer(),
  width = 1920, height = 1080,
  bg = "transparent")
animation

# Save GIF file.
anim_save("figures/animations/NWOklahomaComplex_animation.gif")
