
# Packages ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(extrafont)
loadfonts(device = "win")


# CRS business ------------------------------------------------------------

local_crs <- 28350 #GDA94 MGA50
WGS84_crs2 <- 4326


# Read in data ------------------------------------------------------------

roads_import <- read_sf("./sfiles/gis_osm_roads_free_1.shp")
water_import <- read_sf("./sfiles/coast_int.shp")
railways_import <- read_sf("./sfiles/gis_osm_railways_free_1.shp")
my_places <- read_sf("./sfiles/my_places.shp")

# Setup of border ---------------------------------------------------------

local_aoi_GDA50 <- read_sf("./sfiles/aoi_freo.shp")
local_aoi_ll <- st_transform(local_aoi_GDA50, crs = 4283)
local_aoi_WGS84 <- st_transform(local_aoi_GDA50, crs = 4326)

# Crop data ---------------------------------------------------------------

water_cropped <- st_crop(water_import, local_aoi_ll)
roads_cropped <- st_crop(roads_import, local_aoi_WGS84)
railways_cropped <- st_crop(railways_import, local_aoi_WGS84)


# Clean roads -------------------------------------------------------------

roads_cleaned <- roads_cropped %>% 
  filter(!(fclass  %in% c("steps", "footway", "living_street"))) %>%
  mutate(newclass = str_replace(fclass, "_link", ""),  
         newclass = if_else(newclass %in% c('trunk', 'primary', 'secondary', 'tertiary'), newclass, 'other'),
         newclass = factor(newclass, levels = rev(c('trunk', 'primary', 'secondary', 'tertiary', 'other')))) 



# Plot --------------------------------------------------------------------

#theme_set(theme_void())

blankbg <-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(), axis.title.y=element_blank(),
                panel.background=element_rect(fill = "#d1e9eb"),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank())

ggplot() + blankbg +
  # Plot water first, layer the rest on top
  geom_sf(data = water_cropped, size = 0.01, fill = "white") + #, fill = "#d1e9eb", size = 0.01) +
  ylim(c(32.092, 32.022)) +
  xlim(c(115.725, 115.784)) +
  geom_sf(data = railways_cropped, col = "grey60", size = 0.1) +
  # First plot the small streets, in lighter grey and a bit thiner
  geom_sf(
    data = roads_cleaned %>% filter(newclass == "other"),
    color = "grey50",
    size = 0.1
  ) +
  # Layer all major roads on top, a bit bolder, a bit darker
  geom_sf(
    data = roads_cleaned %>% filter(newclass != "other"),
    color = "grey40",
    size = 0.2
  ) +
  # Add favorite/memorable places
  geom_sf(
    data = my_places,
    aes(#size = Size,
      col = code),
    alpha = 0.8,
    size = 2
  ) +
  labs(caption = 'Our Freo') +
  scale_color_manual(values = c(
    "b" = "#FFCC00",
    "c" = "#663333",
    "d" = "#CC9933",
    "f" = "#CC3333",
    "h" = "#006633",
    "m" = "#33CC33",
    "v" = "#33FFFF"
  )) +
  theme(legend.position = "none",
        plot.caption = element_text(color = "grey20", 
                                    size = 142, 
                                    hjust = .5, 
                                    face = "plain", 
                                    family = "Centaur")
  )

ggsave("our_freo.png", width = 24, height = 36, units = "in",
       dpi = 500)
ggsave("map.png", width = 297, height = 420, units = "mm", dpi = "retina")
ggsave("map.svg")