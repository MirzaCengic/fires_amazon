##########################################
#### Create a Visual Map for Fire Frequency in Amazon
##########################################
#### | Project name: Amazon fires
#### | Script type: Data processing and visualization
#### | What it does: This script processes geographical data to visualize the frequency of 
#### |               fires during August 2019 in the Amazon region, mapping them onto an image of South America.
#### | Date created: September 22, 2019.
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################

# Load required libraries individually
library(Rahat)
library(raster)
library(tidyverse)
library(extdplyr)
library(lubridate)
library(ggpubr)
library(gridExtra)
library(grid)
library(ggmap)
library(ggthemes)
library(sf)
library(mapview)
library(janitor)
library(tictoc)
library(ggsflabel)
library(fasterize)
library(patchwork)
library(glue)
library(cowplot)

# Load data ---------------------------------------------------------------
# Load south america country outlines and simplify for quicker calculations
samer_countries <- st_read("Data/samer_countries.gpkg") %>% 
  st_simplify()

amazon_biome <- st_read("Data_raw/amapoly_ivb/amapoly_ivb.shp")
NE_samer_small <- raster("Data/NE_samer_small.tif")
ifl_resampled_raster <- raster("Data/IFL_raster_resampled.tif")

# Define desired resolution (in degrees)
resl <- 0.5

fire_frequency_august_filename <- "Output/Fire_freqency_august.gpkg"
fire_frequency_august_lcifl_filename <- "Output/Fire_freqency_august_lcifl.gpkg"

# If the required file doesn't exist, process the data to create it
if (!file.exists(fire_frequency_august_lcifl_filename))
{
  # Load data for current fires
  viirs_current <- st_read("Data/VIIRS_historical_new.gpkg")
  # Load fire data for the month of August
  viirs_august <- viirs_current %>%
    filter(acq_date > "2019-07-31" & acq_date < "2019-09-01")
  
  
  my_mask <- aggregate(NE_samer_small, resl / res(NE_samer_small)[1])
  
  tic("Raster to poly")
  mask_1deg <- rasterToPolygons(my_mask) %>% 
    st_as_sf() %>% 
    transmute(
      vals = 1:n()
    )
  toc()
  
  current_viirs_lc_filename <- "Data/august19_viirs_lc_filename.gpkg"
  current_viirs_ifl_filename <- "Data/august19_viirs_ifl_filename.gpkg"
  current_viirs_ifl_lc_filename <- "Data/august19_viirs_ifl_lc_filename.gpkg"
  
  # Extracting necessary data for the map
  if (!exists("viirs_lc"))
  {
    if (!file.exists(current_viirs_ifl_lc_filename))
    {
      
      esa <- "ESA_landcover/TIFF/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif" %>%
        milkunize2("data") %>%
        raster()
      
      tic("Extracting")
      viirs_lc_ifl <- raster::extract(ifl_resampled_raster, viirs_august, na.rm = TRUE, sp = TRUE) %>% 
        st_as_sf() 
      toc()
      
      tic("Extracting")
      viirs_lc_both <- raster::extract(esa, viirs_lc_ifl, na.rm = TRUE, sp = TRUE) %>% 
        st_as_sf() 
      toc()
      
      # Process and classify the extracted data for forest
      viirs_lc <-
        viirs_lc_both %>% 
        rename(lc = ncol(.) - 1) %>% 
        mutate(
          is_forest = ifelse(IFL_raster_resampled == 1 | lc == 50, 1, 0),
          is_forest = ifelse(is.na(is_forest), 0, is_forest),
          is_esa = ifelse(lc == 50, 1, 0),
          is_ifl = ifelse(IFL_raster_resampled == 1, 1, 0),
          is_ifl = ifelse(is.na(is_ifl), 0, is_ifl)
        )
      
      st_write(viirs_lc, current_viirs_ifl_lc_filename, delete_layer = TRUE)
      
    } else {
      
      viirs_lc <- st_read(current_viirs_ifl_filename)
    }
    
  }
  # Compute fire frequency and other statistics
  fire_frequency <- st_join(mask_1deg, viirs_lc, join = st_intersects)
  fire_frequency_lc <- st_join(viirs_lc, mask_1deg, join = st_intersects)
  
  forest_fire_pct <- fire_frequency_lc %>% 
    filter(!is.na(vals)) %>%
    pct_routine(vals, is_forest) %>% 
    filter(is_forest == 1) %>% 
    mutate(pct = pct * 100) %>% 
    ungroup()
  
  grid_fire <- fire_frequency %>%
    group_by(vals) %>% 
    summarize(n = n()) %>% 
    # st_centroid() %>% 
    filter(n != 1)
  
  points_fire_proportion <- grid_fire %>% 
    left_join(forest_fire_pct, by = "vals") %>% 
    mutate(
      pct = ifelse(is.na(pct), 0, pct)
    ) %>% 
    st_centroid() %>% 
    mutate(
      pct_rcl = as.factor(case_when(
        pct <= 1 ~ 0,
        pct > 1 & pct <= 25 ~ 25,
        pct > 25 & pct <= 50 ~ 50,
        pct > 50 & pct <= 75 ~ 75,
        pct > 75 & pct <= 100 ~ 100,
        TRUE ~ 0
      ))
    )
  
  st_write(points_fire_proportion, fire_frequency_august_lcifl_filename) 
  
} else {
  points_fire_proportion <- st_read(fire_frequency_august_lcifl_filename) 
}

# Convert raster to ggplot-compatible format
NE_samer_small_ggg <- raster_to_gg(NE_samer_small)
NE_amazon_forest_ggg <- raster_to_gg(ifl_resampled_raster)

# Read and preprocess Amazon river data
amazon_network <- "amazon_basin.gpkg" %>%
  st_read() %>%
  mutate(num = c(1.5, 1)) # Set line width for rivers

# Create map --------------------------------------------------------------

# Create data sample to generate color scale
mydat <- points_fire_proportion %>% 
  st_set_geometry(NULL)  %>% 
  sample_n(100)

# Compute centroids for country labels and adjust coordinates
samer_centroids <- samer_countries %>% 
  st_centroid() %>% 
  transmute(
    name = NAME_ENGLISH,
    coordx = st_coordinates(.)[, 1],
    coordy = st_coordinates(.)[, 2],
    coord_x = case_when(
      name == "Brazil" ~ coordx + 0.5,
      name == "Colombia" ~ coordx + 3,
      name == "Ecuador" ~ coordx + 2.1,
      name == "Guyana" ~ coordx - 0.2,
      name == "Suriname" ~ coordx - 0.2,
      name == "Bolivia" ~ coordx + 1,
      name == "Peru" ~ coordx - 1,
      name == "French Guiana" ~ coordx + 0.2,
      name == "Venezuela" ~ coordx + 2,
      TRUE ~ coordx
    ),
    coord_y = case_when(
      name == "Brazil" ~ coordy - 9,
      name == "Colombia" ~ coordy - 1.4,
      name == "French Guiana" ~ coordy + 0,
      name == "Peru" ~ coordy + 5.8,
      name == "Guyana" ~ coordy + 1,
      name == "Ecuador" ~ coordy + 0.5,
      name == "Suriname" ~ coordy + 0,
      name == "Bolivia" ~ coordy - 3,
      name == "Venezuela" ~ coordy - 2,
      TRUE ~ coordy
    ),
    name = ifelse(name == "French Guiana", "French\nGuiana", as.character(name))
  )

# Create color scale
brks_scale <- levels(mydat$pct_rcl)
labels_scale <- str_c(rev(brks_scale), "%")
labels_scale2 <- c("76 - 100%", "51 - 75%", "26 - 50%", "1 - 25%", "0%")


colorscale <- c('#fc8d59','#ef6548','#d7301f','#b30000','#7f0000')


# Generate main fire density plot with various layers

plot_firedens <- ggplot() +
  # Plot background raster
  geom_tile(data = NE_samer_small_ggg, aes(x = x, y = y, fill = NE_samer_small),
            show.legend = FALSE) +
  scale_fill_gradient2(low = "grey60",
                       mid = "lightyellow2",
                       high = "grey40") + # Grey
  geom_tile(data = NE_amazon_forest_ggg, aes(x = x, y = y, fill = ifl_raster_resampled),
            alpha = 0.6, fill = "green4") +
  #### South america country borders    
  geom_sf(data = samer_countries, fill = NA,
          color = "grey20") +
  #### Amazon biome outline
  geom_sf(data = amazon_biome, fill = NA, alpha = 0.1, color = "grey10",
          size = 1) +
  geom_sf(data = amazon_network, size = c(0.75, 0.25), color = "blue2", alpha = 0.25) +
  #### Country name labels
  geom_sf_text(data = samer_centroids, aes(x = coord_x, 
                                           y = coord_y, 
                                           label = name),
               size = c(0, 7, 0, 6, 7, 6, 
                        5, 0, 0, 6, 5, 3,
                        5),
               alpha = 0.8,
               color = "grey20", family = "Helvetica-Narrow") +
  #### Fires proprtional circles (colors mapped)
  geom_sf(data = points_fire_proportion, aes(size = n, color = pct_rcl), alpha = 0.7,
          inherit.aes = FALSE) +
  scale_color_manual(
    # in manual scales, one has to define colors, well, manually
    # I can directly access them using viridis' magma-function
    values = colorscale,
    breaks = rev(brks_scale)) +
  # # IFL label
  annotate("text", x = -58.5, y = 8.5, hjust = 0, 
           label = "Intact Forest Landscapes",
           family = "ubuntu", size = 8, color = "green4") +
  annotate("curve", x = -55, y = 7.9, curvature = -0.3,
           xend = -61.7, yend = 3.2, color = "grey10",
           arrow = arrow(angle = 20, length = unit(4, "mm"), type = "closed")) +
  # Points size label
  annotate("text", x = -47.8, y = 2, hjust = 0,
           label = "Point size indicates\nnumber of fires",
           family = "ubuntu", size = 6, color = "grey10") +
  annotate("curve", x = -48, y = 2,
           xend = -53.75, yend = -5.35, color = "grey10",curvature = 0.42,
           arrow = arrow(angle = 20, length = unit(4, "mm"), type = "closed")) +
  # Amazon biome label
  annotate("text", x = -82, y = -18, hjust = 0,
           label = "Amazon biome region",
           family = "ubuntu", size = 7, color = "grey10") +
  annotate("curve", x = -72.1, y = -18.1,
           xend = -68.3, yend = -16, color = "grey10",curvature = 0.2,
           arrow = arrow(angle = 20, length = unit(4, "mm"), type = "closed")) +
  # labels
  labs(title = "Frequency of detected fires during August 2019",
       caption = "itsprettydata.com  |  Data source: VIIRS, IFL, ESA") +
  coord_sf(datum = NA, xlim = c(-81, -38), ylim = c(-19.5, 10)
  ) +
  theme_map(
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 32, 
                              family = "ubuntu",
                              color = "grey10",
                              face = "bold",
                              vjust = 1,
                              hjust = 0),
    plot.subtitle = element_text(size = 18,
                                 family = "mono",
                                 color = "grey30"),
    plot.caption = element_text(size = 16, family = "ubuntu", color = "grey40")
  )

# Save the fire density map
ggsave("Output/Figures/map_firedensity_august_river.en.png", plot_firedens,
       dpi = 360, width = 14, height = 10)



# Color legend ------------------------------------------------------------


colorscale_gg <- mydat %>% 
  # select(qq, pct) %>% head
  ggplot() +
  aes(x = 1, y = pct_rcl) +
  geom_tile(aes(fill = pct_rcl)) +
  theme(
    legend.margin=margin(c(1,5,5,5))
  ) +
  scale_fill_manual(
    values = colorscale,
    breaks = rev(brks_scale),
    labels = labels_scale2,
    drop = FALSE,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(5, units = "mm"),
      keywidth = unit(50 / length(labels), units = "mm"),
      title.position = "top",
      title = "Percentage of forests burnt",
      title.theme = element_text(size = 38, 
                                 # face = "italic",
                                 family = "AvantGarde"),
      # I shift the labels around, the should be placed 
      # exactly at the right end of each legend key
      title.hjust = 0.5,
      label.theme = element_text(size = 24, 
                                 # face = "italic",
                                 family = "AvantGarde"),
      # label.hjust = 1,
      nrow = 1,
      byrow = T,
      # also the guide needs to be reversed
      reverse = T,
      label.position = "bottom",
      NULL
    )
  ) +
  theme(
    plot.margin = margin(t = 10, r = 100, b = 10, l = 10, unit = "cm")
  )


colorscale_gg_legend <- ggpubr::get_legend(colorscale_gg)
ggdraw(colorscale_gg_legend)

ggsave("Output/Figures/legend_colorscale_map.png", colorscale_gg_legend,
       dpi = 300, width = 10, height = 3)


# Point legend ------------------------------------------------------------


#### Create legend for proportional points

pointscale_gg <- points_fire_proportion %>% 
  mutate(
    n_rcl = as.factor(case_when(
      n <= 200 ~ "1 - 200",
      n > 200 & n <= 400 ~ "200 - 400",
      n > 400 & n <= 600 ~ "400 - 600",
      n > 600 & n <= max(n) ~ "600 - 889",
      TRUE ~ as.character(n)
    )),
    n_num = case_when(
      n <= 10 ~ 10,
      n > 10 & n <= 100 ~ 100,
      n > 100 & n <= max(n) ~ 1000,
      TRUE ~ 0
    ),
    n_rcl = fct_relevel(n_rcl, levels = c("1 - 200", "200 - 400", "400 - 600", "600 - 889"))
  ) %>% 
  ggplot() +
  aes(x = 1, y = n_rcl) +
  geom_point(aes(size = n_rcl), color = "#b30000") +
  guides(size = guide_legend(
    direction = "horizontal",
    keyheight = unit(5, units = "mm"),
    title = "Number of fires",          
    title.position = 'top',
    title.hjust = 0.5,
    title.theme = element_text(size = 38, 
                               # face = "italic",
                               # hjust = 0,
                               family = "AvantGarde"),
    # I shift the labels around, the should be placed 
    # exactly at the right end of each legend key
    # title.hjust = 0.5,
    # title.vjust = -1,
    label.theme = element_text(size = 24, 
                               # face = "italic",
                               family = "AvantGarde"),
    label.position = "bottom",
    NULL
  )
  
  )

pointscale_gg_legend <- get_legend(pointscale_gg)
# plot.new()
# ggdraw(pointscale_gg_legend, xlim = c(0, 1))

ggsave("Output/Figures/legend_pointscale_map.png", pointscale_gg_legend,
       dpi = 300, width = 7, height = 3)


