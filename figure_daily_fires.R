##########################################
#### Fires per day figures and Amazon states inset
##########################################
#### | Project name: Amazon fires
#### | Script type: Data processing and visualization
#### | What it does: This script processes fires data for the Amazon region to figure out what's happening. It filters, processes, 
#### |               and visualizes the fires within different states in Brazil 
#### |               for the months of July, August, and September 2019.
#### | Date created: October 13, 2019.
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################

# Load required packages
library(Rahat) # Personal package
library(tidyverse)
library(sf)
library(raster)
library(lubridate)
library(ggsci)
library(ggthemes)
library(ggsflabel)
library(here)

# Data Loading  -----------------------------------------------------------

# Load fires data 
fires_viirs_cleaned_df <- here("Data/fires_viirs_cleaned.rds") %>% 
  read_rds()

# Process fires data to include relevant columns
fires_viirs_cleaned <- fires_viirs_cleaned_df %>%
  transmute(
    acq_date,
    is_ind,
    is_amazon,
    is_pa = if_else(is.na(WDPAID), FALSE, TRUE),
    period = str_c(month(acq_date), "_", year(acq_date)),
    is_both = if_else(is_pa & is_ind, TRUE, FALSE)
  ) %>%
  filter(is_amazon == TRUE)

# Load Amazon biome geographic data
amazon_biome <- st_read("Data_raw/amapoly_ivb/amapoly_ivb.shp") %>% 
  st_make_valid()

# Load South American countries and simplify the polygons to speed up calculations
samer_countries <- st_read("Data/samer_countries.gpkg") %>% 
  st_simplify()

# Load data for Brazil states and compute their area
brazil_adm <- here("Data/Brazil_GADM.gpkg") %>% 
  st_read() %>% 
  mutate(area = units::set_units(st_area(.), "km^2"))

# Data wrangling ----------------------------------------------------------
# Filter fires data that occurred in July, August, September 2019 and return only for Brazil
fires_br <- fires_viirs_cleaned %>%
  filter(period %in% c("7_2019", "8_2019", "9_2019")) %>% 
  st_join(brazil_adm, join = st_intersects)

# Determine affected Brazilian states
amazon_states <- fires_br %>% 
  filter(!is.na(NAME_1)) %>% 
  pull(NAME_1) %>% 
  as.character() %>% 
  unique()


## Compute centroids (used for map labels)
# Compute centroids for the affected Brazilian states
brazil_centroids <- brazil_adm %>% 
  filter(NAME_1 %in% amazon_states) %>% 
  st_centroid() %>% 
  transmute(
    name = as.character(NAME_1),
    coordx = st_coordinates(.)[, 1],
    coordy = st_coordinates(.)[, 2],
    coord_x = case_when(
      name == "Acre" ~ coordx - 0.8,
      name == "Roraima" ~ coordx + 0.2,
      name == "Rondônia" ~ coordx + 0.3,
      TRUE ~ coordx
    ),
    coord_y = case_when(
      name == "Acre" ~ coordy + 0.5,
      name == "Amapá" ~ coordy + 0.2,
      name == "Mato Grosso" ~ coordy + 2.5,
      name == "Roraima" ~ coordy + 0,
      name == "Rondônia" ~ coordy - 0.7,
      TRUE ~ coordy
    )
  )
# Compute centroids for South American countries
samer_centroids <- samer_countries %>% 
  filter(ISO %notin% c("ARG", "URY", "CHL", "PRY")) %>%
  st_centroid() %>% 
  transmute(
    name = NAME_ENGLISH,
    coordx = st_coordinates(.)[, 1],
    coordy = st_coordinates(.)[, 2],
    coord_x = case_when(
      name == "Brazil" ~ coordx + 5,
      name == "Colombia" ~ coordx + 0.5,
      name == "Ecuador" ~ coordx + 2,
      name == "Guyana" ~ coordx - 0.2,
      name == "Suriname" ~ coordx - 0.2,
      name == "Bolivia" ~ coordx - 1,
      name == "Peru" ~ coordx - 1,
      name == "French Guiana" ~ coordx + 0.2,
      name == "Venezuela" ~ coordx + 2,
      TRUE ~ coordx
    ),
    coord_y = case_when(
      name == "Brazil" ~ coordy - 6,
      name == "Colombia" ~ coordy - 2,
      name == "French Guiana" ~ coordy + 0,
      name == "Peru" ~ coordy + 5,
      name == "Guyana" ~ coordy + 1.2,
      name == "Ecuador" ~ coordy + 0.5,
      name == "Suriname" ~ coordy + 0,
      name == "Bolivia" ~ coordy + 3,
      name == "Venezuela" ~ coordy - 2,
      TRUE ~ coordy
    ),
    name = ifelse(name == "French Guiana", "French\nGuiana", as.character(name))
  )

# Subset only relevant countries
samer_countries_subset <- samer_countries %>% 
  filter(ISO %notin% c("ARG", "URY", "CHL", "PRY")) %>%
  filter(ISO != "BRA") %>% 
  transmute(
    name = NAME_ENGLISH
  )

# Define color scales for plotting
scale_color_nejm()
mypal <- c(pal_nejm()(8), "#7E6148B2")
mypal2 <- mypal
mypal2[4] <- "#666666"

# Data Preparation for Visualization
brazil_cln <- brazil_adm %>% 
  transmute(name = NAME_1) 
brazil_cln_amazon <- brazil_cln %>% 
  filter(name %in% amazon_states)

# Plot --------------------------------------------------------------------

# Define plotting boundaries and prepare additional layers
my_bb <- samer_countries_subset %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_difference(amazon_biome)

samer_outline <- st_read("Data/samer_outline.gpkg")

# Create the main plot visualizing fires within Amazon states
# Plot states inset map 
plot_states <- ggplot() +
  geom_sf(data = samer_countries_subset, fill = "#20854EFF",
          color = "grey50", size = 0.5, alpha = 0.7) +
  geom_sf(data = brazil_cln_amazon, aes(fill = name),
          color = "grey30", size = 1, alpha = 0.7) +
  # scale_fill_manual(values = mypal3) +
  geom_sf(data = amazon_biome, fill = NA, 
          color = "grey10", size = 1.25) +
  #### Fade layer
  geom_sf(data = my_bb, alpha = 0.65, fill = "grey60") +
  #### State name labels
  geom_sf_text(data = brazil_centroids, aes(x = coord_x, 
                                            y = coord_y, 
                                            label = name),
               size = c(6, 5, 8, 4, 6,
                        8, 6, 5, 4),
               alpha = 0.8,
               color = "grey30", family = "Helvetica-Narrow") +
  #### Country name labels
  geom_sf_text(data = samer_centroids, aes(x = coord_x, 
                                           y = coord_y, 
                                           label = name),
               size = c(8, 7, 7, 6,
                        5, 6, 5, 5, 
                        5),
               alpha = 0.8,
               color = "grey30", family = "Helvetica-Narrow") +
  coord_sf(datum = NA, xlim = c(-80, -44.4), ylim = c(-17.7, 9)) +
  theme_map() +
  theme(
    legend.position = "below",
    legend.title = element_blank()
  )
# Save plot 
ggsave("Output/Figures/states_amazon.png", plot_states, 
       dpi = 300, width = 14, height = 10)


# Join the current fire data with Brazil states' data
current_viirs_brazil <- st_join(current_viirs, brazil_adm, join = st_intersects)

# Sort and filter states 
states_subset <- c("Acre", "Amazonas", "Mato Grosso", "Not Brazil", "Pará", "Rondônia")
amazon_states %>% sort()
mypal3 <- mypal2[c(1, 3, 5, 4, 6, 7, 2, 8, 9)]

# Plot per day ------------------------------------------------------------

# Group by date and calculate fire count for each day
fr_mn <- fires_viirs_cleaned %>% 
  mutate(
    period2 = str_c(day(acq_date),
                    "_",
                    month(acq_date),
                    "_",
                    year(acq_date)),
    mnt = month(acq_date)
    
  ) %>% 
  group_by(period2) %>% 
  mutate(
    n = n()
  )

# Calculate average value of fire frequency per day
avg_daily_value <- fr_mn %>% 
  st_set_geometry(NULL) %>% 
  filter(!str_detect(period, "2019")) %>%
  distinct(period2, .keep_all = T) %>% 
  group_by(mnt) %>% 
  summarize(
    mean = mean(n)
  )

# Data preparation for plotting
p_data <- fires_br %>% 
  mutate(
    state = ifelse(is.na(NAME_1), "Not Brazil", as.character(NAME_1)),
    state = as.character(state)
  ) %>% 
  # filter(state %notin% c(,"Amapá", "Roraima")) %>% 
  st_set_geometry(NULL) %>% 
  group_by(state) %>%
  mutate(
    n = n()
  ) %>% 
  ungroup() %>% 
  filter(n > 100)

p_data2 <- p_data %>% 
  group_by(acq_date) %>% 
  summarize(n = n())

# Determine the above-average fire frequency for each day
p_data3 <- p_data2 %>% 
  mutate(
    month = month(acq_date),
    nn = case_when(
      month == 7 & n > filter(avg_daily_value, mnt == 7)$mean ~ filter(avg_daily_value, mnt == 7)$mean,
      month == 8 & n > filter(avg_daily_value, mnt == 8)$mean ~ filter(avg_daily_value, mnt == 8)$mean,
      month == 9 & n > filter(avg_daily_value, mnt == 9)$mean ~ filter(avg_daily_value, mnt == 9)$mean,
      month == 10 & n > filter(avg_daily_value, mnt == 10)$mean ~ filter(avg_daily_value, mnt == 10)$mean,
      TRUE ~ as.numeric(n)
    )
  ) 

# Define plot theme
theme_pd_text <- theme(
  legend.title = element_blank(),
  plot.title = element_text(family = 'ubuntu', 
                            color = 'grey10', 
                            face = 'bold',
                            size = 32, 
                            vjust = 1,
                            hjust = 0),
  plot.subtitle = element_text(size = 18, 
                               family = "mono",
                               color = "grey30"),
  plot.caption = element_text(color = "grey40",
                              family = 'ubuntu', 
                              size = 14, 
                              vjust = 1),
  axis.text.y = element_text(family = "ubuntu", color = "grey10"),
  axis.text.x = element_text(size = 18, family = "ubuntu", color = "grey20")
) 

# Create fire frequency plot (english version)
Sys.setlocale("LC_TIME", "en_GB.UTF-8")   # Locale for my Linux version

plot_daily_base <- p_data2 %>% 
  ggplot() +
  aes(x = acq_date, y  = n) +
  geom_col(fill = "red") +
  geom_col(data = p_data3, aes(x = acq_date, y  = nn), fill = "grey50") +
  # July mean line
  geom_segment(x = as_date("2019-07-01"), y = filter(avg_daily_value, mnt == 7)$mean, 
               xend = as_date("2019-08-01"), yend = filter(avg_daily_value, mnt == 7)$mean,
               linetype="dashed", color = "red"
  ) +
  # August mean line
  geom_segment(x = as_date("2019-08-01"), y = filter(avg_daily_value, mnt == 8)$mean, 
               xend = as_date("2019-09-01"), yend = filter(avg_daily_value, mnt == 8)$mean,
               linetype="dashed", color = "red"
  ) +
  # September mean line
  geom_segment(x = as_date("2019-09-01"), y = filter(avg_daily_value, mnt == 9)$mean, 
               xend = as_date("2019-09-30"), yend = filter(avg_daily_value, mnt == 9)$mean,
               linetype="dashed", color = "red"
  ) +
  theme_minimal() +
  theme_pd_text +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.direction = "horizontal",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = c(24, 24, 24, 24, 21,
                                        24, 24, 24, 24, 24)),
    axis.title.x = element_blank(),
    axis.ticks.y = element_line(size = 2)
  )

lims <- as.POSIXct(strptime(c("2019-07-01", "2019-09-30"), 
                            format = "%Y-%m-%d")) %>% 
  as_date()

plot_daily <- plot_daily_base +
  # Month mean label
  annotate("text", x = as_date("2019-07-01"), y = 570, hjust = 0,
           label = "Monthly mean value based\non a 7 year average",
           family = "ubuntu", size = 8, color = "black") +
  annotate("curve", x = as_date("2019-07-17"), y = 570,
           xend = as_date("2019-07-18"), yend = 60, color = "black",
           curvature = 0.1,
           arrow = arrow(angle = 20, length = unit(4, "mm"), type = "closed")) +
  annotate("curve", x = as_date("2019-07-20"), y = 581,
           xend = as_date("2019-08-01"), yend = 255, color = "black",
           curvature = -0.2,
           arrow = arrow(angle = 20, length = unit(4, "mm"), type = "closed")) +
  annotate("curve", x = as_date("2019-07-20"), y = 610,
           xend = as_date("2019-09-01"), yend = 346, color = "black",
           curvature = -0.2,
           arrow = arrow(angle = 20, length = unit(4, "mm"), type = "closed")) +
  # Fire day layer
  annotate("text", x = as_date("2019-07-07"), y = 1370, hjust = 0,
           label = "\"Day of fire\"\nAugust 10th",
           family = "ubuntu", size = 10, color = "grey10") +
  annotate("curve", x = as_date("2019-07-14"), y = 1290,
           xend = as_date("2019-08-10"), yend = 1100, color = "grey10",
           curvature = 0.2,
           arrow = arrow(angle = 20, length = unit(4, "mm"), type = "closed")) +
  # Averages label
  annotate("text", x = as_date("2019-08-18"), y = 1420, hjust = 0,
           label = "Red part of bars show above-average\nfrequency of fires",
           family = "ubuntu", size = 10, color = "red") +
  scale_x_date(
    limits = lims,
    date_labels = "%e %B"
  ) +
  theme(
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 22),
    plot.title = element_text(size = 48),
    axis.title.y = element_text(size = 24, color = "grey10"),
    axis.ticks.x = element_line(),
    plot.caption = element_text(size = 24),
    panel.grid.major.y = element_line(size = 0.25, color = "grey80"),
    plot.subtitle = element_text(family = "ubuntu", size = 32)
  ) +
  labs(title = "Daily frequency of detected fires in the Amazon basin", y = "Total number of detected fires",
       caption = "itsprettydata.com  |  Data source: VIIRS",
       subtitle = "July - September 2019"
  ) +
  NULL



ggsave(
  here("Output/Figures/fires_daily.png"), 
  plot_daily,
  dpi = 300, width = 22, height = 12)



# bh. version plot  -----------------------------------------------------------
Sys.setlocale("LC_TIME", "bs_BA.utf8")   # set locale
plot_daily_ba <- plot_daily_base +
  # Month mean label
  annotate("text", x = as_date("2019-07-01"), y = 570, hjust = 0,
           label = "Srednja mjesečna vrijednost po\nsedmogodišnjem prosjeku",
           family = "ubuntu", size = 8, color = "black") +
  annotate("curve", x = as_date("2019-07-21"), y = 570,
           xend = as_date("2019-07-18"), yend = 60, color = "black",
           curvature = 0.1,
           arrow = arrow(angle = 20, length = unit(4, "mm"), type = "closed")) +
  annotate("curve", x = as_date("2019-07-23"), y = 581,
           xend = as_date("2019-08-01"), yend = 255, color = "black",
           curvature = -0.2,
           arrow = arrow(angle = 20, length = unit(4, "mm"), type = "closed")) +
  annotate("curve", x = as_date("2019-07-23"), y = 610,
           xend = as_date("2019-09-01"), yend = 346, color = "black",
           curvature = -0.2,
           arrow = arrow(angle = 20, length = unit(4, "mm"), type = "closed")) +
  # Fire day layer
  annotate("text", x = as_date("2019-07-07"), y = 1370, hjust = 0,
           label = "\"Dan požara\"\n10. august",
           family = "ubuntu", size = 8, color = "grey10") +
  annotate("curve", x = as_date("2019-07-14"), y = 1290,
           xend = as_date("2019-08-10"), yend = 1100, color = "grey10",
           curvature = 0.2,
           arrow = arrow(angle = 20, length = unit(4, "mm"), type = "closed")) +
  # Averages layer
  annotate("text", x = as_date("2019-08-18"), y = 1420, hjust = 0,
           label = "Crveni dijelovi stubova pokazuju nadprosječnu\nfrekvenciju požara",
           family = "ubuntu", size = 8, color = "red") +
  scale_x_date(
    limits = lims,
    date_labels = "%e %B"
  ) +
  theme(
    axis.text.x = element_text(size = 24),
    axis.title.y = element_text(size = 18),
    axis.ticks.x = element_line(),
    panel.grid.major.y = element_line(size = 0.25, color = "grey80"),
    plot.subtitle = element_text(family = "ubuntu", size = 18)
    
  ) +
  labs(title = "Dnevna frekvencija detektovanih požara u slivu Amazona",
       y = "Ukupan broj detektovanih požara",
       caption = "itsprettydata.com  |  Izvor podataka: VIIRS",
       subtitle = "Juli - Septembar 2019."
  ) +
  NULL


ggsave(
  here("Output/Figures/fires_daily.ba.png"), 
  plot_daily_ba,
  dpi = 300, width = 22, height = 12)