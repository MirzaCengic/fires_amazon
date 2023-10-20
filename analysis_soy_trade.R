##########################################
####  Soya Trade Analysis
##########################################
#### | Project name: My research project
#### | Script type: Data processing & Visualization
#### | What it does: Processes and visualizes the Soya trade data for Brazilian ports 
#### |               including the volume of exports and their destination countries.
#### | Date created: October 03, 2019.
#### | Creator: Mirza Cengic
#### | Contact: mirzaceng@gmail.com
##########################################

# Load required packages --------------------------------------------------
library(Rahat)
library(tidyverse)
library(raster)
library(ggmap)
library(sf)
library(mapview)
library(ggthemes)
library(ggsflabel)

# Register Google key for geocoding
register_google(key = "...")

# Load Data ----------------------------------------------------------------

# Load Amazon river network data
amazon_network <- "amazon_basin.gpkg" %>% 
  st_read() %>% 
  mutate(
    num = c(2, 1)
  )

# Load Amazon biome limits
amazon_biome <- st_read("Data_raw/amapoly_ivb/amapoly_ivb.shp")

# Load North-South ports data
# Data is taken from ABIOVE
data_ports_raw <- data.frame(stringsAsFactors=FALSE,
                             NAPorto = c("São Luís", "Barcarena", "Salvador", "Santarém", "Manaus",
                                         "Outros - Arco Norte", "Santos", "Paranaguá", "Rio Grande",
                                         "São Fco. do Sul", "Vitória", "Outros - Arco Sul"),
                             NApart = c("N", "N", "S", "N", "N", "N", "S", "S", "S", "S", "S", "S"), 
                             NA2015 = c(5004499, 2185381, 2693166, 1027239, 1653273, 189811, 13031789,
                                        8518898, 11372732, 4614864, 3771931, 259649),
                             NA2016 = c(3850196, 2187261, 1407801, 1695169, 1974313, 154776, 14475763,
                                        8157251, 9704071, 3961713, 2944967, 1068594),
                             NA2017 = c(6127570, 4462686, 3168533, 1877759, 2137667, 325549, 16589640,
                                        11349446, 12549977, 4718238, 3850616, 997127),
                             NA2018 = c(8202237, 5495124, 3677828, 2486027, 2480060, 797034, 20785873,
                                        14927423, 13816017, 5636526, 4256379, 1044681),
                             NA2019 = c(6188122, 5138132, 1736148, 2779017, 2654643, 66666, 16230657,
                                        8104348, 7593553, 2837864, 2719426, 784931)
) 
# Clean up ports data
data_ports <- data_ports_raw %>% 
  gather("year", "cases", 3:7) %>% 
  transmute(
    port = NAPorto,
    part = as.factor(NApart),
    year = str_remove(year, "NA"),
    tons = cases,
    partt = ifelse(part == "N", "Northern", "Southern") %>% fct_rev(),
    part = fct_rev(part)
  )

# Load and process export data
# Total export to other countries

data_export_raw <- data.frame(stringsAsFactors=FALSE,
                              Destino = c("China", "União Europeia", "Ásia (exceto China)",
                                          "Outros Destinos"),
                              y2015 = c(40925507, 5619776, 4834984, 2943971),
                              y2016 = c(38563909, 5279870, 4336048, 3402048),
                              y2017 = c(53796980, 5191076, 5275129, 3891624),
                              y2018 = c(68839903, 5096943, 3650388, 6017973),
                              y2019 = c(42788683, 4809346, 4025270, 5210208)
)


data_export <- data_export_raw %>% 
  gather("year", "tons", 2:6) %>% 
  transmute(
    dest = Destino,
    year = str_remove(year, "y"),
    tons
  )

# Load South American countries data and simplify
samer_countries <- st_read("Data/samer_countries.gpkg") %>% 
  st_simplify()

port_location_name <- "Data/port_locations.gpkg"

# Check for existing port locations and geocode if necessary

port_location_name <- "Data/port_locations.gpkg"

if (!file.exists(port_location_name))
{
  # Geocode -----------------------------------------------------------------
  port_names <- data_ports %>% 
    filter(!str_detect(port, "Outros")) %>% 
    mutate(
      ports = str_c(port, " Brazil")
    ) %>% 
    pull(ports) %>% 
    unique() 
  # Check coodinates of ports - geocoding seems stohastic so needs checking
  locations <- geocode(port_names, output = "more")
  #
  locations_ports <- locations %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # Combine with port data
  data_ports_part <- data_ports %>% 
    filter(!str_detect(port, "Outros")) %>% 
    distinct(port, .keep_all = TRUE) %>% 
    dplyr::select(port, part)
  
  
  locations_ports_cln <- locations_ports %>%
    mutate(
      port = data_ports_part$port,
      part = data_ports_part$part
    ) 
  
  locations_ports_cln %>%
    mapview(zcol = "part")
  
  st_write(locations_ports_cln, port_location_name)
  
} else {
  locations_ports_cln <- st_read(port_location_name)
}
# Map preparation and visualization ---------------------------------------

# Adjustments for map visualization (used for line width)
amazon_network_aa <- amazon_network %>% 
  mutate(
    num = c(1.5, 1)
  )


brazil_adm <- samer_countries %>% 
  filter(ISO == "BRA")
amazon_network <- "amazon_basin.gpkg" %>%
  st_read() %>%
  mutate(
    num = c(1.5, 1)
  )

amazon_network_crop <- st_intersection(amazon_network, brazil_adm)
# Adjust labels for port locations on the map

locations_ports_cln_mod <- locations_ports_cln %>% 
  mutate(
    labx = case_when(
      port == "São Luís" ~ west + 5.5,
      port == "Barcarena" ~ west + 6.8,
      port == "Salvador" ~ west - 4,
      port == "Santarém" ~ west + 2,
      port == "Manaus" ~ west - 4,
      port == "Santos" ~ west,
      port == "Paranaguá" ~ west - 8,
      port == "Rio Grande" ~ west + 14.5,
      port == "São Fco. do Sul" ~ west + 5,
      port == "Vitória" ~ west - 3
    ),
    laby = case_when(
      port == "São Luís" ~ south - 2,
      port == "Barcarena" ~ south + 2.5,
      port == "Salvador" ~ south + 2.5,
      port == "Santarém" ~ south - 2.5,
      port == "Manaus" ~ south + 2.2,
      port == "Santos" ~ south + 2.5,
      port == "Paranaguá" ~ south + 1.75,
      port == "Rio Grande" ~ south + 1.5,
      port == "São Fco. do Sul" ~ south - 1.5,
      port == "Vitória" ~ south + 2.5)
  )

# Create the main map visual

p1 <- ggplot() +
  ## South america country borders
  geom_sf(data = amazon_network_crop, size = c(1.5, 0.5), color = "blue2", alpha = 0.25) +
  geom_sf(data = brazil_adm, fill = NA,
          color = "grey35", size = 1.25) +
  geom_sf(data = locations_ports_cln, aes(color = part), size = 20) +
  scale_color_manual(values = c("orange", "grey70")) +
 
  geom_sf_text(data = locations_ports_cln_mod, 
               aes(
                 x = labx,
                 y = laby,
                 label = port),
               # nudge_x = 6,
               # nudge_y = 2,
               size = 20,
               alpha = 0.9,
               color = "grey10", family = "Helvetica-Narrow"
  ) +
  coord_sf(datum = NA, xlim = c(-73, -35), ylim = c(-32, 4)) +
  theme_map() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      # size = 22, 
      family = "ubuntu",
      color = "grey10",
      face = "bold",
      size = 32, 
      vjust = 1,
      hjust = 0),
    plot.subtitle = element_text(size = 18, 
                                 family = "ubuntu",
                                 color = "grey30"),
    plot.caption = element_text(size = 10, color = "grey40")
  )

# Save map to file
ggsave("Output/Figures/ports_map.png", p1, dpi = 200, width = 16, height = 12)


# Bar plot visualization for Ports
p_ports <- data_ports %>% 
  mutate(
    mtons = tons / 1000000
  ) %>% 
  group_by(year, partt) %>% 
  summarise(
    tot = sum(mtons)
  ) %>% 
  ungroup() %>% 
  %>% 
  ggplot() +
  aes(x = year, y = tot, fill = partt) +
  geom_col(position = "dodge") +
  geom_text(data = filter(aa, partt == "Southern", year == 2019), 
            aes(x = year, y = tot  + 2, label = round(tot, 1)),
            hjust = 1.75,
            color = "grey10",
            family = "ubuntu",
            size = 12
  ) +
  geom_text(data = filter(aa, partt == "Southern", year != 2019), 
            aes(x = year, y = tot  + 2, label = round(tot, 1)),
            hjust = 1.25,
            color = "grey10",
            family = "ubuntu",
            size = 12
  ) +
  geom_text(data = filter(aa, partt == "Northern", year != 2016), 
            aes(x = year, y = tot  + 2, label = round(tot, 1)),
            hjust = -0.2,
            color = "grey10",
            family = "ubuntu",
            size = 12
  ) +
  geom_text(data = filter(aa, partt == "Northern", year == 2016), 
            aes(x = year, y = tot  + 2, label = round(tot, 1)),
            hjust = -0.5,
            color = "grey10",
            family = "ubuntu",
            size = 12
  ) +  

  labs(y = "Million tons", title = "Export of soya from Brazilian ports",
       caption = "itsprettydata.com | Data source: ABIOVE"
  ) +
  annotate(
    "text", label = "Data only until\nAugust 2019", x = "2019",
    y = 11, hjust = 0.5, family = "Helvetica-Narrow", size = 7,
    color = "grey20"
  ) +
  scale_fill_manual(values = c("grey70", "orange"), labels = c("  Atlantic ports  ", " Amazon ports ")) +

  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(size = 36, 
                              family = "ubuntu",
                              color = "grey10",
                              face = "bold",
                              vjust = 1,
                              hjust = 0),
    plot.subtitle = element_text(family = "mono", 
                                 size = 16, 
                                 color = "grey30"),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(color = "grey40", family = 'ubuntu', vjust = 1, size = 16),
    legend.text = element_text(family = "AvantGarde", size = 32, color = "grey10"),
    legend.key.size = unit(1.5, "cm"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(family = "ubuntu", size = 30, color = "grey10"),
    axis.text = element_text(family = "ubuntu", size = 22, color = "grey30"),
    axis.text.x = element_text(size = 26)
  )

ggsave("Output/Figures/ports_export2.png", p_ports,
       dpi = 200, width = 16,
       height = 12)
