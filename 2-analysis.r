# Get simple features for Census tracts in every state
states <- unique(fips_codes$state)[2:51]
tract_test <- get_acs(geography = "tract", variables = "B01003_001",state = "AL", geometry = TRUE)

for (state in states) {
  yay <- get_acs(geography = "tract", variables = "B01003_001", 
          state = state, geometry = TRUE)
  tract_test <- rbind(yay, tract_test)
}

transit <- read_csv("data/tod_database_download.csv") %>%
  mutate(year = as.factor(`Year Opened`),
         id = row_number())

usa <- map_data("state")
usa_map <- map("state", fill = TRUE)

IDs <- sapply(strsplit(usa_map$names, ":"), function(x) x[1])
usa_sp <- map2SpatialPolygons(usa_map, IDs = IDs)

transit_coords <- as.data.frame(cbind(transit$Longitude, transit$Latitude))
transit_sp <- SpatialPoints(coords = transit_coords)
transit_over <- over(transit_sp, usa_sp)
transit$state <- IDs[transit_over]

tracts_sp <- as(tract_test, "Spatial")
proj4string(transit_sp) <- proj4string(tracts_sp)

test <- over(transit_sp, tracts_sp)
transit$tract <- test$NAME

tod_map <- transit %>%
  ggplot() +
  geom_map(aes(map_id = region),
           map = usa, data = usa, fill = "gray90", color = "white", size = .25) +
  coord_map("albers", parameters = c(39, 45)) +
  geom_jitter(aes(x = Longitude, y = Latitude, color = as.factor(Agency)),
             cex = .1, show.legend = FALSE, width = .1, height = .1) +
  scale_color_viridis(discrete = TRUE, option = "plasma") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_sbmap +
  theme(plot.margin = unit(c(0, 0, 0, 0), units = "in"))

# ggsave(plot = tod_map, "~/dissertation/figures/tod.png", width = 6.5, height = 4)

dc_tracts <- tracts("DC")
dc_tracts <- st_as_sf(dc_tracts)
transit2 <- st_as_sf(x = transit, coords = c("Longitude", "Latitude"))

st_crs(transit2) <- st_crs(dc_tracts)
dc_bbox <- st_bbox(dc_tracts)

tod_map3 <- dc_tracts %>%
  ggplot() +
  geom_sf(fill = "gray90", color = "white", size = .2) +
  coord_sf(datum = NA) +
  geom_point(data = transit, aes(x = Longitude, y = Latitude, color = Agency), show.legend = FALSE, size = 0.5) +
  scale_x_continuous(limits = c(dc_bbox[["xmin"]], dc_bbox[["xmax"]])) +
  scale_y_continuous(limits = c(dc_bbox[["ymin"]], dc_bbox[["ymax"]])) +
  theme_sbmap

ggsave(plot = tod_map3, "~/dissertation/figures/tod3.png", width = 6.5, height = 4)

sld_source <- read.dbf("SmartLocationDb.dbf")
dc_sld_source <- sld_source %>% filter(SFIPS == 11) 

rm(sld_source)

dc_bg <- block_groups("DC") %>%
  fortify(region = 'GEOID')

dc_sld <- dc_sld_source %>%
  mutate(id = as.numeric(GEOID10)) %>%
  filter(TOTPOP10 > 0)

dc_map1 <- dc_bg %>%
  ggplot() +
  geom_map(map = dc_bg, fill = NA,
           aes(x = long, y = lat, map_id = id)) +
  geom_map(data = dc_sld, map = dc_bg,
           aes(fill = D5ar, map_id = GEOID10),
           color = "#ffffff", size = .15) +
  scale_fill_viridis(option = "plasma",
                     limits = c(0, max(dc_sld$D5ar)),
                     breaks = seq(0, max(dc_sld$D5ar), 100000),
                     labels = comma) +
  coord_map("polyconic") +
  labs(title = "Jobs Accessible in a 45-Minute Drive",
       fill = "Jobs") + 
  theme_sbmap

dc_map2 <- dc_bg %>%
  ggplot() +
  geom_map(map = dc_bg, fill = NA,
           aes(x = long, y = lat, map_id = id)) +
  geom_map(data = dc_sld, map = dc_bg,
           aes(fill = D5br, map_id = GEOID10),
           color = "#ffffff", size = .15) +
  scale_fill_viridis(option = "plasma",
                     limits = c(0, max(dc_sld$D5ar)),
                     breaks = seq(0, max(dc_sld$D5ar), 100000),
                     labels = comma) +
  coord_map("polyconic") +
  labs(title = "Jobs Accessible in a 45-Minute Transit Trip",
       fill = "Jobs") +
  theme_sbmap

dc_combined <- plot_grid(dc_map1, dc_map2) +
  theme_sbmap +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(plot.title = element_text(size = 12,
                                  margin = margin(b = 12, unit = "pt")),
        plot.caption = element_text(margin = margin(t = 0)),
        plot.margin = unit(c(.1, .1, .1, .1), "in"))

ggsave(plot = dc_combined, file = "plots/sld.pdf", width = 6.5, height = 3.5, device = cairo_pdf)
ggsave(plot = dc_combined, file = "plots/sld.png", width = 6.5, height = 3.5)
