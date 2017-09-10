dc_sld <- read.dbf("data/SmartLocationDb.dbf") %>%
  filter(SFIPS == 11)

dc_sld <- dc_sld %>%
  mutate(id = GEOID10)

dc_bg <- block_groups("DC")
dc_bg_fortify <- dc_bg %>%
  fortify(region = 'GEOID')

dc_merged <- left_join(dc_bg_fortify, dc_sld, by = "id")

dc_stamen <- get_map(location = "Washington DC", zoom = 12, maptype = "toner")

ggmap(dc_stamen) +
  geom_polygon(data = dc_merged, aes(x = long, y = lat, group = group,
                                    fill = D5ar), color = "white", size = .2,
               alpha = .7) +
  scale_fill_viridis(option = "plasma",
                     limits = c(0, max(dc_merged$D5ar)),
                     breaks = seq(0, max(dc_merged$D5ar), 100000),
                     labels = comma) +
  theme_sbmap

ggsave(plot = dc_combined, file = "plots/sld.png", width = 6.5, height = 3.5)
