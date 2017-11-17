# Tidyverse
library(tidyverse)
library(modelr)

# Visualization
library(viridis)
library(cowplot)
library(scales)
library(ggmap)
library(extrafont)

# Spatial analysis
library(maps)
library(maptools)
library(sp)
library(sf)
library(tigris)
library(tidycensus)

# Utility
library(stringr)
library(foreign)
library(data.table)

options(tigris_use_cache = TRUE)

theme_sb <- theme_void() +
  theme(
    axis.ticks = element_blank(),
    text = element_text(family = "Gill Sans MT", size = 10),
    axis.text = element_text(family = "Gill Sans MT", size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = .25, color = 'gray'),
    panel.background = element_blank(),
    plot.margin = unit(c(.1, .1, .1, .1), "in")
  )

theme_sbmap <- theme_void() +
  theme(text = element_text(family = "Gill Sans MT", size = 7),
        plot.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.key.width = unit(.1, "in"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        )
