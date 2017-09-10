# Tidyverse
library(tidyverse)
library(modelr)

# Visualization
library(ggthemes)
library(viridis)
library(cowplot)
library(scales)
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

options(tigris_use_cache = TRUE)
census_api_key('31b071508a01c82ee7be49c0dd16a45a6c2689e6', install = TRUE)

theme_sb <- theme_void() +
  theme(
    axis.ticks = element_blank(),
    text = element_text(family = "Lato", size = 10),
    axis.text = element_text(family = "Lato", size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = .25, color = 'gray'),
    panel.background = element_blank(),
    plot.margin = unit(c(.1, .1, .1, .1), "in")
  )

theme_sbmap <- theme_void() +
  theme(text = element_text(family = "Lato", size = 8),
        plot.title = element_text(size = 10),
        legend.title = element_text(size = 8),
        legend.key.width = unit(.1, "in"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        )
