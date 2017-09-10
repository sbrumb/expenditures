library(tidyverse)
library(foreign)
library(maps)
library(tigris)
library(ggthemes)
library(viridis)
library(cowplot)
library(extrafont)
library(scales)
library(reshape)
library(stringr)
library(modelr)

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
