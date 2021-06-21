library(dplyr)
library(purrr)
library(ggplot2)
library(cowplot)
library(tidyverse)

HusbyRaster <- readRDS("HusbyRaster.rds")

HusbyRaster_DF <- HusbyRaster %>% as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  pivot_longer(twi:asiltnor)

Husby <- readRDS("~/Documents/Husby_Trojborg/Husby.rds")






HusbyRaster_DF %>% 
  mutate(name = case_when(name == "aclaynor" ~ "Clay",
                          name == "afsandno" ~ "Fine sand",
                          name == "agsandno" ~ "Coarse sand",
                          name == "asiltnor" ~ "Silt",
                          TRUE ~ name)) %>% 
  group_split(name) %>% 
  map(
    ~ggplot() + geom_sf(data = Husby) + 
      geom_raster(data = .x, aes(x = x, y = y, fill = value)) +
      theme_void() +
      scale_fill_viridis_c(name = "",na.value="transparent", option = "A") + 
      scale_x_continuous(breaks = c(8.13, 8.14 ,8.15)) +
      scale_y_continuous(breaks = c(56.29, 56.295, 56.3)) +
      labs(x = NULL, y = NULL) +
      facet_grid(~ name, labeller = function(x) label_value(x, multi_line = FALSE))
  ) %>% 
  plot_grid(plotlist = ., align = 'hv', ncol = 4)
