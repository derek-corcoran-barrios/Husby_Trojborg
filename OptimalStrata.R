library(sf)
library(raster)
library(tidyverse)
library(vegan)

Husby <- readRDS("HusbyRaster.rds")

ForK <- as.data.frame(Husby) %>%
  mutate_all(scale) %>% 
  tibble::rowid_to_column(var = "ID") %>% 
  dplyr::filter_all(~!is.na(.x)) 

TestHusby <- cascadeKM(ForK[,-1],inf.gr = 2, sup.gr = 10, iter = 100)

plot(TestHusby)

saveRDS(TestHusby, "StrataHusby.rds")

Trojborg <- readRDS("TrojborgRaster.rds")

ForK <- as.data.frame(Trojborg) %>%
  mutate_all(scale) %>% 
  tibble::rowid_to_column(var = "ID") %>% 
  dplyr::filter_all(~!is.na(.x)) 

TestTrojborg <- cascadeKM(ForK[,-1],inf.gr = 2, sup.gr = 10, iter = 100)

plot(TestTrojborg)

saveRDS(TestTrojborg, "StrataTrojborg.rds")
