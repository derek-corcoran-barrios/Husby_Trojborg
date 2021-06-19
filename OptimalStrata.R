library(sf)
library(raster)
library(tidyverse)
library(vegan)
library(janitor)

Husby <- readRDS("HusbyRaster.rds")

ForK <- as.data.frame(Husby) %>%
  mutate_all(scale) %>% 
  tibble::rowid_to_column(var = "ID") %>% 
  dplyr::filter_all(~!is.na(.x)) 

TestHusby <- cascadeKM(ForK[,-1],inf.gr = 2, sup.gr = 10, iter = 100)

plot(TestHusby)

Results <- TestHusby$results %>% t %>% 
  as.data.frame()

Results$Groups <- make_clean_names(rownames(Results))

Selected <- Results %>% 
  dplyr::filter(calinski == max(calinski)) %>% 
  pull(Groups)

Partition <- TestHusby$partition %>% 
  as.data.frame() %>% 
  clean_names() %>% 
  pull(Selected)

Groups <- Husby[[1]]

values(Groups)[ForK$ID] <- Partition  
values(Groups) <- ifelse(values(Groups) > length(unique(Partition)), NA, values(Groups))

plot(Groups, colNA = "black")

saveRDS(Groups, "GroupsHusby.rds")
saveRDS(TestHusby, "StrataHusby.rds")

Trojborg <- readRDS("TrojborgRaster.rds")

ForK <- as.data.frame(Trojborg) %>%
  mutate_all(scale) %>% 
  tibble::rowid_to_column(var = "ID") %>% 
  dplyr::filter_all(~!is.na(.x)) 

TestTrojborg <- cascadeKM(ForK[,-1],inf.gr = 2, sup.gr = 10, iter = 100)

plot(TestTrojborg)

Results <- TestTrojborg$results %>% t %>% 
  as.data.frame()

Results$Groups <- make_clean_names(rownames(Results))

Selected <- Results %>% 
  dplyr::filter(calinski == max(calinski)) %>% 
  pull(Groups)

Partition <- TestTrojborg$partition %>% 
  as.data.frame() %>% 
  clean_names() %>% 
  pull(Selected)

Groups <- Trojborg[[1]]

values(Groups)[ForK$ID] <- Partition  
values(Groups) <- ifelse(values(Groups) > length(unique(Partition)), NA, values(Groups))

plot(Groups, colNA = "black")

saveRDS(Groups, "GroupsTrojborg.rds")

saveRDS(TestTrojborg, "StrataTrojborg.rds")
