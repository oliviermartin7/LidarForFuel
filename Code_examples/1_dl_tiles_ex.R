library(stringr)
library(lidR)
library(future)

# Download from a list ----

rm(list = ls())

all_url <- read.delim("~/path_to_a_list_of_url_of_ALS_data")

options(timeout = 9999999999)
for (i in 1:length(all_url[, 1])) {
  tile_ID <- str_split(all_url[i, 1], "/")[[1]][8]
  download.file(url = all_url[i, 1], destfile = paste0("~pathtodestination", tile_ID), quiet = F, mode = "wb")
}

# Example with LiDAR HD grid crossed with a Departement map to create a list of LiDAR HD URL  ----


library(sf)
library(terra)
library(data.table)
library(ggplot2)
library(tidyterra)
library(terra)
library(lidR)
library(sf)
library(raster)
library(data.table)
library(ggthemes)
library(ggridges)
library(ggpubr)
library(ggpmisc)
library(tidyverse)
library(GGally)

# Load LiDAR HD grid
LiDARHD_Classe <- st_read("D:/LiDARandField_Data_Fuel/Data_LiDAR_IGN/grille/09_07_2024_classes/TA_diff_pkk_lidarhd_classe.shp")

# load a departement map
dep <- st_read("D:/PostDocFire_RES/Other_Shp/France/departements-20180101-shp/departements-20180101.shp")
# Transform the coordinate system in lambert p93 : EPSG:2154
dep <- st_transform(dep, "EPSG:2154")

# Select a departement: here Isere = 38
dep2dl <- dep[which(dep$code_insee == 38), ]

# Cross lidar grid with url with department to download
inter <- data.frame(st_intersects(LiDARHD_Classe, dep2dl))
df_inter <- data.frame(inter)

# Get the url and transform in table
ID_LIDARHD_tiles <- LiDARHD_Classe[unique(df_inter$row.id), ]
all_url <- data.table(ID_LIDARHD_tiles$url_telech)

options(timeout = 9999999999)
for (i in 1:length(all_url[, 1])) {
  tile_ID <- str_split(all_url[i, 1], "/")[[1]][8]
  download.file(url = as.character(all_url[i, 1]), destfile = paste0("~pathtodestination", tile_ID), quiet = F, mode = "wb")
}
