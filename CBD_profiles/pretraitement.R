library(tiff)
library(raster)
library(lidR)
library(data.table)
library(RANN)
library(terra)
library(sf)
library(future)

rm(list=ls())

source("C:/Users/ateurnier/OneDrive - INRAE SharePoint SE/CDD/carto/fPCpretreatment.R")

LMA = raster("C:/Users/ateurnier/OneDrive - INRAE SharePoint SE/CDD/carto/LMA_MAP_France_Sud.tif")

subs_catalog=readALSLAScatalog(folder = "D:/subsample/")

# set the number of workers
plan(sequential)
plan(multisession, workers = 6L) # pour des dalles de 500m 8 coeur est bien => ~6Go par dalle

# Show progression  (8 hours for luberon => 400kmB2)
opt_progress(subs_catalog)=T
opt_stop_early(subs_catalog)=T

# select the zize of the tile
opt_chunk_size(subs_catalog)=500

# Do not use a Buffer, no need and slows down the process
opt_chunk_buffer(subs_catalog) <- 0

opt_laz_compression(subs_catalog) = T

# Where to output the rasters
opt_output_files(subs_catalog) <- "H:/REMOTE_SENSING/LiDAR/ALS/IGN/LiDAR_HD/11_66_83_84_13_04_05_subsample/{ID}" # chemin vers le dossier de sortie des nouveau quadras

catalog_apply(subs_catalog,fPCpretreatment,LMA="C:/Users/ateurnier/OneDrive - INRAE SharePoint SE/CDD/carto/LMA_MAP_France_Sud.tif")


