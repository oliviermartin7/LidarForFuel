library(tiff)
library(raster)
library(lidR)
library(data.table)
library(RANN)
library(terra)
library(sf)
library(future)

rm(list=ls())

source("~/Documents/lidarForFuel/CBD_profiles/fPCpretreatment.R")

liste = list.files("~/Documents/subsample")
subs_catalog=readALSLAScatalog(folder = paste0("~/Documents/subsample/", liste))

# set the number of workers
plan(sequential)
# plan(multisession, workers = 15L) # pour des dalles de 500m 8 coeur est bien => ~6Go par dalle

# Show progression  (8 hours for luberon => 400kmB2)
opt_progress(subs_catalog)=T
opt_stop_early(subs_catalog)=F

# select the zize of the tile
opt_chunk_size(subs_catalog)=500

# Do not use a Buffer, no need and slows down the process
opt_chunk_buffer(subs_catalog) <- 0

opt_laz_compression(subs_catalog) = T

# Where to output the rasters
opt_output_files(subs_catalog) <- "~/Documents/subsample_pretreated/{ID}" # chemin vers le dossier de sortie des nouveau quadras

opt_restart(subs_catalog) = 11920

catalog_apply(subs_catalog,fPCpretreatment,LMA="~/Documents/CDD/carto/LMA_MAP_France_Sud.tif", norm_ground = FALSE)


