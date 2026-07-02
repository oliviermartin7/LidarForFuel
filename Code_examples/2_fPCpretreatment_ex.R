library(tiff)
library(raster)
library(lidR)
library(data.table)
library(RANN)
library(terra)
library(sf)
library(future)



liste <- list.files("~/folder_to_raw_laz_file")
subs_catalog <- readALSLAScatalog(folder = paste0("~/folder_to_raw_laz_file", liste))

# set the number of workers
plan(sequential)
plan(multisession, workers = 12L) # ~6Go RAM/ per 500m tiles

# Show progression
opt_progress(subs_catalog) <- T
opt_stop_early(subs_catalog) <- F

# select the size of the tile
opt_chunk_size(subs_catalog) <- 500

# Do not use a Buffer, no need and slows down the process
opt_chunk_buffer(subs_catalog) <- 0

opt_laz_compression(subs_catalog) <- T

# Where to output the rasters
opt_output_files(subs_catalog) <- "~/output_for_the_pretreated_tiles/{ID}" # chemin vers le dossier de sortie des nouvelles dalles

# opt_restart(subs_catalog) = 11989 => restart to a specific tile number

# Run the function in parallel on the catalog. In the example below we provide a LMA map (same coordinate systems as the LiDAR data) but not that you can specify a generic LMA and WD value for the whole pixel or for canopy and understory
catalog_apply(subs_catalog, fPCpretreatment,
  LMA = 140,
  WD = 591,
  WD_bush = 591,
  LMA_bush = 140,
  Height_filter = 80
)
