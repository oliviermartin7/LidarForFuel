library(terra)
library(sf)
library(stringr)
library(lidR)
library(Rfast)
library(data.table)
library(plyr)
library(ggthemes)
library(sf)
library(stars)
library(lidarforfuel)
library(data.table)
library(lidarHD)
library(here)
library(ggplot2)
library(yaml)

# If using LiDAR HD French national campaign you can download LiDAR HD tiles of interest with Jean-Mathieu Monnet lidarHD package
# https://lidar.pages-forge.inrae.fr/lidarHD/

# Load configuration from YAML. If you don't use a YAML you can replace the values in pad_params and ffuel_params with your own values below.

config <- yaml::read_yaml(here::here("inst/extdata/Config_example.yml")) # CAREFUL THIS JUST AN EXAMPLE OF THE PARAMETRIZATION IN CERTAIN CONTEXT (French LiDAR-HD campaign for 10 m resolution..)
pad_params <- config$pad_metrics_1m
ffuel_params <- config$ffuelmetrics2

# get PAD at plot or stand (raster) scale ----

## Portugal plot ----

# First we need trajectographies either you have or you can generate them with get_traj
# Careful we recommend a 500m buffer around the area of interest to generate trajectographies

plot_i=readLAS(here::here("inst/extdata/2_bas_lig.laz"))
traj_i=get_traj(plot_i)

# In this case we used already available trajectories that we generate with a 500m buffer around plot_i.

traj_i=st_read(here::here("inst/extdata/2_bas_lig.gpkg"))

# Below it is just to focus on a 20mx20m plot at the center of the laz
center_x=ext(plot_i)[1]+50
center_y=ext(plot_i)[3]+50
plot_i=clip_rectangle(plot_i,xleft = center_x-10,ybottom = center_y-10,xright = center_x+10,ytop = center_y+10)

# Step 1 pretreatment ----

las_i=fPCpretreatment(
  plot_i,
  classify = FALSE,
  exclude_classes = NULL,
  traj = traj_i,
  dtm = NULL
)

# Step 2: make the pad profile ----

## Plot scale ----

pad <- lidR::cloud_metrics(las_i, do.call(pad_metrics, c(list(z0 = 0), pad_params)))

## Make a table for input in ffuel metrics from output of pad_metrics (the positions of the PAD values from the pad_metrics output; vertical reolution; heights of the pad layers) ----

tab_4_input_fuelmetrics=parse_pad_heights(names(pad))


# Step 3: estimate fuel metrics ----

# below the FMA values are 0.25 for canopy and shrub corresponding to average values of the species in MArtin-Ducup et al 2025, but it can be changed according to the shrub and canopy species
plot_i_metrics=do.call(ffuelmetrics2, c(list(PADval = unlist(pad[tab_4_input_fuelmetrics$idx]), zval = tab_4_input_fuelmetrics$z_bottom), ffuel_params))

# Graphic representation of the vertical profile and the boundaries identified
plot_profile(PADval =unlist(pad[tab_4_input_fuelmetrics$idx]),Fuel_metrics = plot_i_metrics,plotname="My_forestplot",zval= tab_4_input_fuelmetrics$z_bottom)

# Example on a list of plot
ls_plot=str_split(list.files(here::here("inst/extdata/"),pattern=".laz"),".laz",simplify = T)[,1]

pdf(here::here("inst/extdata/plot_example.pdf")) # ajuste le chemin
tab_metrics=data.table()
for (i in ls_plot){
plot_i=readLAS(paste0(here::here("inst/extdata/"),"/",i,".laz"))
traj_i=st_read((paste0(here::here("inst/extdata/"),"/",i,".gpkg")))
center_x=ext(plot_i)[1]+50
center_y=ext(plot_i)[3]+50
plot_i=clip_rectangle(plot_i,xleft = center_x-10,ybottom = center_y-10,xright = center_x+10,ytop = center_y+10)
las_i=fPCpretreatment(plot_i, traj = traj_i)
pad <- lidR::cloud_metrics(las_i, do.call(pad_metrics, c(list(z0 = 0), pad_params)))
tab_4_input_fuelmetrics=parse_pad_heights(names(pad))
plot_i_metrics=do.call(ffuelmetrics2, c(list(PADval = unlist(pad[tab_4_input_fuelmetrics$idx]), zval = tab_4_input_fuelmetrics$z_bottom), ffuel_params))
tab_metrics=rbind(tab_metrics,data.table(cbind(plotName=i,t(plot_i_metrics))),fill=TRUE)
plot_profile(PADval =unlist(pad[tab_4_input_fuelmetrics$idx]),Fuel_metrics = plot_i_metrics,plotname="My_forestplot",zval= tab_4_input_fuelmetrics$z_bottom)
}
dev.off()

# Stand (larger) scale mapping ----
plot_i=readLAS(here::here("inst/extdata/2_bas_lig.laz"))

## rasterize the pad profile ----

pad_rast <- lidR::pixel_metrics(las_i, do.call(pad_metrics, c(list(z0 = 0), pad_params)), res = 10)

## make a table for input in ffuel metrics from output of pad_metrics (the positions of the PAD values from the pad_metrics output; vertical reolution; heights of the pad layers) ----
tab_4_input_fuelmetrics=parse_pad_heights(names(pad_rast))


## estimate fuel metrics from the raster and return the results in a raster ----
rast_i_metrics=do.call(ffuelmetrics2, c(list(PADval = pad_rast[[tab_4_input_fuelmetrics$idx]], zval = tab_4_input_fuelmetrics$z_bottom), ffuel_params))
plot(rast_i_metrics)

# Very large scale (multi-laz) mapping ----

##  Pretreatment (we assume here that the user has a trajectory for the whole zone) ----

### Option 1 : parallelization with catalog apply and a height normalization using ground points and tin algorithm (lidR). Note that this approach needs a buffer and is more time and memory consuming ----

traj=st_read("path_to_your_traj") # traj has to be in a single file

ctg=catalog("~pathtoyourrawdata")
plan(multisession, workers = 4)  # number of workers depend on your machine RAM
opt_progress(ctg) =TRUE
opt_chunk_buffer(ctg)   <- 20      # This buffer allow to avoid edge effect on nomalization
opt_chunk_size(ctg)     <- 500      # We do recommend 500 m tiles to avoid overloading the ram
opt_laz_compression(ctg) <- TRUE
opt_output_files(ctg)   <- paste0("path_to_output_folder_for_pretreated_tiles", "/{ID}_pretreated")  # pour sauvegarder les résultats par dalle
catalog_apply(ctg, fPCpretreatment,traj=traj)

### Option 2: parallelization with catalog apply and a height normalization using your own DTM (previously generated with lasR for example). No buffer needed ----

traj=st_read("path_to_your_traj.gpkg") # traj has to be in a single geometry file
DTM=rast("path_to_your_DTM.tif") # DTM has to be in a single raster file

ctg=catalog("~pathtoyourrawdata") # CAREFUL THE BUFFER HAS TO BE REMOVED TO AVOID OVERLAPPING ZONES
plan(multisession, workers = 4)  # number of workers depend on your machine RAM
opt_progress(ctg) =TRUE
opt_chunk_buffer(ctg)   <- 0      # No buffer because we provide the DTM
opt_chunk_size(ctg)     <- 500      # We do recommend 500 m tiles to avoid overloading the ram
opt_laz_compression(ctg) <- TRUE
opt_output_files(ctg)   <- paste0("path_to_output_folder_for_pretreated_tiles", "/{ID}_pretreated")  # pour sauvegarder les résultats par dalle
catalog_apply(ctg, fPCpretreatment,traj=traj,dtm=DTM)

## Step 2: make the pad profile ----

traj=st_read("path_to_your_traj.gpkg") # traj has to be in a single geometry file
DTM=rast("path_to_your_DTM.tif") # DTM has to be in a single raster file

ctg=catalog("path_to_your_pretreated_tiles") # CAREFUL THE BUFFER HAS TO BE REMOVED TO AVOID OVERLAPPING ZONES
plan(multisession, workers = 4)  # number of workers depend on your machine RAM
opt_progress(ctg) =TRUE
opt_chunk_buffer(ctg)   <- 0      # No buffer because we provide the DTM
opt_chunk_size(ctg)     <- 0      # We do recommend 500 m tiles to avoid overloading the ram
opt_output_files(ctg)   <- paste0("path_to_output_folder_for_PADraster", "/{ID}_pretreated")  # pour sauvegarder les résultats par dalle

lidR::pixel_metrics(
  ctg,
  do.call(lidarforfuel::pad_metrics, c(list(z0 = 0), pad_params)),
  res = 10
)

## Step 3: estimate fuel metrics from a list of raster and return the results in a folder ----

library(foreach)
library(doParallel)

pad_files <- list.files("path_to_PADraster_folder", full.names = TRUE)
metrics_dir <- "path_to_output_folder_for_fuelmetrics"
cl <- parallel::makeCluster(4)
doParallel::registerDoParallel(cl)

metric_files <- foreach(
  pad_file = pad_files,
  .packages = c("terra", "lidarforfuel"),
  .errorhandling = "stop"
) %dopar% {
  pad_rast <- terra::rast(pad_file)
  tab_4_input_fuelmetrics <- lidarforfuel::parse_pad_heights(names(pad_rast))

  rast_i_metrics <- do.call(
    lidarforfuel::ffuelmetrics2,
    c(
      list(
        PADval = pad_rast[[tab_4_input_fuelmetrics$idx]],
        zval = tab_4_input_fuelmetrics$z_bottom
      ),
      ffuel_params
    )
  )

  output_file <- file.path(
    metrics_dir,
    paste0(tools::file_path_sans_ext(basename(pad_file)), "_fuelmetrics.tif")
  )
  terra::writeRaster(rast_i_metrics, output_file, overwrite = TRUE)
}
