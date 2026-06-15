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


# If using LiDAR HD French national campaign you can download LiDAR HD tiles of interest with Jean-Mathieu Monnet lidarHD package 
# https://lidar.pages-forge.inrae.fr/lidarHD/


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

pad <- lidR::cloud_metrics(las_i, pad_metrics(z0 = 0, dz = 1, nlayers = 60,ground_margin = 0.1,use_cover =T ,deviation_days = 14  ))

## Make a table for input in ffuel metrics from output of pad_metrics (the positions of the PAD values from the pad_metrics output; vertical reolution; heights of the pad layers) ----

tab_4_input_fuelmetrics=parse_pad_heights(names(pad))


# Step 3: estimate fuel metrics ----

# below the FMA values are 0.25 for canopy and shrub corresponding to average values of the species in MArtin-Ducup et al 2025, but it can be changed according to the shrub and canopy species
plot_i_metrics=ffuelmetrics2(PADval = unlist(pad[tab_4_input_fuelmetrics$idx]),zval = tab_4_input_fuelmetrics$z_bottom,dz=1,FMAcan = 0.25,FMAshrub = 0.25)

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
pad <- lidR::cloud_metrics(las_i, pad_metrics(z0 = 0, dz = 1, nlayers = 60,ground_margin = 0.1,use_cover =T ,deviation_days = 14  ))
tab_4_input_fuelmetrics=parse_pad_heights(names(pad))
plot_i_metrics=ffuelmetrics2(PADval = unlist(pad[tab_4_input_fuelmetrics$idx]),zval = tab_4_input_fuelmetrics$z_bottom,dz=1,FMAcan = 0.25,FMAshrub = 0.25)
tab_metrics=rbind(tab_metrics,data.table(cbind(plotName=i,t(plot_i_metrics))),fill=TRUE)
plot_profile(PADval =unlist(pad[tab_4_input_fuelmetrics$idx]),Fuel_metrics = plot_i_metrics,plotname="My_forestplot",zval= tab_4_input_fuelmetrics$z_bottom)
}
dev.off()
# Stand (larger) scale mapping ----
plot_i=readLAS(here::here("inst/extdata/2_bas_lig.laz"))

## rasterize the pad profile ---- 

pad_rast <- lidR::pixel_metrics(las_i, pad_metrics(z0 = 0, dz = 1, nlayers = 60,ground_margin = 0.1,use_cover =T ,deviation_days = 14  ), res = 10)

## make a table for input in ffuel metrics from output of pad_metrics (the positions of the PAD values from the pad_metrics output; vertical reolution; heights of the pad layers) ----
tab_4_input_fuelmetrics=parse_pad_heights(names(pad_rast))


## estimate fuel metrics from the raster and return the results in a raster ----
rast_i_metrics=ffuelmetrics2(PADval = pad_rast[[tab_4_input_fuelmetrics$idx]],zval = tab_4_input_fuelmetrics$z_bottom,dz=1,FMAcan = 1,FMAshrub = 1)
plot(rast_i_metrics)

# Very large scale (multi-laz) mapping ----

?ffuelmetrics2
