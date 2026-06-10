




library(terra)
library(stringr)
library(lidR)
library(Rfast)
library(data.table)
library(plyr)
library(ggthemes)
library(sf)
library(stars)
library(lidarforfuel)
source("D:/git/R_package/LidarForFuel/WIP/fPAD.R")
source("D:/git/R_package/LidarForFuel/WIP/fRay_tracing.R")
source("D:/git/CartoNat/fuels/CanopyFuelMetricsAnalysis/computeLiDARMetrics_v1.R")
source("D:/git/CartoNat/fuels/TreeCoverMetric/fcoverraster.R")
source("D:/git/CartoNat/fuels/TreeCoverMetric/fcoverraster.R")
# get PAD from several methods plots several ----

## Portugal plot ----

ls=list.files("D:/Donnees_LiDAR_Temp/CartoNat/Plot_ONF/lidarhd/plots_preproc/")
BDD_field=fread("D:/Feux/Analysis_PAD/Placette_ONF_DFCI/transfer_11373206_files_7790e345/FD_LIDAR_DFCI_01_04_05_8413_48_42_67_33iefc_26_07_06_83_3340onf_baslig_16102025.csv")


plot_i=readLAS(paste0("D:/Donnees_LiDAR_Temp/CartoNat/Plot_ONF/lidarhd/plots_clip/",ls[1])) # mettre le plot + les trajs dans lepackage pour cet exemple
traj_i=st_read(paste0("D:/Donnees_LiDAR_Temp/CartoNat/Plot_ONF/lidarhd/plots_traj/0_42.gpkg"))
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

pad <- lidR::cloud_metrics(las_i, pad_metrics(z0 = 0, dz = 1, nlayers = 120,ground_margin = 0,use_cover =T))
pad_2 <- lidR::cloud_metrics(las_i, pad_metrics_2(z0 = 0, dz = 1, nlayers = 120,ground_margin = 0,use_cover =T))

# Stand (largeer) scale mapping ----
pad_rast <- lidR::pixel_metrics(las_i, pad_metrics(), res = 10)

Ni=unlist(pad)[grep("^Ni_", names(pad))]
N=unlist(pad)[grep("^N_", names(pad))]
PAD=unlist(pad)[grep("^PAD_", names(pad))]
PAD[PAD!=0]
fCBDprofile_fuelmetrics(las_i)[[2]]$Ni

a=parse_pad_heights(names(pad))



# Step 3: estimate fuel metrics ----

ffuelmetrics2(PADval = unlist(pad[a$idx]),zval = a$z_bottom,dz=1,FMAcan = 1,FMAshrub = 1)

computeLiDARMetrics(PADval = as.vector(PAD[PAD!=0]),zval = 1:23,dz=1,FMAcan = 1,FMAshrub = 1)

ffuelmetrics2_bis(PAD[a$idx],zval = a$z_bottom,dz=1,FMAcan = 1,FMAshrub = 1)


metrics_rast=ffuelmetrics2(pad_rast[[7:23]],zval = 1:17,dz=1,FMAcan = 1,FMAshrub = 1)


