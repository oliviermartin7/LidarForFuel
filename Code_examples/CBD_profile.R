library(lidR)
library(data.table)
library(ggplot2)
library(ggthemes)
library(future)


catalog_pleiade = catalog("/VOlumes/LaCie/subample_pretreated/")

# set the number of workers
plan(sequential)
plan(multisession, workers = 8L)

# Show progression  (8 hours for luberon => 400km²)
opt_progress(catalog_pleiade)=T

opt_stop_early(catalog_pleiade)=T
# Keep the size of the tiles (1km²)
opt_chunk_size(catalog_pleiade)=0

# Do not use a Buffer, no need and slows down the process
opt_chunk_buffer(catalog_pleiade) <- 0

opt_laz_compression(catalog_pleiade) = T

# Where to output the rasters
opt_output_files(catalog_pleiade) <- "/Volumes/LaCie/CDB_Subsample/{ID}" # chemin vers le dossier de sortie des nouveau quadras

# catalog_apply(catalog_pleiade,Fuel_metrics_4_Map_FireEURisk)

pixel_metrics(catalog_pleiade,~fCBDprofile_fuelmetrics(X=X,Y=Y,Z=Z,Zref = Zref,Easting = Easting ,Northing = Northing,Elevation = Elevation ,LMA = LMA,threshold = 0.02,WD = 591 ,scanning_angle = T,limit_flyheight = 800,limit_N_points = 400,datatype = "Pixel",omega = 0.77,d=0.5,G = 0.5),res=20)

opt_select(catalog_pleiade)
