library(lidR)
library(data.table)
library(ggplot2)
library(ggthemes)
library(future)


catalog_pretreated= catalog("~/path_to_the_pretreated_lazfile")

# set the number of workers
plan(sequential)
plan(multisession, workers = 16L)

# Show progression  (8 hours for luberon => 400km²)
opt_progress(catalog_pretreated)=T

opt_stop_early(catalog_pretreated)=T
# Keep the size of the tiles (1km²)
opt_chunk_size(catalog_pretreated)=0

# Do not use a Buffer, no need and slows down the process
opt_chunk_buffer(catalog_pretreated) <- 0

opt_laz_compression(catalog_pretreated) = T

# Where to output the rasters
opt_output_files(catalog_pretreated) <- "~/path_to_the_output_folder/{ID}" # chemin vers le dossier de sortie des nouveau quadras

# Run BD profile on the catalog to get the rasters => example of parameters
pixel_metrics(catalog_pretreated,~fCBDprofile_fuelmetrics(X=X,Y=Y,Z=Z,Zref = Zref,Easting = Easting ,Northing = Northing,Elevation = Elevation ,LMA = LMA, WD=WD,
                                                       threshold = 0.02,
                                                       scanning_angle = T,
                                                       limit_flyheight = 800,
                                                       limit_N_points = 400,
                                                       datatype = "Pixel",
                                                       omega = 0.77,
                                                       d=0.5,
                                                       G = 0.5),
              res=20)


