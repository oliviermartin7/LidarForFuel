library(lidR)
library(data.table)
library(ggplot2)
library(ggthemes)

?pixel_metrics

source("~/Documents/lidarForFuel/CBD_profiles/fCBDprofile_fuelmetrics.R")



las = readLAS("D:/placettes_tot_pretraite/44.laz")
test = pixel_metrics(las, fCBDprofile_fuelmetrics, 20)


catalog_pleiade = catalog(c("~/Documents/subsample_pretreated/1.laz", "~/Documents/subsample_pretreated/2.laz"))
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
opt_output_files(catalog_pleiade) <- "~/Documents/{ID}" # chemin vers le dossier de sortie des nouveau quadras



# catalog_apply(catalog_pleiade,Fuel_metrics_4_Map_FireEURisk)

pixel_metrics(catalog_pleiade,~fCBDprofile_fuelmetrics(X=X,Y=Y,Z=Z,Zref = Zref,Easting = Easting ,Northing = Northing,Elevation = Elevation ,LMA = LMA,threshold = 0.02,WD = 591 ,scanning_angle = T,limit_flyheight = 800,limit_N_points = 400,datatype = "Pixel",omega = 0.77,d=0.5,G = 0.5),res=20)

opt_select(catalog_pleiade)


Metric_20=pixel_metrics(las,~fCBDprofile_fuelmetrics(limit_flyheight = 500,X=X,Y=Y,Z=Z,Zref = Zref,Easting = Easting ,Northing = Northing,Elevation = Elevation ,LMA = LMA,threshold = 0.012,WD = 575 ,limit_N_points = 400,datatype = "Pixel",omega = 0.7,d=0.5,G = 0.5),res=20)



X = las$X
Y = las$Y
Z = las$Z
Zref = las$Zref
Easting = las$Easting 
Northing = las$Northing
Elevation = las$Elevation 
LMA = las$LMA

test = fCBDprofile_fuelmetrics(limit_flyheight = 500,X=X,Y=Y,Z=Z,Zref = Zref,Easting = Easting ,Northing = Northing,Elevation = Elevation ,LMA = LMA,threshold = 0.012,WD = 575 ,limit_N_points = 400,datatype = "Plot",omega = 0.7,d=0.5,G = 0.5)

Recouvrement = rep(NA,58)
test_2 = data.frame(test[[2]])
test_2 = cbind(test_2,Recouvrement)
test_2 = test_2[,c("H","CBD", "Recouvrement")]

df_recouvrement = data.frame(H = c(0.5,1,2,3,4,5,8),CBD = rep(NA,7), Recouvrement = c(40,50,50,10,10,30,60))

ggplot() +
  geom_line(aes(y  = test_2$CBD, x = test_2$H), lwd=2, col = "green") +
  geom_line(aes(y = df_recouvrement$Recouvrement/500, x = df_recouvrement$H), lwd=2, col = "lightgreen") +
  theme_few()+
  geom_hline(aes(yintercept = 0.05),col="gray66",show.legend = T,lwd=1.2)+
  geom_hline(aes(yintercept = 0.1),col="red",show.legend = T,lwd=1.2)+
  scale_y_continuous(name = "CBD", sec.axis = sec_axis(trans = ~ .*500, name = "Recouvrement"))+
  coord_flip()+
  xlim(c(0,NA))+
  # facet_wrap(~Threshold_value)+
  theme(axis.text.x=element_text(size=8))
