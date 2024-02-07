library(lidR)
library(terra)
library(data.table)
library(dbscan)
library(sf)
library(stars)
library(future)
catalog_Uchaux=catalog("D:/LiDARandField_Data_Fuel/Data_LiDAR_IGN/OLD/Uchaux/Treated_4_CBD_new/")
crs(catalog_Uchaux)="epsg:2154"

getpolybushandtree=function(las){
  # read chunk
  # las <- readLAS(chunk)
  if (is.empty(las)) return(NULL)
  
clip_zone_bush=filter_poi(las,Z<=3)
chm_bush <- rasterize_canopy(clip_zone_bush, res = 0.3, p2r())
chm_bush <- app(chm_bush, fun=function(x){ 
x[x <= 3&x>=0.2] <- 1
x[x < 0.2] <- NA
x[x > 3] <- NA
; return(x)} )


poly_bush=st_as_stars(chm_bush)
poly_bush=st_as_sf(poly_bush,as_point=F,merge=T)
poly_bush$area=st_area(poly_bush)
poly_bush[which(as.numeric(poly_bush$area)<0.18),]



clip_zone_trees=filter_poi(las,Z>3)
chm_trees <- rasterize_canopy(clip_zone_trees, res = 0.3, p2r())
chm_trees <- app(chm_trees, fun=function(x){ 
  x[x > 3] <- 1
  ; return(x)} )



poly_trees=st_as_stars(chm_trees)
poly_trees=st_as_sf(poly_trees,as_point=F,merge=T)
poly_trees$area=st_area(poly_trees)

poly_trees$type="trees"
poly_bush$type="bush"

poly=rbind(poly_trees,poly_bush)
poly=poly[which(as.numeric(poly$area)>0.36),]

return(poly)
}


# run on a zone  ----

Route_de_Mornas=clip_rectangle(catalog_Uchaux,xleft = 842627.4-50,ybottom = 6347872-50,xright =843227.7+50,ytop =6348472+50 )


shp_Route_de_Mornas=getpolybushandtree(Route_de_Mornas)
mapview::mapview(shp_Route_de_Mornas[which(as.numeric(shp_Route_de_Mornas$area)>2&shp_Route_de_Mornas$type=="trees"),2],legend=T,layer.name="Cluster arbre (surface m²)")
mapview::mapview(shp_Route_de_Mornas[which(as.numeric(shp_Route_de_Mornas$area)>2&shp_Route_de_Mornas$type=="bush"),2],legend=T,layer.name="Bush arbre (surface m²)")

  st_write(shp_Route_de_Mornas,"D:/LiDARandField_Data_Fuel/Data_LiDAR_IGN/OLD/Uchaux/Shape_Zone_a_visiter/shp_Route_de_Mornas.shp")

Chemin_Saint_Michel=clip_rectangle(catalog_Uchaux,xleft = 843413-50,ybottom = 6347884.24-50,xright =843816+50,ytop =6348302.42+50 )


shp_Chemin_Saint_Michel=getpolybushandtree(Chemin_Saint_Michel)
mapview::mapview(shp_Chemin_Saint_Michel[which(as.numeric(shp_Chemin_Saint_Michel$area)>2&shp_Chemin_Saint_Michel$type=="trees"),2],legend=T,layer.name="Cluster arbre (surface m²)")
mapview::mapview(shp_Chemin_Saint_Michel[which(as.numeric(shp_Chemin_Saint_Michel$area)>2&shp_Chemin_Saint_Michel$type=="bush"),2],legend=T,layer.name="Bush arbre (surface m²)")

st_write(shp_Chemin_Saint_Michel,"D:/LiDARandField_Data_Fuel/Data_LiDAR_IGN/OLD/Uchaux/Shape_Zone_a_visiter/shp_Chemin_Saint_Michel.shp")

Route_de_Serignan=clip_rectangle(catalog_Uchaux,xleft = 844024.83-50,ybottom = 6346037.80-50,xright =844435.63+50,ytop =6346521.93+50 )

shp_Route_de_Serignan=getpolybushandtree(Route_de_Serignan)
mapview::mapview(shp_Route_de_Serignan[which(as.numeric(shp_Route_de_Serignan$area)>2&shp_Route_de_Serignan$type=="trees"),2],legend=T,layer.name="Cluster arbre (surface m²)")
mapview::mapview(shp_Route_de_Serignan[which(as.numeric(shp_Route_de_Serignan$area)>2&shp_Route_de_Serignan$type=="bush"),2],legend=T,layer.name="Bush arbre (surface m²)")

st_write(shp_Route_de_Serignan,"D:/LiDARandField_Data_Fuel/Data_LiDAR_IGN/OLD/Uchaux/Shape_Zone_a_visiter/shp_Route_de_Serignan.shp")


Chemin_de_Rocquecourbe=clip_rectangle(catalog_Uchaux,xleft = 843547.33-50,ybottom = 6346299.34-50,xright =843717.77, +50,ytop =6346521.93+50 )

shp_Chemin_de_Rocquecourbe=getpolybushandtree(Chemin_de_Rocquecourbe)
mapview::mapview(shp_Chemin_de_Rocquecourbe[which(as.numeric(shp_Chemin_de_Rocquecourbe$area)>2&shp_Chemin_de_Rocquecourbe$type=="trees"),2],legend=T,layer.name="Cluster arbre (surface m²)")
mapview::mapview(shp_Chemin_de_Rocquecourbe[which(as.numeric(shp_Chemin_de_Rocquecourbe$area)>2&shp_Chemin_de_Rocquecourbe$type=="bush"),2],legend=T,layer.name="Cluster bush (surface m²)")

st_write(shp_Chemin_de_Rocquecourbe,"D:/LiDARandField_Data_Fuel/Data_LiDAR_IGN/OLD/Uchaux/Shape_Zone_a_visiter/Chemin_de_Rocquecourbe.shp")


## map ----

Zone_Test_Uchaux=getpolybushandtree(clip_zone)

Zone_Test_Uchaux
Zone_test_Uchaux_Trees_10m=mapview::mapview(Zone_Test_Uchaux[which(as.numeric(Zone_Test_Uchaux$area)>2&Zone_Test_Uchaux$type=="trees"),2],legend=T,layer.name="Cluster arbre (surface m²)")
Zone_test_Uchaux_Bush_10m=mapview::mapview(Zone_Test_Uchaux[which(as.numeric(Zone_Test_Uchaux$area)>2&Zone_Test_Uchaux$type=="bush"),2],legend=T,layer.name="Cluster arbustif (surface m²)")
Zone_test_Uchaux_Bush_Trees=mapview::mapview(Zone_Test_Uchaux[which(as.numeric(Zone_Test_Uchaux$area)>2),3],legend=T,layer.name="Type Cluster")




# run on the whole commune ----
# set the number of workers 
plan(sequential)
plan(multisession, workers = 5L)

# Show progression  (8 hours for luberon => 400kmB2)
opt_progress(catalog_Uchaux)=T
opt_stop_early(catalog_Uchaux)=T

# 500m
opt_chunk_size(catalog_Uchaux)=500

# Do not use a Buffer, no need and slows down the process
opt_chunk_buffer(catalog_Uchaux) <- 100

# opt_laz_compression(catalog_Uchaux) = T

# Where to output the rasters
opt_output_files(catalog_Uchaux) <- "D:/LiDARandField_Data_Fuel/Data_LiDAR_IGN/OLD/Uchaux/Cluster_treesandbush_full/{ID}" # chemin vers le dossier de sortie des nouveau quadras

test=catalog_apply(catalog_Uchaux,getpolybushandtree)



library(sf)
library(tidyverse)
file_list <- list.files("D:/LiDARandField_Data_Fuel/Data_LiDAR_IGN/OLD/Uchaux/Cluster_treesandbush_full/", pattern = "*shp", full.names = TRUE)
shapefile_list <- lapply(file_list, read_sf)

# map ----


mapview::mapshot(Zone_test_Uchaux_Trees_10m, url = paste0("D:/LiDARandField_Data_Fuel/Data_LiDAR_IGN/OLD/Graphiques/Uchaux/4_François/Zone_test_Uchaux_Trees.html"))
mapview::mapshot(Zone_test_Uchaux_Bush_10m, url = paste0("D:/LiDARandField_Data_Fuel/Data_LiDAR_IGN/OLD/Graphiques/Uchaux/4_François/Zone_test_Uchaux_Bush.html"))
mapview::mapshot(Zone_test_Uchaux_Bush_Trees, url = paste0("D:/LiDARandField_Data_Fuel/Data_LiDAR_IGN/OLD/Graphiques/Uchaux/4_François/Zone_test_Uchaux_Bush_Trees.html"))

# clip_zone_buch=filter_poi(clip_zone,Z<=3&Z>0.5)



poly_trees$type="trees"
poly_bush$type="bush"

poly=rbind(poly_trees,poly_bush)

mapview::mapview(Uchaux_Shp[which(as.numeric(Uchaux_Shp$area)>50&Uchaux_Shp$type=="trees"),2],legend=T,layer.name="Cluster arbre",)
mapview::mapview(Uchaux_Shp[which(as.numeric(poly_trees$area)>0.36),3],legend=T,layer.name="Type de Cluster")


tab=data.table(as.data.frame(chm_bush,xy=T))
res <- dbscan(tab, eps = 0.5, minPts = 3)
tab$cluster=res$cluster
tab$clus_group=-1
tab[cluster==0]$cluster=NA
tes=rast(tab)
crs(clip_zone)="EPSG:2154"



clip_zone_bush=merge_spatial(clip_zone_bush,tes$cluster)
 clip_zone_bush=filter_poi(clip_zone_bush,id>=1)
 names(clip_zone_bush@data)[25]="treeID"
 poly_bush= crown_metrics(clip_zone_bush, .stdtreemetrics,attribute = "treeID", geom = "concave",concaveman = c(1,0))
 poly_bush$grpe_bush=cut(poly_bush$convhull_area,breaks = c(0,2,10,50,Inf))
 poly_bush=poly_bush[which(poly_bush$convhull_area>0.5),]
 plot(poly_bush[,6])
 st_write(poly_bush,"D:/Data_LiDAR_IGN/OLD/Graphiques/Uchaux/Shape_cluster/Bush_cluster.shp",overwrite =T)
 
 
 chm_trees <- app(chm, fun=function(x){ 
   x[x<= 5] <- NA
    x[x>5] <- 1
   
   ; return(x)} )
 
 tab_trees=data.table(as.data.frame(chm_trees,xy=T))
 res <- dbscan(tab_trees, eps = 0.5, minPts = 10)
 tab_trees$cluster=res$cluster
 tab_trees$clus_group=-1
 tab_trees[cluster==0]$cluster=NA
 rast_trees=rast(tab_trees)
 crs(clip_zone)="EPSG:2154"
 
 clip_zone_trees=merge_spatial(clip_zone,rast_trees$cluster)
 clip_zone_trees=filter_poi(clip_zone_trees,id>=1)
 names(clip_zone_trees@data)[25]="treeID"
 poly_trees= crown_metrics(clip_zone_trees, .stdtreemetrics,attribute = "treeID", geom = "concave",concaveman = c(0.5, 0))
 poly_trees$grpe_trees=cut(poly_trees$convhull_area,breaks = c(0,10,50,Inf))
 poly_trees=poly_trees[which(poly$convhull_area>0.5),]

 plot(poly_trees[,6])
 st_write(poly_trees,"D:/Data_LiDAR_IGN/OLD/Graphiques/Uchaux/Shape_cluster/Trees_cluster_5m.shp")
 st_read(poly_trees,"D:/Data_LiDAR_IGN/OLD/Graphiques/Uchaux/Shape_cluster/Trees_cluster.shp")
 
 
 
 bush_cluster=crown_metrics(clip_zone_bush,attribute="id")
 
 st_concave_hull(clip_zone_bush)
 
clus_size=tab[,.N,by="cluster"]
clus_size$size=clus_size$N/4

tab$cluster_size=clus_size[match(tab$cluster,clus_size$cluster)]$size

tab[cluster_size<2]$clus_group=1
tab[cluster_size>=2&cluster_size<10]$clus_group=2
tab[cluster_size>=10&cluster_size<50]$clus_group=3
tab[cluster_size>=50]$clus_group=4

concaveman(tab[cluster==1,1:2])

plot(tes$clus_group)
chm_bush$cluster=res$cluster
plot(chm_bush$cluster)



cluster_sf = st_as_sf(tab[is.na(cluster)==F], coords = c("x", "y"), crs = 2154)

split_clus <- split(tab,tab$cluster,drop = F )


hulls <- lapply(split_clus,function(x){return(concaveman(st_as_sf(x,coords = c("x", "y"), crs = 2154)))})
hulls <- do.call('rbind', hulls)

plot(meuse_sf['soil'], pch = 20, cex = 1, reset = FALSE, axes = T)
plot(hulls, add = TRUE, border = 'grey70', col = NA)




dbscan::dbscan(tab[,1:2]
chm_trees <- app(chm, fun=function(x){ x[x> 3] <- 2; return(x)} )
chm_trees