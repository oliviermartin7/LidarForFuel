
library(lidR)
library(terra)
library(data.table)
library(dbscan)
library(sf)
library(stars)
library(future)

# chunk = path to a laz file (chunk because it can be used with catalog_apply from lidR). 
# bush_strata_limits = a vector. lower and upper limits (in meter) of the bush strata.default : c(0.2,2)
# lower_tree_strata_limit = lower limit (in meter) of the canopy strata.  Default: 3
# res_pixel_bush = pixel size (in meter) used in the bush strata. Default: 0.3
# res_pixel_trees = pixel size (in meter) used in the tree strata. Default: 0.3
# filter_tree_size = filter tree cluster below 0.1m² to avoid many useless small clusters. Default: 0.1

fgetpolybushandtree=function(chunk,bush_strata_limits=c(0.2,2),lower_tree_strata_limit=3,res_pixel_bush=0.3,res_pixel_trees=0.3,filter_tree_size=0.1){
  # read chunk
  # if(class(chunk)=="character"){
  las <- lidR::readLAS(chunk)
  # else{las=chunk} 
  
  if (is.empty(las)) return(NULL)
  
  # Normalyze height
  las=lidR::normalize_height(las = las,algorithm =  lidR::tin() )
  # Remove points too low (<-3) or too high (>35m)
  las=lidR::classify_noise(las, lidR::sor(5,10))
  las=lidR::filter_poi(las,Classification<=5)
  # ratio_bush=pixel_metrics(las,fun2(Z=Z),1)
  # N_in_bush=pixel_metrics(las,fN(Z),1)
  
  clip_zone_bush=lidR::filter_poi(las,Z<=2) # selectionne les pts bush < 3m
  chm_bush <- lidR::rasterize_canopy(clip_zone_bush, res = res_pixel_bush, p2r()) # crée un model de surface à partir du ndp bush
  chm_bush <- terra::app(chm_bush, fun=function(x){ 
    x[x <= bush_strata_limits[2]&x>=bush_strata_limits[1]] <- 1
    x[x < bush_strata_limits[1]] <- NA
    x[x > bush_strata_limits[2]] <- NA
    ; return(x)} ) # Associe valeur 1 aux cellule de bush dans le raster
  
  
  poly_bush=stars::st_as_stars(chm_bush)
  poly_bush=sf::st_as_sf(poly_bush,as_point=F,merge=T) # créer un shape à partir du raster bush 
  poly_bush$area=sf::st_area(poly_bush)
  # poly_bush[which(as.numeric(poly_bush$area)<0.18),]
  
  
  
  clip_zone_trees=lidR::filter_poi(las,Z>3)
  chm_trees <- lidR::rasterize_canopy(clip_zone_trees, res = res_pixel_trees, p2r())
  chm_trees <- terra::app(chm_trees, fun=function(x){ 
    x[x > lower_tree_strata_limit] <- 1
    ; return(x)} )
  
  
  
  poly_trees=stars::st_as_stars(chm_trees)
  poly_trees=sf::st_as_sf(poly_trees,as_point=F,merge=T)
  poly_trees$area=sf::st_area(poly_trees)
  poly_trees[which(as.numeric(poly_trees$area)<filter_tree_size),]
  
  
  poly_trees$type="trees"
  poly_bush$type="bush"
  
  poly=rbind(poly_trees,poly_bush)
  # poly=poly[which(as.numeric(poly$area)>0.36),]
  
  return(poly)
}


# run on a zone  ----

shp_test=fgetpolybushandtree("pathtothelazfile")

## plot dynamic map of trees and bush clusters (min cluster size of 2 m²) ----
mapview::mapview(shp_test[which(as.numeric(shp_test$area)>2&shp_test$type=="trees"),2],legend=T,layer.name="Cluster trees (m²)")
mapview::mapview(shp_test[which(as.numeric(shp_test$area)>2&shp_test$type=="bush"),2],legend=T,layer.name="Cluster bush (m²)")

## plot both ----
mapview::mapview(shp_test,legend=T,layer.name="Bush & trees  (m²)")
