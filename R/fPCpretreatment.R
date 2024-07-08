#' Point cloud pre-treatment for using fCBDprofile_fuelmetrics in pixels
#'
#' @description Function to do the pretreatment on las (laz) files for using in fCBDprofile_fuelmetrics (get CBD profiles & fuel metrics). This can be used in catalog_apply lidR function. The pre-treatment consist in adding to noramlyze the pointcloud and adding several attribute: plane position for each point (easting, northin, elevation) and LMA by crossing the pointcloud with a LMA map
#' @param chunk path a las (laz) file. Can be apply to a catalog see lidr catalog apply)
#' @param classify logical (default is FALSE). Make a ground classification. Only if the original point cloud is not classified
#' @param norm_ground logical (default is FALSE). Calculate ground normals. 
#' @param LMA character or numeric. Default = 140. If available path to a LMA map (.tif) of the area if available or a single LMA value in g.m² (e.g 140. cf: Martin-Ducup et al. 2024)
#' @param WD character or numeric. Default = 591. If available, path to a WD map (.tif) of the area if available or a single WD value in kg.m3 (e.g 591 cf: Martin-Ducup et al. 2024).
#' @param LMA_bush  Default = 140. similar to LMA but for the understorey strata 0 to 2m
#' @param WD_bush Default = 591. similar to WD but for the understorey strata 0 to 2m
#' @return a Normalized point cloud (.laz) with several new attributes need to run fCBDprofile_fuelmetrics
#' @details
#' The attributes added to the laz are LMA : LMA value of each point. Zref :original Z; Easting, Northing, Elevation, Time that are the X,Y,Z position of the plane and the its GPStime for each point (obtained from lidR::track_sensor()). In a following version it will be possible to directly load a trajectory file if available.
#' @examples
#' \donttest{
#' path2laz=system.file("extdata","M30_FontBlanche.laz", package="lidarforfuel")
#'  #LMA value selected = 120.6 that is the LMA for Pinus halepensis, the dominant species of the plot
#' M30_FontBlanche_pretreated<-fPCpretreatment(path2laz,LMA=120.6)
#' # displaying the new attributes in the las
#' names(M30_FontBlanche_pretreated)
#' }

fPCpretreatment <- function(chunk,classify=F,norm_ground=F,LMA=140,WD=591,WD_bush=591,LMA_bush=140){ 
  
  # read chunk
  las <- lidR::readLAS(chunk)
  if (lidR::is.empty(las)) return(NULL)
  # Keep only classes 1 to 5
  if (classify == F){las=lidR::filter_poi(las,Classification%in%(1:5))}
  las_4_traj=las
  traj=try(lidR::track_sensor(las_4_traj,algorithm = lidR::Roussel2020()),silent=T)
  if(class(traj)[1]=="try-error"){
    first_last=lidR::filter_firstlast(las_4_traj)
    tab_count=first_last@data[, .(count = .N), by = gpstime]
    
    las_4_traj@data=las_4_traj@data[gpstime!=tab_count[count>2]$gpstime]
    las_4_traj@data=las_4_traj@data[!gpstime%in%tab_count[count>2]$gpstime]
    
    traj=try(lidR::track_sensor(las_4_traj,algorithm = lidR::Roussel2020()),silent=T)}
  # if track sensor not working at all take mean coordinates ( 1400 for Z) and gpstime to estimate trajectory 
  if(class(traj)[1]=="try-error"){
    traj= data.table(lidR::filter_ground(las)@data[,1:4])
    traj= traj[,.(Easting=mean(X),Northing=mean(Y),Elevation=mean(Z)+1400,Time=mean(gpstime)),]
    
  }
  if(class(traj)[1]!="data.table"){
    traj=data.table(cbind(st_coordinates(traj),Time=traj$gpstime))}
  # if track sensor not working  at all take mean coordinates ( 1400 for Z) and gpstime to estimate trajectory 
  
  if(nrow(traj)==0){
    traj= data.table(filter_ground(las)@data[,1:4])
    traj= traj[,.(Easting=mean(X),Northing=mean(Y),Elevation=mean(Z)+1400,Time=mean(gpstime)),]
  }
  names(traj)=c("Easting","Northing","Elevation","Time")
  # Find closest gpstime between traj and las
  nn2_gpstimes=RANN::nn2(traj$Time,las@data$gpstime,k=1)
  las@data=cbind(las@data,traj[nn2_gpstimes$nn.idx,])
  
  if(classify==T){
    lidR::classify_ground(las,algorithm = csf())
    
  }
  
  if (norm_ground == TRUE){
    # Filter ground points
    las_ground=lidR::filter_ground(las)
    dtm = lidR::rasterize_terrain(las_ground, algorithm = tin(),res=3)
    dtm_las=LAS(data.table(as.data.frame(dtm,xy=T)))
    # calculate normals on dtm and get vector components
    dtm_las=geom_features(las=dtm_las,search_radius = 6,features_list = c("Nx","Ny","Nz"))
    
    # Find closest neighboor between normal DTM and las and attribute normal DTM to each point of the las
    nn2_las_DTM=nn2(dtm_las@data[,1:3],las@data[,1:3],k=1)
    las@data=cbind(las@data,dtm_las@data[nn2_las_DTM$nn.idx,4:6])
  }
  
  # LMA
  if(is.numeric(LMA)){las@data$LMA=LMA}
  if(is.numeric(WD)){las@data$LMA=WD}
  if(is.numeric(LMA)==F){
    ## Load LMA map
    LMA_map=terra::rast(LMA)
    ### Add LMA to point cloud
    las=lidR::merge_spatial(las,LMA_map$LMA,attribute = "LMA")
  }
  if(is.numeric(WD)==F){
    ## Load LMA map
    WD_map=terra::rast(WD)
    ### Add WD to point cloud
    las=lidR::merge_spatial(las,WD_map$WD,attribute = "WD")
  }
  
  # Normalyze height
  las=lidR::normalize_height(las = las,algorithm =  lidR::tin() )
  # Remove points too low (<-3) or too high (>35m)
  las=lidR::classify_noise(las, lidR::sor(5,10))
  las=lidR::filter_poi(las,Classification<=5)
  las@data[Z<=2]$LMA=LMA_bush
  las@data[Z<=2]$WD=WD_bush
  # add names to laz
  las=lidR::add_lasattribute(las,name="LMA",desc="leaf mass area")
  las=lidR::add_lasattribute(las,name="WD",desc="Wood density")
  las=lidR::add_lasattribute(las,name="Zref",desc="original Z")
  las=lidR::add_lasattribute(las,name="Easting",desc="traj")
  las=lidR::add_lasattribute(las,name="Northing",desc="traj")
  las=lidR::add_lasattribute(las,name="Elevation",desc="traj")
  if (norm_ground == T){
    las=lidR::add_lasattribute(las,name="Nx",desc="normal")
    las=lidR::add_lasattribute(las,name="Ny",desc="normal")
    las=lidR::add_lasattribute(las,name="Nz",desc="normal")
  }
  las=lidR::add_lasattribute(las,name="Time",desc="plane time")
  # las=remove_lasattribute(las, name="Reflectance")
  # las=remove_lasattribute(las, name="Deviation")
  # las@data=las@data[,c("X","Y","Z","LMA","Zref","Easting","Northing","Elevation","Nx","Ny","Nz","Time")]
  # las@header@VLR = list("X","Y","Z","LMA","Zref","Easting","Northing","Elevation","Nx","Ny","Nz","Time")
  
  return(las)
}
