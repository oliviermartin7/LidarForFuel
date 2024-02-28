
#########################################################################
### Point cloud pre-treatment for fCBDprofile_fuelmetrics in pixels  ####
#########################################################################

# description: Function to do the pretreatment on las (laz) files for using in fCBDprofile_fuelmetrics (get CBD profiles & fuel metrics). This can be used in catalog_apply lidR function. The pre-treatment consist in adding to noramlyze the pointcloud and adding several attribute: plane position for each point (easting, northin, elevation) and LMA by crossing the pointcloud with a LMA map
# Input:
## chunk: path a las (laz) file. Can be apply to a catalog see lidr catalog apply)
## classify: logical (default is FALSE). Make a ground classification. Only if the original point cloud is not classified
## norm_ground: logical (default is FALSE). Calculate ground normals. "Not needed anymore"
## LMA: Path to a LMA map (.tif) of the area if available or a single LMA value in g.m² (e.g 140. cf: Martin-Ducup et al. 2024)
# Output :
## a normalyze and filter (keep only ground, vegetation and not classified points => remove buildings... and noise. Apply only if classify = F) las with new attributes

fPCpretreatment <- function(chunk,classify=F,norm_ground=F,LMA){ 
  
  # read chunk
  las <- readLAS(chunk)
  if (lidR::is.empty(las)) return(NULL)
  # Keep only classes 1 to 5
  if (classify == F){las=filter_poi(las,Classification%in%(1:5))}
  las_4_traj=las
  traj=try(track_sensor(las_4_traj,algorithm = Roussel2020()),silent=T)
  if(class(traj)[1]=="try-error"){
    first_last=filter_firstlast(las_4_traj)
    tab_count=first_last@data[, .(count = .N), by = gpstime]
    
    las_4_traj@data=las_4_traj@data[gpstime!=tab_count[count>2]$gpstime]
    las_4_traj@data=las_4_traj@data[!gpstime%in%tab_count[count>2]$gpstime]
    
    traj=try(track_sensor(las_4_traj,algorithm = Roussel2020()),silent=T)}
  # if track sensor not working at all take mean coordinates ( 1400 for Z) and gpstime to estimate trajectory 
  if(class(traj)[1]=="try-error"){
    traj= data.table(filter_ground(las)@data[,1:4])
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
  nn2_gpstimes=nn2(traj$Time,las@data$gpstime,k=1)
  las@data=cbind(las@data,traj[nn2_gpstimes$nn.idx,])
  
  if(classify==T){
    classify_ground(las,algorithm = csf())
    
  }
  
  if (norm_ground == TRUE){
    # Filter ground points
    las_ground=filter_ground(las)
    dtm = rasterize_terrain(las_ground, algorithm = tin(),res=3)
    dtm_las=LAS(data.table(as.data.frame(dtm,xy=T)))
    # calculate normals on dtm and get vector components
    dtm_las=geom_features(las=dtm_las,search_radius = 6,features_list = c("Nx","Ny","Nz"))
    
    # Find closest neighboor between normal DTM and las and attribute normal DTM to each point of the las
    nn2_las_DTM=nn2(dtm_las@data[,1:3],las@data[,1:3],k=1)
    las@data=cbind(las@data,dtm_las@data[nn2_las_DTM$nn.idx,4:6])
  }
  
  # LMA
  if(is.numeric(LMA)){las@data$LMA=LMA}
  if(is.numeric(LMA)==F){
    ## Load LMA map
    LMA_map=rast(LMA)
    ### Add LMA to point cloud
    las=merge_spatial(las,LMA_map$LMA,attribute = "LMA")
    }
  
  # Normalyze height
  las=normalize_height(las = las,algorithm =  tin() )
  # Remove points too low (<-3) or too high (>35m)
  las=classify_noise(las, sor(5,10))
  las=filter_poi(las,Classification<=5)
  
  # add names to laz
  las=add_lasattribute(las,name="LMA",desc="leaf mass area")
  las=add_lasattribute(las,name="Zref",desc="original Z")
  las=add_lasattribute(las,name="Easting",desc="traj")
  las=add_lasattribute(las,name="Northing",desc="traj")
  las=add_lasattribute(las,name="Elevation",desc="traj")
  if (norm_ground == T){
    las=add_lasattribute(las,name="Nx",desc="normal")
    las=add_lasattribute(las,name="Ny",desc="normal")
    las=add_lasattribute(las,name="Nz",desc="normal")
  }
  las=add_lasattribute(las,name="Time",desc="plane time")
  # las=remove_lasattribute(las, name="Reflectance")
  # las=remove_lasattribute(las, name="Deviation")
  # las@data=las@data[,c("X","Y","Z","LMA","Zref","Easting","Northing","Elevation","Nx","Ny","Nz","Time")]
  # las@header@VLR = list("X","Y","Z","LMA","Zref","Easting","Northing","Elevation","Nx","Ny","Nz","Time")
  
  return(las)
}
