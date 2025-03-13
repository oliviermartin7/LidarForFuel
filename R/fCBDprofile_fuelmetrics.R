#' Fuel metrics LiDAR
#'
#' @description Function to compute PAD and CBD profiles from a pretreated ALS point cloud (cf fPCpretreatment) and obtain fuel metrics from it.
#' @param datatype either "Pixel" or directly a laz/las file. Default is "Pixel". "Pixel" if the function is used with pixel_metric function to map fuel metrics. Or a .laz/.las file if a plot point cloud only needs to be computed. In the latter case, all arguments corresponding to a .laz file attributes are automatically retrieved, therefore, no need to fulfill argument X,Y,Z,Zref, Easting, Northing, Elevation, LMA, gpstime.
#' @param X,Y,Z,Zref numeric, coordinates of a point cloud (Z being the normalized Z coordinate and Zref the original one)
#' @param ReturnNumber numeric, the return number of a pulse. Used to for calculating cover.
#' @param Easting,Northing,Elevation numeric, coordinates of the plane associated to each point
#' @param LMA numeric. Leaf mass area in g.cm² associated to each point or a generic value
#' @param WD numeric. wood density associated to each point or a generic value
#' @param threshold numeric or character. Default = 0.012. Bulk density critical threshold  used to discriminate the different strata limits, midstrorey height, canopy base, canopy top etc. Either numeric : a bulk density value (in kg/m3) or character: a percentage of maximum CBD value.
#' @param limit_N_points numeric. Default = 400. minimum number of point in the pixel/plot for computing profiles & metrics.
#' @param limit_flightheight numeric. Default = 800. flight height above canopy in m. If the flight height is lower than limit_flyheight bulk density profile is not computed.  This limit serves as a safeguard to eliminate cases where the trajectory reconstruction would be outlier.
#' @param scanning_angle logical. Default = TRUE. Use the scanning angle computed from the trajectories to estimate cos(theta). If false: cos(theta) = 1
#' @param omega numeric. clumping factor. Default is 1. One means "no clumping" and therefore assumes a homogeneous distribution of vegetation element in the strata.
#' @param d numeric. Default = 1. depth of the strata in meter to compute the profile
#' @param G numeric. Default = 0.5. Leaf projection ratio.
#' @param gpstime gpstime of the point cloud. Only used to retrieve date of scanning
#' @param Height_cover numeric, Default = 2. The height from which the canopy cover should be estimated.
#' @return If datatype = "Pixel" raster is returned with 173 Bands corresponding to metrics and bulk density profile value per strata of depth d. If datatype is a las a list of two elements: 1) a vector with all fuel metrics 2) a data.table with the PAD and CBD profile value (three columns: H, PAD and CBD),
#' @details
#' This function can be used with pixel_metrics lidR function to generate maps (raster). Most of the argument of the function (i.e X,Y,Z,Zref,Easting, Northing,Elevation,LMA,WD,gpstime) comes from a pretreated poincloud obtained with the function fPCpretreatment. Note that not only fuel metrics are quantified but also: Height, plant area index above one meter (PAI_tot), vertical complexity index (VCI) based on plant area density profile or based on point cloud (lidR method). Note that the bulk density values of the profile are given in the raster using one layer per strata (with a depth = d) starting from layer 23 (i.e Band 23). Note also that in case of using the plot approach (i.e datatype = las) the profile  is given as a data.table in the second element of the list.
#' @examples
#' \donttest{
#' path2laz <- system.file("extdata","M30_FontBlanche_pretreated.laz", package="lidarforfuel")
#'
#' # read a pretreated las
#' M30_FontBlanche_pretreated <- lidR::readLAS(path2laz)
#'
#' # Run the function on a las file and get a vector of the metrics and PAD and CDB profile
#' Fuel_metrics <- fCBDprofile_fuelmetrics(datatype=M30_FontBlanche_pretreated,WD=500)
#'
#' # Run with pixel_metrics and get a raster
#' M30_FontBlanche_Raster <- lidR::pixel_metrics(M30_FontBlanche_pretreated,~fCBDprofile_fuelmetrics(X=X,Y=Y,Z=Z,Zref = Zref,gpstime =gpstime, ReturnNumber=ReturnNumber,Easting = Easting ,Northing = Northing,Elevation = Elevation ,LMA = LMA,threshold = 0.016,WD = 500 ,limit_N_points = 400,datatype = "Pixel",omega = 0.77,d=0.5,G = 0.5),res=10)
#'
#' # Replace -1 in cells not computed  by NA
#' M30_FontBlanche_Raster <- terra::subst(M30_FontBlanche_Raster,-1,NA)
#'
#' # Plot a few metrics
#' terra::plot(M30_FontBlanche_Raster[[8:23]])
#'
#' }


fCBDprofile_fuelmetrics=function(datatype="Pixel",X,Y,Z,Zref,ReturnNumber,Easting,Northing,Elevation,LMA,gpstime,Height_Cover=2,threshold=0.012,scanning_angle=TRUE,WD,limit_N_points=400,limit_flightheight=800,omega=1,d=1,G=0.5){
  if(class(datatype)[1]=="LAS"){
    X=datatype$X
    Y=datatype$Y
    Z=datatype$Z
    Zref=datatype$Zref
    ReturnNumber=datatype$ReturnNumber
    Easting=datatype$Easting
    Northing=datatype$Northing
    Elevation=datatype$Elevation
    LMA=datatype$LMA
    gpstime=datatype$gpstime
  }
  date=mean(gpstime)
  library(data.table)
  if(length(Z)<limit_N_points){
    VVP_metrics=c(Profil_Type=-1,Profil_Type_L=-1,threshold=-1,Height=-1,CBH=-1,FSG=-1,Top_Fuel=-1,H_Bush=-1,continuity=-1,VCI_PAD=-1,VCI_lidr=-1,entropy_lidr=-1,PAI_tot=-1,CBD_max=-1,CFL=-1,TFL=-1,MFL=-1,FL_1_3=-1,GSFL=-1,FL_0_1=-1,FMA=-1,date=date,Cover=-1)
    VVP_metrics_CBD=rep(-1,150)
    VVP_metrics=c(VVP_metrics,VVP_metrics_CBD)
    PAD_CBD_Profile=NULL
    names(VVP_metrics)=c("Profil_Type","Profil_Type_L","threshold","Height","CBH","FSG","Top_Fuel","H_Bush","continuity","VCI_PAD","VCI_lidr","entropy_lidr","PAI_tot","CBD_max","CFL","TFL","MFL","FL_1_3","GSFL","FL_0_1","FMA","date","Cover",paste0("CBD_",rep(1:150)))
    if(class(datatype)[1]=="LAS"){
      return(list(VVP_metrics,PAD_CBD_Profile))}
    if(datatype=="Pixel"){
      return(as.list(VVP_metrics))}
  }
  # get cover

  Cover=length(which(ReturnNumber[which(Z>Height_Cover)]==1))/length(which(ReturnNumber==1))


  # Get PAD and CBD profile ----
  ## Create a sequence to make strata  ----
  seq_layer=c(min(Z),seq(0,max(Z),d),max(Z))
  ## hist to get number of return in strata  ----
  hist_z=hist(Z,breaks=seq_layer,plot=F)$count
  ## NRD estimation  ----
  NRD=hist_z/cumsum(hist_z)
  ## Gap fraction estimation ----
  Gf=1-NRD

  if(scanning_angle==TRUE){
  ## calculates component of  vector U (plane -> point). To take into account scanning angle in PAD estimation ----
  norm_U=sqrt((X-Easting)^2+(Y-Northing)^2+(Zref-Elevation)^2)
  Nx_U=abs((X-Easting)/norm_U)
  Ny_U=abs((Y-Northing)/norm_U)
  Nz_U=abs((Zref-Elevation)/norm_U)
  }

  if(scanning_angle==FALSE){
  Nz_U=1
  norm_U=999999
  }
  ### Exception if the mean of norm_U < limit_flightheight For LiDAr HD 1000m mean that plane flew lower than 1000m over the plot => unlikely for LiDAR HD => probably error in trajectory reconstruction
  if(mean(norm_U,na.rm=T)<limit_flightheight){
    VVP_metrics=c(Profil_Type=-1,Profil_Type_L=-1,threshold=-1,Height=-1,CBH=-1,FSG=-1,Top_Fuel=-1,H_Bush=-1,continuity=-1,VCI_PAD=-1,VCI_lidr=-1,entropy_lidr=-1,PAI_tot=-1,CBD_max=-1,CFL=-1,TFL=-1,MFL=-1,FL_1_3=-1,GSFL=-1,FL_0_1=-1,FMA=-1,date=date,Cover=-1)
    VVP_metrics_CBD=rep(-1,150)
    VVP_metrics=c(VVP_metrics,VVP_metrics_CBD)
    PAD_CBD_Profile=NULL
    names(VVP_metrics)=c("Profil_Type","Profil_Type_L","threshold","Height","CBH","FSG","Top_Fuel","H_Bush","continuity","VCI_PAD","VCI_lidr","entropy_lidr","PAI_tot","CBD_max","CFL","TFL","MFL","FL_1_3","GSFL","FL_0_1","FMA","date","Cover",paste0("CBD_",rep(1:150)))
    if(class(datatype)[1]=="LAS"){
      return(list(VVP_metrics,PAD_CBD_Profile))}
    if(datatype=="Pixel"){
      return(as.list(VVP_metrics))}
  }

  ### cos theta take into account scanning angle
  cos_theta=mean(abs(Nz_U))

  G=G   # Leaf projection angle
  d=d # strata depth
  omega=omega # Clumping factor. 1= Random distribution = < 1 = clumped
  ## Plant area density calculation (actually FAD --> fuel area density: leaves + twigs) ----
  PAD=-(log(Gf)*cos_theta/(G*omega)/d)
  Var_PAD=(PAD^2/(NRD))/cumsum(hist_z)
  ### LMA from g/cm² to kg.m2
  LMA=mean(LMA,na.rm=T)/1000

  ## Partition of fuel surface (fine branch vs leaves) ----
  ### Wood density (kg/m3)
  WD=mean(WD)
  ### Surface volume ratio (SVR: m²/m3) for 4mm diameter twigs (=> wood fuel) = 2.pi.r.l*(1/2)/pi.r².l = 1/r
  SVR=1/0.002
  ### Wood mass area (WMA)
  WMA=WD/SVR

  ### Partition of wood and leaves => M. Soma phd thesis data
  partW=0.51
  partL=0.49
  ## Fuel mass area  ----
  if(is.na(LMA)){LMA=0.15}
  FMA=1/((partW/WMA)+(partL/LMA))

  ## CBD in kg/m3 ----
  CBD=PAD*(FMA)


  ### remove the bottom & top value of the seq and add d/2 to get the middle height of the strata for each stratum
  seq_layer=seq_layer[-c(1,length(seq_layer))]+(d/2)

  ## Table with strata height PAD and CBD ----
  PAD_CBD_Profile=data.table::data.table(cbind(H=seq_layer),PAD=PAD[-1],CBD=CBD[-1],Var_PAD=Var_PAD[-1],NRD=NRD[-1],Ni=hist_z[-1])

  # 2. Work on profile to get FPT and fuel metrics ----

  ##  Define threshold when threshold is a proportion of CBD max----
  if(stringr::str_detect(threshold,"%")){
    threshold_prop=as.numeric( str_split(threshold,"%",simplify = T)[,1])/100
    threshold=max(PAD_CBD_Profile[H>1]$CBD)*threshold_prop
  }

  ### no data above 0.5m
  if(max(PAD_CBD_Profile$H)<0.5){
    VVP_metrics=c(Profil_Type=-1,Profil_Type_L=-1,threshold=-1,Height=-1,CBH=-1,FSG=-1,Top_Fuel=-1,H_Bush=-1,continuity=-1,VCI_PAD=-1,VCI_lidr=-1,entropy_lidr=-1,PAI_tot=-1,CBD_max=-1,CFL=-1,TFL=-1,MFL=-1,FL_1_3=-1,GSFL=-1,FL_0_1=-1,FMA=-1,date=date,Cover=-1)
    VVP_metrics_CBD=rep(-1,150)
    VVP_metrics=c(VVP_metrics,VVP_metrics_CBD)
    names(VVP_metrics)=c("Profil_Type","Profil_Type_L","threshold","Height","CBH","FSG","Top_Fuel","H_Bush","continuity","VCI_PAD","VCI_lidr","entropy_lidr","PAI_tot","CBD_max","CFL","TFL","MFL","FL_1_3","GSFL","FL_0_1","FMA","date","Cover",paste0("CBD_",rep(1:150)))
    if(class(datatype)[1]=="LAS"){
      return(list(VVP_metrics,PAD_CBD_Profile))}
    if(datatype=="Pixel"){
      return(as.list(VVP_metrics))}
  }
  ## Get CBD roll mean to smooth the profiles & get the profile above CBD threshold  ----
  ### Organise roll mean CBD depending on number of 0.5m strata >3
  if(nrow(PAD_CBD_Profile)>3){
    PAD_CBD_Profile$CBD_rollM=data.table::frollmean(PAD_CBD_Profile$CBD,3,algo="exact")
    PAD_CBD_Profile$CBD_rollM[1:3]=PAD_CBD_Profile$CBD[1:3]
    PAD_CBD_Profile_threshold=PAD_CBD_Profile[CBD_rollM>threshold]
  }
  ### Organise roll mean CBD depending on number of 0.5m strata <= 3
  if(nrow(PAD_CBD_Profile)<=3){
    PAD_CBD_Profile$CBD_rollM=PAD_CBD_Profile$CBD
    PAD_CBD_Profile_threshold=PAD_CBD_Profile[CBD_rollM>threshold]
  }
  ### No data
  if(nrow(PAD_CBD_Profile_threshold)==0){
    VVP_metrics=c(Profil_Type=-1,Profil_Type_L=-1,threshold=-1,Height=-1,CBH=-1,FSG=-1,Top_Fuel=-1,H_Bush=-1,continuity=-1,VCI_PAD=-1,VCI_lidr=-1,entropy_lidr=-1,PAI_tot=-1,CBD_max=-1,CFL=-1,TFL=-1,MFL=-1,FL_1_3=-1,GSFL=-1,FL_0_1=-1,FMA=-1,date=date,Cover=-1)
    VVP_metrics_CBD=rep(-1,150)
    VVP_metrics=c(VVP_metrics,VVP_metrics_CBD)
    names(VVP_metrics)=c("Profil_Type","Profil_Type_L","threshold","Height","CBH","FSG","Top_Fuel","H_Bush","continuity","VCI_PAD","VCI_lidr","entropy_lidr","PAI_tot","CBD_max","CFL","TFL","MFL","FL_1_3","GSFL","FL_0_1","FMA","date","Cover",paste0("CBD_",rep(1:150)))
    if(class(datatype)[1]=="LAS"){
      return(list(VVP_metrics,PAD_CBD_Profile))}
    if(datatype=="Pixel"){
      return(as.list(VVP_metrics))}
  }

  ## Get number of discontinuity (FSG) of 1m or more ----
  shift_H=data.table::shift(PAD_CBD_Profile_threshold$H)
  shift_H[1]=d/2
  delta_layer=PAD_CBD_Profile_threshold$H-shift_H
  Discontinuity=delta_layer[which(delta_layer>1)]

  ## GEt the FPT ----

  ### one discontinuity = Discontinuous = Stratified = Profile 1
  if(length(Discontinuity)==1){
    delta_ID=which(delta_layer==Discontinuity)
    Profil_Type=1
    Profil_Type_L=1
  }
  ###  If more than one discontinuities
  if(length(Discontinuity)>1){

    ##### if all the gap are <= 1 keep only the first => slightly Complex with small discontinuities = almost continuous = Profile type 2
    if(all(Discontinuity<=1)){
      Discontinuity=Discontinuity[1]
      delta_ID=which(delta_layer==Discontinuity)[1]
      Profil_Type=2
      Profil_Type_L=3
    }

    #### If only one of the continuities is >1 keep this one.  Complex : One big discontinuities and other small = almost stratified but small complexity =  Profile type 3
    if(length(which(Discontinuity>1))==1){
      Discontinuity=Discontinuity[which(Discontinuity>1)]
      delta_ID=which(delta_layer==Discontinuity)
      Profil_Type=3
      Profil_Type_L=3
    }

    ##### If more than one disconitnuities is above one keep the first = Complex : Multilayered = Profil_Type= 4
    if(length(which(Discontinuity>1))>1){

      Discontinuity=Discontinuity[1]
      delta_ID=which(delta_layer==Discontinuity)

      if(length(delta_ID)>1){delta_ID=delta_ID[1]}
      Profil_Type=4
      Profil_Type_L=3
    }

  }
  ## Get metrics ----
  ### Profil continue = Profil 5
  if(length(Discontinuity)==0){

    #
    CBH=0
    FSG=0
    Top_Fuel=max(PAD_CBD_Profile_threshold$H)
    H_Bush=Top_Fuel
    continuity=1
    Profil_Type=5
    Profil_Type_L=4

  }

  ### Profil discontinue
  if(length(Discontinuity)>0){

    #### profil discontinue without understory strata
    if(min(PAD_CBD_Profile_threshold$H)>1.25){
      CBH=PAD_CBD_Profile_threshold$H[delta_ID]
      FSG=Discontinuity
      H_Bush=0

    }
    #### profil discontinue with understory strata
    if(min(PAD_CBD_Profile_threshold$H)<=1.25){

      CBH=PAD_CBD_Profile_threshold$H[delta_ID]
      FSG=Discontinuity
      H_Bush=CBH-FSG
      if(Profil_Type_L==1){Profil_Type_L=2}
    }
    Top_Fuel=max(PAD_CBD_Profile_threshold$H)
    continuity = 0
  }

  # get metrics (above 0.5m)
  PAI_tot=sum(PAD_CBD_Profile[H>1]$PAD)*d
  VCI_PAD =-sum(PAD_CBD_Profile[H>1]$PAD/sum(PAD_CBD_Profile[H>1]$PAD)*log(PAD_CBD_Profile[H>1]$PAD/sum(PAD_CBD_Profile[H>1]$PAD)))/log(length(PAD_CBD_Profile[H>1]$PAD))
  VCI_lidr=lidR::VCI(Z[Z>1],zmax = max(Z))
  entropy_lidr=lidR::entropy(Z[Z>1],zmax = max(Z))
  Height=max(PAD_CBD_Profile$H)
  CBD_max=max(PAD_CBD_Profile[H>1]$CBD_rollM)
  CFL=sum(PAD_CBD_Profile[H>1&H>=CBH&H<=Height]$CBD_rollM)*d
  TFL=sum(PAD_CBD_Profile[H>1&H<=Height]$CBD_rollM)*d
  if(CBH==0){MFL=TFL}else(MFL=sum(PAD_CBD_Profile[H>1&H<=H_Bush]$CBD_rollM)*d)
  FL_0_1=sum(PAD_CBD_Profile[H<=1]$CBD_rollM)*d
  FL_1_3=sum(PAD_CBD_Profile[H>1&H<=3]$CBD_rollM)*d
    if(FSG==0){GSFL=0}else{GSFL=sum(PAD_CBD_Profile[H>H_Bush&H<=CBH]$CBD_rollM)*d}


  VVP_metrics=c(Profil_Type,Profil_Type_L,threshold,Height,CBH,FSG,Top_Fuel,H_Bush,continuity,VCI_PAD,VCI_lidr,entropy_lidr,PAI_tot,CBD_max,CFL,TFL,MFL,FL_1_3,GSFL,FL_0_1,FMA,date,Cover)

  VVP_metrics_CBD=rep(-1,150)
  VVP_metrics_CBD[1:length(PAD_CBD_Profile$CBD_rollM)]=PAD_CBD_Profile$CBD_rollM
  VVP_metrics=c(VVP_metrics,VVP_metrics_CBD)

  names(VVP_metrics)=c("Profil_Type","Profil_Type_L","threshold","Height","CBH","FSG","Top_Fuel","H_Bush","continuity","VCI_PAD","VCI_lidr","entropy_lidr","PAI_tot","CBD_max","CFL","TFL","MFL","FL_1_3","GSFL","FL_0_1","FMA","date","Cover",paste0("CBD_",rep(1:150)))

  if(class(datatype)[1]=="LAS"){
    return(list(VVP_metrics,PAD_CBD_Profile))}
  if(datatype=="Pixel"){
    return(as.list(VVP_metrics))}
}
