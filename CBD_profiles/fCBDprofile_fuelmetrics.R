
###########################
### Fuel metrics LiDAR ####
###########################

# description: Function to compute PAD and CBD profiles from ALS point cloud and obtain fuel metrics from it (Cf Martin-Ducup et al. 2024). This function can be used with pixel_metrics lidR function to generate maps (raster). This explains why several elements of a las object have to be given as separated argument (impossible to use a las in pixel_metrics)
# Input:
## X,Y,Z,Zref : coordinates of a point cloud (Z being the normalyzed Z corddinate and Zref the original one)
## Easting,Northing,Elevation: coordinates of the plane associated to each point
## LMA: Leaf mass area in g.cm² associated to each point or a generic value
## WD: wood density associated to each point or a generic value
## threshold=0.012: CBD critical threshold value used
## limit_N_points= minimum numer of point in the pixel/plot for computing profiles & metrics. Default is 400
## datatype: Either "Pixel" (if using pixel metric function) or "Plot" if only a plot is computed. Default is "Pixel"
## omega: clumping factor. Default is 1
## d: strata depth. Default is 0.5
## G: leaf projection ratio. Default is 0.5

# Output :
## If datatype = "Pixel" a vector containing all the fuel metrics and the CBD value for each strata
## If datatype="Plot" a list of 2 elements: 1) a vector with all fuel metrics 2) a datable with the PAD anbd CBD profile value (H, PAD and CBD) CBD profile is given in layer (one layer per strata), 




fCBDprofile_fuelmetrics=function(X,Y,Z,Zref,Easting,Northing,Elevation,LMA,threshold=0.012,WD,limit_N_points=400,datatype="Pixel",omega=1,d=0.5,G=0.5){
  library(data.table)
  if(length(Z)<limit_N_points){
    VVP_metrics=c(Profil_Type=-1,Profil_Type_L=-1,threshold=-1,CBH=-1,FSG=-1,Top_Fuel=-1,H_Bush=-1,continuity=-1,VCI_PAD=-1,VCI_lidr=-1,entropy_lidr=-1,PAI_tot=-1,CBD_max=-1,CFL=-1,TFL=-1,UFL=-1,FL_05_3=-1,FMA=-1)
    VVP_metrics_CBD=rep(-1,100)
    VVP_metrics=c(VVP_metrics,VVP_metrics_CBD)
    names(VVP_metrics)=c("Profil_Type","Profil_Type_L","threshold","CBH","FSG","Top_Fuel","H_Bush","continuity","VCI_PAD","VCI_lidr","entropy_lidr","PAI_tot","CBD_max","CFL","TFL","UFL","FL_05_3","FMA",paste0("CBD_",rep(1:100)))
    if(datatype=="Plot"){ 
      return(list(VVP_metrics,PAD_CBD_Profile))}
    if(datatype=="Pixel"){
      return(as.list(VVP_metrics))}
  }
  
  # Get PAD and CBD profile ----
  ## Create a sequence to make strata  ----
  seq_layer=c(min(Z),seq(0,max(Z),d),max(Z))
  ## hist to get number of return in strata  ----
  hist_z=hist(Z,breaks=seq_layer,plot=F)$count
  ## NRD estimation  ----
  NRD=hist_z/cumsum(hist_z)
  ## Gap fraction estimation ----
  Gf=1-NRD
  
  ## calculates component of  vector U (plane -> point). To take into account scanning angle in PAD estimation ----
  norm_U=sqrt((X-Easting)^2+(Y-Northing)^2+(Zref-Elevation)^2)
  Nx_U=abs((X-Easting)/norm_U)
  Ny_U=abs((Y-Northing)/norm_U)
  Nz_U=abs((Zref-Elevation)/norm_U)
  
  ### Exception if the mean of norm_U < 1000 mean that plane flew lower than 1000m over the plot => unlikely for LiDAR HD => probably error in trajectory reconstruction
  if(mean(norm_U,na.rm=T)<1000){
    VVP_metrics=c(Profil_Type=-1,Profil_Type_L=-1,threshold=-1,CBH=-1,FSG=-1,Top_Fuel=-1,H_Bush=-1,continuity=-1,VCI_PAD=-1,VCI_lidr=-1,entropy_lidr=-1,PAI_tot=-1,CBD_max=-1,CFL=-1,TFL=-1,UFL=-1,FL_05_3=-1,FMA=-1)
    VVP_metrics_CBD=rep(-1,100)
    VVP_metrics=c(VVP_metrics,VVP_metrics_CBD)
    names(VVP_metrics)=c("Profil_Type","Profil_Type_L","threshold","CBH","FSG","Top_Fuel","H_Bush","continuity","VCI_PAD","VCI_lidr","entropy_lidr","PAI_tot","CBD_max","CFL","TFL","UFL","FL_05_3","FMA",paste0("CBD_",rep(1:100)))
    if(datatype=="Plot"){ 
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
  
  ### LMA from g/cm² to kg.m2
  LMA=mean(LMA,na.rm=T)/1000
  
  ## Partition of fuel surface (fine branch vs leaves) ----
  ### Wood density (kg/m3)
  WD=mean(WD)
  ### Surface volume ratio (SVR: m²/m3) for 4mm diameter twigs (=> wood fuel) = 2.pi.r.l/pi.r².l = 2/r
  SVR=2/0.002 
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
  PAD_CBD_Profile=data.table::data.table(cbind(H=seq_layer),PAD=PAD[-1],CBD=CBD[-1])
  
  # 2. Work on profile to get FPT and fuel metrics ----
  
  ##  Define threshold when threshold is a proportion of CBD max----
  if(stringr::str_detect(threshold,"%")){
    threshold_prop=as.numeric( str_split(threshold,"%",simplify = T)[,1])/100
    threshold=max(PAD_CBD_Profile[H>1]$CBD)*threshold_prop
  }
  
  ### no data above 0.5m
  if(max(PAD_CBD_Profile$H)<0.5){  
    VVP_metrics=c(Profil_Type=-1,Profil_Type_L=-1,threshold=-1,CBH=-1,FSG=-1,Top_Fuel=-1,H_Bush=-1,continuity=-1,VCI_PAD=-1,VCI_lidr=-1,entropy_lidr=-1,PAI_tot=-1,CBD_max=-1,CFL=-1,TFL=-1,UFL=-1,FL_05_3=-1,FMA=-1)
    VVP_metrics_CBD=rep(-1,100)
    VVP_metrics=c(VVP_metrics,VVP_metrics_CBD)
    names(VVP_metrics)=c("Profil_Type","Profil_Type_L","threshold","CBH","FSG","Top_Fuel","H_Bush","continuity","VCI_PAD","VCI_lidr","entropy_lidr","PAI_tot","CBD_max","CFL","TFL","UFL","FL_05_3","FMA",paste0("CBD_",rep(1:100)))
    if(datatype=="Plot"){ 
      return(list(VVP_metrics,PAD_CBD_Profile))}
    if(datatype=="Pixel"){
      return(as.list(VVP_metrics))}
  }
  ## Get CBD roll mean to smooth the profiles & get the profile above CBD threshold  ----
  ### Organise roll mean CBD depending on number of 0.5m strata >3
  if(nrow(PAD_CBD_Profile)>3){
    PAD_CBD_Profile$CBD_rollM=data.table::frollmean(PAD_CBD_Profile$CBD,3,algo="exact")
    PAD_CBD_Profile$CBD_rollM[1:3]=PAD_CBD_Profile$CBD[1:3]
    PAD_CBD_Profile_threshold=PAD_CBD_Profile[H>1&CBD_rollM>threshold]
  }
  ### Organise roll mean CBD depending on number of 0.5m strata <= 3 
  if(nrow(PAD_CBD_Profile)<=3){
    PAD_CBD_Profile$CBD_rollM=PAD_CBD_Profile$CBD
    PAD_CBD_Profile_threshold=PAD_CBD_Profile[H>1&CBD_rollM>threshold]
  }
  ### No data 
  if(nrow(PAD_CBD_Profile_threshold)==0){
    VVP_metrics=c(Profil_Type=-1,Profil_Type_L=-1,threshold=-1,CBH=-1,FSG=-1,Top_Fuel=-1,H_Bush=-1,continuity=-1,VCI_PAD=-1,VCI_lidr=-1,entropy_lidr=-1,PAI_tot=-1,CBD_max=-1,CFL=-1,TFL=-1,UFL=-1,FL_05_3=-1,FMA=-1)
    VVP_metrics_CBD=rep(-1,100)
    VVP_metrics=c(VVP_metrics,VVP_metrics_CBD)
    names(VVP_metrics)=c("Profil_Type","Profil_Type_L","threshold","CBH","FSG","Top_Fuel","H_Bush","continuity","VCI_PAD","VCI_lidr","entropy_lidr","PAI_tot","CBD_max","CFL","TFL","UFL","FL_05_3","FMA",paste0("CBD_",rep(1:100)))
    if(datatype=="Plot"){ 
      return(list(VVP_metrics,PAD_CBD_Profile))}
    if(datatype=="Pixel"){
      return(as.list(VVP_metrics))}
  }
  
  ## Get number of discontinuity (FSG) of 1m or more ----
  shift_H=data.table::shift(PAD_CBD_Profile_threshold$H)
  shift_H[1]=1.25
  delta_layer=PAD_CBD_Profile_threshold$H-shift_H
  Discontinuity=delta_layer[which(delta_layer>0.5)]
  
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
    
    ##### If more than one conitnuities is above one keep the first = Complex : Multilayered = Profil_Type= 4
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
      CBH=min(PAD_CBD_Profile_threshold$H)
      FSG=CBH
      H_Bush=0
      
    }
    #### profil discontinue with understory strata
    if(min(PAD_CBD_Profile_threshold$H)==1.25){
      
      CBH=PAD_CBD_Profile_threshold$H[delta_ID]
      FSG=Discontinuity
      H_Bush=CBH-FSG
      if(Profil_Type_L==1){Profil_Type_L=2}
    }
    Top_Fuel=max(PAD_CBD_Profile_threshold$H)
    continuity = 0
  }
  
  # get metrics (above 0.5m)
  PAI_tot=sum(PAD_CBD_Profile[H>0.5]$PAD)*d
  VCI_PAD =-sum(PAD_CBD_Profile[H>0.5]$PAD*log(PAD_CBD_Profile[H>0.5]$PAD))/length(PAD_CBD_Profile[H>0.5]$PAD)
  VCI_lidr=VCI(Z[Z>0.5],zmax = max(Z))
  entropy_lidr=entropy(Z[Z>0.5],zmax = max(Z))
  CBD_max=max(PAD_CBD_Profile[H>1]$CBD_rollM)
  CFL=sum(PAD_CBD_Profile[H>0.5&H>CBH&H<Top_Fuel]$CBD_rollM)*d
  TFL=sum(PAD_CBD_Profile[H>0.5&H<Top_Fuel]$CBD_rollM)*d
  if(CBH==0){UFL=TFL}else(UFL=sum(PAD_CBD_Profile[H>0.5&H<CBH]$CBD_rollM)*d)
  FL_05_3=sum(PAD_CBD_Profile[H>0.5&H<=3]$CBD_rollM)*d
  
  VVP_metrics=c(Profil_Type,Profil_Type_L,threshold,CBH,FSG,Top_Fuel,H_Bush,continuity,VCI_PAD,VCI_lidr,entropy_lidr,PAI_tot,CBD_max,CFL,TFL,UFL,FL_05_3,FMA)
  VVP_metrics_CBD=rep(-1,100)
  VVP_metrics_CBD[1:length(PAD_CBD_Profile$CBD_rollM)]=PAD_CBD_Profile$CBD_rollM
  VVP_metrics=c(VVP_metrics,VVP_metrics_CBD)
  
  names(VVP_metrics)=c("Profil_Type","Profil_Type_L","threshold","CBH","FSG","Top_Fuel","H_Bush","continuity","VCI_PAD","VCI_lidr","entropy_lidr","PAI_tot","CBD_max","CFL","TFL","UFL","FL_05_3","FMA",paste0("CBD_",rep(1:100)))
  
  if(datatype=="Plot"){ 
    return(list(VVP_metrics,PAD_CBD_Profile))}
  if(datatype=="Pixel"){
    return(as.list(VVP_metrics))}
}
