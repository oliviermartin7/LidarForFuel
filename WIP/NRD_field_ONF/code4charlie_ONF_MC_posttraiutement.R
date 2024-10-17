

ls_files=list.files("D:/Subsample_CBD_20m/CBD_posttreated_MC_ONF/",pattern = ".tif",full.names = T)
ls_files=ls_files[1:20]

ls_raster=lapply(ls_files, rast)


###   Post traitement des .tif ----
Metrics=function(x){
  MFL=NA
  CFL=NA
  FL_0_1=NA
  GSFL=NA
  #MFL
  if(is.na(x[4])){
    MFL=NA
    CFL=NA
    FL_0_1=NA
  }
  if(is.na(x[5])==F){
    if(x[5]==0){
      MFL=sum(x[22:169],na.rm=T)*0.5
    }
    
    if(x[8]>0){
      MFL=sum(x[22:(20+round(x[8]*2))],na.rm=T)*0.5}}
  # CFL
  if(is.na(x[5])==F){
    if(x[5]==0){CFL=0}
    if(x[5]>0){
      CFL=sum(x[(20+round(x[5]*2)):169],na.rm=T)*0.5}}
  
  #FL_0_1
  FL_0_1=sum(x[20:21],na.rm=T)*0.5
  #GSFL
  if(is.na(x[6])==F){
    if(x[6]==0){GSFL=0}
    if(x[6]>0){
      GSFL=sum(x[(20+round(x[8]*2)):20+round(x[5]*2)],na.rm=T)*0.5}}
  
  return(c(FL_0_1=FL_0_1,MFL=MFL,CFL=CFL,GSFL=GSFL))
}


ls_files=list.files("D:/Subsample_CBD_20m/CDB_Subsample/",pattern = ".tif",full.names = T)

ls_raster=lapply(ls_files, rast)

for (i in 1:length(ls_raster)){
  print(i)
  x=ls_raster[[i]]
  x=subst(x,-1,NA)
  x=subst(x,Inf,NA)
  x_Metrics=app(x,Metrics)
  names(x_Metrics)=c("FL_0_1","MFL","CFL","GSFL")
  rast_new= c(x_Metrics$FL_0_1,x_Metrics$MFL,x_Metrics$GSFL,x_Metrics$CFL,x$TFL,x$FL_05_3,
              x$CBH,x$FSG,x$H_Bush,x$Height,
              x$Profil_Type_L,
              x$PAI_tot,x$VCI_PAD)
  ls_raster[[i]]=rast_new
}

# Cas du subsampling ----

for (i in 1:length(ls_raster)){
  writeRaster(ls_raster[[i]],paste0("D:/Subsample_CBD_20m/CBD_posttreated_MC_ONF/",i,".tif"))
}

test=rast("D:/Subsample_CBD_20m/CBD_posttreated_MC_ONF/1.tif")

# Cas des departements : merger les tiles ----

collec=sprc(x = ls_raster) # crée une collection de raster
merge(collec,filename = "D:/Aude/Map_Aude_Complet.tif",overwrite=T) # ecrit le .tif des dalles assemblés
test_crop=crop(collec,dep_04)


test_crop_subs=merge(test_crop,filename = "D:/test_crop_subs.tif",overwrite=T) 

resample(,dep_04,method="near")

test_2=c(dep_04,test_resamp)
test_2=aggregate(test_2,fact=4)
gc()

test_2_tab=data.table(as.data.frame(test_2,na.rm=T,xy=T))


# 

dep_04=rast("C:/Users/ormartin/OneDrive - INRAE SharePoint SE/CDD_Inge_Charlie/Carte/Carte_sensibilite_ONF/dep04_inrae/dep04_inrae/dep04_description_LMA.tif")


## ggplot des jolies distrib



distribut_CFL=Map_lidar_toevaluate_tab[description%in%c(sp_to_keep)] %>%
  ggplot(aes(y=reorder(description ,CFL_1   ,na.rm = TRUE),x=CFL_1      ,fill=stat(x))) +
  geom_density_ridges_gradient(scale = 3, quantile_lines=TRUE,quantile_fun=function(x,...)mean(x))+
  scale_fill_viridis_c(option="inferno")+   
  theme_few()+
  theme(axis.text.y = element_text(angle = 45,hjust=1),axis.title.y=element_blank())
