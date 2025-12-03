####################
### Ray tracing ####
####################

# description: Function to get parameters needed for PAD estimates by strata using a pseudo ray tracing approach on ALS pointcloud
# Input (for now) :
# las: A pretreated las/laz (functionfc_pretreatment of lidarforfuel)
# G: projection ratio
# DTM_table : DTM in table format (X,Y,Z of each cell in row)
# plot limits:
# xplotmin
# xplotmax
# yplotmin
# yplotmax
# zplotmax

# Output :
# Table with parameters of the strata: sumdelta; Ni; SumZi; PAD_MLE

# Details : the general idea consist in obtaining a precise estimations of the path length (hit and non-hit) inside volume (strata or pancakes in this case) where PAD has to b computed. The output are the different parameters (path lengths but also number of hits,...etc : see outputs) inside the strata needed to compute PAD with different methods at the strata scale

Vertical_strata_Ray_tracing=function(las,G=0.5,d=0.5,xplotmin,xplotmax,yplotmin,yplotmax,zplotmax,DTM_table,firstreturn=F){

  if(d=="ONF_Field"){
    breaks=c(0,0.5,1,2,3,4,5,zplotmax)

  }else{breaks=seq(0,zplotmax+d,d)}

  if(firstreturn==T){las=filter_first(las)}

las@data$norm_U=sqrt((las@data$X-las@data$Easting)^2+(las@data$Y-las@data$Northing)^2+(las@data$Zref-las@data$Elevation)^2)
las@data$Ux=(las@data$Easting-las@data$X)/las@data$norm_U
las@data$Uy=(las@data$Northing-las@data$Y)/las@data$norm_U
las@data$Uz=(las@data$Elevation-las@data$Zref)/las@data$norm_U

# Transform points underground to have them at DTM height ----
# get point underground
Zinf0=las@data[Z<0]

Zinf0$X=Zinf0$X-Zinf0$Z*(Zinf0$Ux/Zinf0$Uz)
Zinf0$Y=Zinf0$Y-Zinf0$Z*(Zinf0$Uy/Zinf0$Uz)
if(nrow(Zinf0)!=0){
Zinf0$Zref=DTM_tab[RANN::nn2(DTM_tab[,c(1,2)],Zinf0[,c("X","Y")],1)$nn.idx]$Z} # get Z from DTM

# replace Z < 0 in original las
las@data[Z<0]=Zinf0

# Treating hit outside plot (las_op) to find if they pass through the plot, what height they get in, what height the get out (delta Ze)  ----

# get point outside the plot

hit_out=las@data[X>xplotmax|Y<yplotmin|X<xplotmin|Y>yplotmax,c("X","Y","Zref","Ux","Uy","Uz")]

# get coordinates  of intersection of the ray with the bounding box of hit outside the bbox.
# run function on all hits outside (plot = 20 m, buffer = 20m) => Decrease buffer size and restrict it to the relevant sides of the plot to save computing time

#  coord_ray_hit_out (vector based and towMins => much faster than apply below)
hit_out$tmx_xpmin=((xplotmin-hit_out$X)/hit_out$Ux)+0.1
hit_out$tmx_xpmax=((xplotmax-hit_out$X)/hit_out$Ux)+0.1
hit_out$tmy_ypmin=((yplotmin-hit_out$Y)/hit_out$Uy)+0.1
hit_out$tmy_ypmax=((yplotmax-hit_out$Y)/hit_out$Uy)+0.1

# Awful... but avoid format issues
hit_out$min_tmx=-99999999
hit_out$min_tmy=-99999999

hit_out[tmx_xpmin<0&tmx_xpmax>0]$min_tmx=hit_out[tmx_xpmin<0&tmx_xpmax>0]$tmx_xpmax
hit_out[tmx_xpmin>0&tmx_xpmax<0]$min_tmx=hit_out[tmx_xpmin>0&tmx_xpmax<0]$tmx_xpmin
hit_out[tmx_xpmin>0&tmx_xpmax>0]$min_tmx=Rfast::rowMins(as.matrix(hit_out[tmx_xpmin>0&tmx_xpmax>0,7:8]),value=T)

hit_out[tmy_ypmin<0&tmy_ypmax>0]$min_tmy=hit_out[tmy_ypmin<0&tmy_ypmax>0]$tmy_ypmax
hit_out[tmy_ypmin>0&tmy_ypmax<0]$min_tmy=hit_out[tmy_ypmin>0&tmy_ypmax<0]$tmy_ypmin
hit_out[tmy_ypmin>0&tmy_ypmax>0]$min_tmy=Rfast::rowMins(as.matrix(hit_out[tmy_ypmin>0&tmy_ypmax>0,9:10]),value=T)

# Back to NA
hit_out[hit_out==-99999999]=NA


hit_out$t=-99999999
hit_out[is.na(min_tmx)==T]$t=hit_out[is.na(min_tmx)==T]$min_tmy
hit_out[is.na(min_tmy)==T]$t=hit_out[is.na(min_tmy)==T]$min_tmx

hit_out[is.na(min_tmy)==F&is.na(min_tmx)==F]$t=Rfast::rowMins(as.matrix(hit_out[is.na(min_tmy)==F&is.na(min_tmx)==F,c(11,12)]),value=T)
hit_out=hit_out[is.na(t)==F]


coord_ray_hitout=data.table(cbind(
  Xm=hit_out$X+hit_out$t*hit_out$Ux,
  Ym=hit_out$Y+hit_out$t*hit_out$Uy,
  Zrefm=hit_out$Z+hit_out$t*hit_out$Uz,
  Ux=hit_out$Ux,
  Uy=hit_out$Uy,
  Uz=hit_out$Uz
))

# version coord_ray_hit_out with apply ( ~35 seconds => too slow)
# coord_ray_hitout_2=apply(hit_out,1,function(x){
#
#   tmx=c((xplotmin-x[1])/x[4],((xplotmax-x[1])/x[4]))+0.1
#   tmy=c((yplotmin-x[2])/x[5],((yplotmax-x[2])/x[5]))+0.1
#   #
#   # test of line parameter in the parametrics equation of the line (Xl=X+tu;...)
#   t=c()
#   if(max(tmx)>0){
#     tx=min(tmx[tmx>0])
#     t[1]=tx
#   }
#
#   if(max(tmy)>0){
#     ty=min(tmy[tmy>0])
#     t[1+length(t)]=ty
#   }
#
#   if(length(t)==0){return(NULL)}
#   t=sort(t)[1]
#
#   Xm=x[1]+t*x[4]
#   Ym=x[2]+t*x[5]
#   Zrefm=x[3]+t*x[6]
#   return(cbind(Xm,Ym,Zrefm,Ux=x[4],Uy=x[5],Uz=x[6]))
#   }
#   ,simplify=F)
#
#
#  coord_ray_hitout_2=do.call("rbind",coord_ray_hitout_2)
#  coord_ray_hitout_2=data.table(coord_ray_hitout_2)

coord_ray_hitout=na.omit(coord_ray_hitout)

coord_ray_hitout$Zm=coord_ray_hitout$Zrefm-DTM_tab[RANN::nn2(DTM_tab[,1:2],coord_ray_hitout[,1:2],1)$nn.idx]$Z # get Z from DTM
coord_ray_hitout=coord_ray_hitout[Xm>=xplotmin&Xm<=xplotmax&Ym<=yplotmax&Ym>=yplotmin&Zm<=zplotmax]

# rename output
coord_ray_hitout=coord_ray_hitout[,c(1:3,7,4:6)]
names(coord_ray_hitout)=c("X_out_in","Y_out_in","Zref_out_in","Z_out_in","Ux" ,"Uy" , "Uz")

# What height ray outside the plot passing through plot get out of the plot ? ----
# Get which plane intersect the coord_ray_hitout
tmx=rbind((xplotmin-coord_ray_hitout$X_out_in)/coord_ray_hitout$Ux,(xplotmax-coord_ray_hitout$X_out_in)/coord_ray_hitout$Ux)-0.01
tmy=rbind((yplotmin-coord_ray_hitout$Y_out_in)/coord_ray_hitout$Uy,(yplotmax-coord_ray_hitout$Y_out_in)/coord_ray_hitout$Uy)-0.01
tmx_max=plyr::aaply(tmx,2,max)
tmy_max=plyr::aaply(tmy,2,max)
t=plyr::aaply(rbind(tmx_max,tmy_max),2,min)

# get coordinate of intersection
coord_ray_hitout$X_out_out=coord_ray_hitout$X+t*coord_ray_hitout$Ux
coord_ray_hitout$Y_out_out=coord_ray_hitout$Y+t*coord_ray_hitout$Uy
coord_ray_hitout$Zref_out_out=coord_ray_hitout$Zref_out_in+t*coord_ray_hitout$Uz

# get Zm (height where the point get out of the plot) from DTM
if(nrow(coord_ray_hitout)!=0){
  coord_ray_hitout$Z_out_out=coord_ray_hitout$Zref_out_out-DTM_tab[RANN::nn2(DTM_tab[,c(1,2)],coord_ray_hitout[,c("X_out_out","Y_out_out")],1)$nn.idx]$Z}

# Ray tracing inside the bbox  for hit outside
ls_rayouttracing=na.omit(apply(coord_ray_hitout,1,function(x){
  max_z=min(zplotmax,x[11])
  k=breaks[breaks>x[4]&breaks<max_z+d]
  if(length(k)==0)(return())
  ls_rayouttracing=cbind(k=k,delta=d/x[7])
  # get exact delta of the last layer when ray getting out of the bbox lower than maximum Z of bbox (max (k))
  if(ls_rayouttracing[nrow(ls_rayouttracing),1]>x[11]){
    ls_rayouttracing[nrow(ls_rayouttracing),2]=(ls_rayouttracing[nrow(ls_rayouttracing),1]-max_z)/x[7]
  }
  return(ls_rayouttracing)
}))

ls_rayouttracing=do.call("rbind",ls_rayouttracing)
ls_rayouttracing=data.table(ls_rayouttracing)
# get sumdeltai
sumdeltai_out=ls_rayouttracing[,.(sumdelta=sum(delta),N=length(delta)),by="k"]

# Treating points inside the plot to find where (which height) the ray get out of the plot ----
hit_in=las@data[X<xplotmax&X>xplotmin&Y<yplotmax&Y>yplotmin]

# Get which plane intersect the ray
tmx_xmin=((xplotmin-hit_in$X)/hit_in$Ux)-0.01
tmx_xmax=((xplotmax-hit_in$X)/hit_in$Ux)-0.01

tmy_ymin=((yplotmin-hit_in$Y)/hit_in$Uy)-0.01
tmy_ymax=((yplotmax-hit_in$Y)/hit_in$Uy)-0.01

tmx_max=pmax(tmx_xmin,tmx_xmax)
tmy_max=pmax(tmy_ymin,tmy_ymax)


t=pmin(tmx_max,tmy_max)

# get coordinate of intersection
Xm=hit_in$X+t*hit_in$Ux
Ym=hit_in$Y+t*hit_in$Uy
Zrefm=hit_in$Zref+t*hit_in$Uz

# get Zm (height where the point get out of the bbox) from DTM
Z_in_out=Zrefm-DTM_tab[RANN::nn2(DTM_tab[,c(1,2)],cbind(Xm,Ym),1)$nn.idx]$Z

k=breaks[findInterval(hit_in$Z,breaks,left.open = T)+1]
# Zi is the path lenght inside the strata where hit happened ( < d)
Zi=(k-hit_in$Z)/hit_in$Uz
Table_hit=data.table(cbind(k,Zi))
SumZi=Table_hit[,.(SumZi=sum(Zi),Ni=.N),by="k"]
SumZi=SumZi[order(k),]

# get a table for parameter of hit inside SumZi; Ni
# Table_khit=cbind(Ni=as.vector(table(Table_hit$k)),SumZi)
Table_khit=SumZi
Table_khit$N_ortho=cumsum(Table_khit$Ni)
Z_in_out=cbind(hit_in$Z,as.matrix(Z_in_out),hit_in$Uz)

# Ray tracing inside the bbox  for hit inside
ls_rayintracing=na.omit(apply(Z_in_out,1,function(x){
  # select the highest value between Z bbox or Z out
  max_z=min(zplotmax,x[2])
  k=breaks[breaks>=x[1]+d&breaks<max_z+d]
  if(length(k)==0)(return())

  ls_rayintracing=cbind(k=k,delta=d/x[3])
  # get exact delta of the last layer when ray getting out of the bbox lower than maximum Z of bbox (i.e max(k))
  if(ls_rayintracing[nrow(ls_rayintracing),1]>x[2]){
    ls_rayintracing[nrow(ls_rayintracing),2]=(ls_rayintracing[nrow(ls_rayintracing),1]-x[2])/x[3]
  }
  return(ls_rayintracing)
}))

# unlist matrix
ls_rayintracing=do.call("rbind",ls_rayintracing)
ls_rayintracing=data.table(ls_rayintracing)
# sum delta by k
sumdeltai_in=ls_rayintracing[,.(sumdelta=sum(delta),N=length(delta)),,by="k"]

# merge sumdelta for hit outside and hit inside
sumdeltai=rbind(sumdeltai_in,sumdeltai_out)
sumdeltai=sumdeltai[,.(sumdelta=sum(sumdelta),N=sum(N)),,by="k"]
sumdeltai=sumdeltai[order(k),]

# merge everything()
Table_hit$Dist_floor=hit_in$Z-(k-d)
param_k=merge(sumdeltai,Table_khit,by="k",all=T)
param_k[is.na(param_k)]=0
param_k[N_ortho==0]$N_ortho=param_k$N_ortho[1]
param_k$N=param_k$N+param_k$Ni
param_k$cos_theta=mean(las@data$Uz)
return(list(param_k,Table_hit))

}

