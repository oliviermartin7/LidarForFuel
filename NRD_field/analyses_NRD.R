library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(gridExtra)
library(vioplot)
library(tidyverse)
library(grid)
library(ggpubr)
library(gridtext)
library(stringr)

rm(list = ls())

#  IMPORTATION DES DONNEES  ----

data_placettes = read.csv("C:/users/ateurnier/OneDrive - INRAE SharePoint SE/CDD/data_placettes.csv", sep = ";")
# data_placettes = subset(data_placettes, data_placettes$Num_Plac %in% all_plot_DFCI_recal_OM$Plot)

# tab_lidR = read.delim("C:/Users/ateurnier/OneDrive - INRAE SharePoint SE/CDD/tab_lidR.txt", sep =",")
tab_IGN = read.delim("C:/Users/ateurnier/OneDrive - INRAE SharePoint SE/CDD/tab_IGN.txt", sep =",")


## Nouvelles variables ----
# data_placettes = read.csv("C:/Users/ateurnier/OneDrive - INRAE SharePoint SE/CDD/clean_datas/archivedwl-959/FD_LIDAR_DFCI_04_05_8413_06_48_datas_vol.csv", sep = ";")
# data_placettes = separate(data_placettes, objet_datemodification, into = c("date", "heure"), sep =" ")
# data_placettes$protocole = ifelse(data_placettes$dep == 48, "Lozère", "pas Lozère")
# 
# data_placettes <- data_placettes %>%
#   mutate(Esp1_code = ifelse(Esp1_code == "chv", "CHV", Esp1_code))
# 
# data_placettes$date = as.Date(data_placettes$date, format="%d/%m/%Y")
# data_placettes$Date = as.Date(data_placettes$Date, format="%d/%m/%Y")
# data_placettes$diff_jours = abs(as.numeric(difftime(data_placettes$date, data_placettes$Date, units = "days")))
# 
# data_placettes_IGN = merge(all_plot_DFCI_recal_OM, data_placettes_IGN, by.y = "Num_Plac", by.x = "Plot")
# data_placettes_IGN$Num_Plac = data_placettes_IGN$Plot
# 
# for (i in 1:length(data_placettes$Num_Plac)){
#   if(data_placettes$diff_jours[i] > 182 & !is.na(data_placettes$diff_jours[i])){
#     data_placettes$dist_saison[i] = abs(data_placettes$diff_jours[i] - 365)
#   } else data_placettes$dist_saison[i] = data_placettes$diff_jours[i]
# }
# data_placettes$dist_saison = abs(data_placettes$diff_jours - 365)


data_placettes$decidu = NA

decidious = c( data_placettes$Latin_Name_supra_Dominant[1] , data_placettes$Latin_Name_supra_Dominant[5] , data_placettes$Latin_Name_supra_Dominant[26] , data_placettes$Latin_Name_supra_Dominant[32] , data_placettes$Latin_Name_supra_Dominant[33] , data_placettes$Latin_Name_supra_Dominant[36] , data_placettes$Latin_Name_supra_Dominant[54] , data_placettes$Latin_Name_supra_Dominant[97] , data_placettes$Latin_Name_supra_Dominant[153] , data_placettes$Latin_Name_supra_Dominant[178] , data_placettes$Latin_Name_supra_Dominant[258] , data_placettes$Latin_Name_supra_Dominant[316] , data_placettes$Latin_Name_supra_Dominant[337] , data_placettes$Latin_Name_supra_Dominant[343])

for (i in 1:length(data_placettes$decidu)){
  if(!is.na(data_placettes$Latin_Name_supra_Dominant[i])){
    if(data_placettes$Latin_Name_supra_Dominant[i] %in% decidious){
      data_placettes$decidu[i] = "OUI"
    } 
  }
}

data_placettes$feuillaison = NA

for(i in 1:length(data_placettes$feuillaison)){
  if(!is.na(data_placettes$Date.1[i])){
    if(as.numeric(str_split(data_placettes$Date.1[i], pattern = "-")[[1]][2])>=5 & as.numeric(str_split(data_placettes$Date.1[i], pattern = "-")[[1]][2])<=9){
      x=1
    }else x=0
  }
  if(!is.na(data_placettes$date[i])){
    if(as.numeric(str_split(data_placettes$date[i], pattern = "/")[[1]][2])>=5 & as.numeric(str_split(data_placettes$date[i], pattern = "/")[[1]][2])<=9){
      y=1
    }else y=0
  }
  if(exists("x") & exists("y")){
    if(x+y == 1 & x == 1){
      data_placettes$feuillaison[i] = "feuilles lidar"
    }
    if(x+y == 1 & x == 0){
      data_placettes$feuillaison[i] = "feuilles terrain"
    }
    if(x+y!=1){
      data_placettes$feuillaison[i] = "NON"
    }
  }
}

## MERGE NRD ET CALCULER RECOUV MOYEN ----

# tab_lidR$NRD = tab_lidR$NRD *100
# 
tab_IGN$NRD = tab_IGN$NRD  *100
# 
# tab = read.csv("C:/Users/ateurnier/OneDrive - INRAE SharePoint SE/CDD/analyses/Merge_ALS_FD_8413_04_05_4graphics.txt")
# tab$NRD = tab$NRD *100

# data_placettes$Recouv005 = NULL
# data_placettes$Recouv051 = NULL
# data_placettes$Recouv12 = NULL
# data_placettes$Recouv23 = NULL
# data_placettes$Recouv34 = NULL
# data_placettes$Recouv45 = NULL
# data_placettes$Recouv5 = NULL
# 
# for (i in 1:length(data_placettes$Num_Plac)){
#   data_placettes$Recouv005[i] = mean(c(data_placettes$Recouv005m[i], data_placettes$Recouv005m2[i],data_placettes$Recouv005m23[i],data_placettes$Recouv005m234[i]))*10
#   data_placettes$Recouv051[i] = mean(c(data_placettes$Recouv051m[i], data_placettes$Recouv051m2[i],data_placettes$Recouv051m23[i],data_placettes$Recouv052m[i]))*10
#   data_placettes$Recouv12[i] = mean(c(data_placettes$Recouv12m[i],data_placettes$Recouv12m2[i],data_placettes$Recouv12m23[i],data_placettes$Recouv12m234[i]))*10
#   data_placettes$Recouv23[i] = mean(c(data_placettes$Recouv23m[i],data_placettes$Recouv23m2[i],data_placettes$Recouv23m23[i],data_placettes$Recouv23m234[i]))*10
#   data_placettes$Recouv34[i] = mean(c(data_placettes$Recouv34m[i],data_placettes$Recouv34m2[i],data_placettes$Recouv34m23[i],data_placettes$Recouv34m234[i]))*10
#   data_placettes$Recouv45[i] = mean(c(data_placettes$Recouv45m[i],data_placettes$Recouv45m2[i],data_placettes$Recouv45m23[i],data_placettes$Recouv45m234[i]))*10
#   data_placettes$Recouv5[i] = mean(c(data_placettes$Recouv5m[i],data_placettes$Recouv5m2[i],data_placettes$Recouv5m23[i],data_placettes$Recouv5m234[i]))*10
# }


# tab_lidR_2 = cbind(tab_lidR[, c("PlotID", "strata_ONF", "NRD")])
# tab_lidR_3 = tab_lidR_2 %>% pivot_wider(names_from = strata_ONF, values_from = NRD, values_fn = mean)

tab_IGN_2 = cbind(tab_IGN[, c("PlotID", "strata_ONF", "NRD")])
tab_IGN_3 = tab_IGN_2 %>% pivot_wider(names_from = strata_ONF, values_from = NRD, values_fn = mean)

# tab_2 = cbind(tab[, c("Plot_ID", "Strata", "NRD")])
# tab_2 = tab_2 %>% pivot_wider(names_from = Strata, values_from = NRD, values_fn = mean)
# 
# tab_3 = cbind(tab[, c("Plot_ID", "Strata", "Cover")])
# tab_3 = tab_3 %>% pivot_wider(names_from = Strata, values_from = Cover, values_fn = mean)
# 
# 
# tab_lidR_3$`0-0.5m` = tab_lidR_3$`0-0.5m`/ max(tab_lidR_3$`0-0.5m`)*100
# tab_lidR_3$`0.5-1m` = tab_lidR_3$`0.5-1m`/ max(tab_lidR_3$`0.5-1m`)*100
# tab_lidR_3$`1-2m` = tab_lidR_3$`1-2m`/ max(tab_lidR_3$`1-2m`)*100
# tab_lidR_3$`2-3m` = tab_lidR_3$`2-3m`/ max(tab_lidR_3$`2-3m`)*100
# tab_lidR_3$`3-4m` = tab_lidR_3$`3-4m`/ max(tab_lidR_3$`3-4m`)*100
# tab_lidR_3$`4-5m` = tab_lidR_3$`4-5m`/ max(tab_lidR_3$`4-5m`)*100
# tab_lidR_3$`>5m` = tab_lidR_3$`>5m`/ max(tab_lidR_3$`>5m`)*100

data_placettes_IGN = merge(data_placettes, tab_IGN_3, by.x = "Num_Plac", by.y = "PlotID")
# data_placettes_lidR = merge(data_placettes, tab_lidR_3, by.x = "Num_Plac", by.y = "PlotID")
# data_placettes_Olivier = merge(data_placettes, tab_2, by.x = "Num_Plac", by.y = "Plot_ID")

# data_placettes_lidR2 = data_placettes_lidR[which(!is.na(data_placettes_lidR$diff_jours)),]
# data_placettes_lidR_2 = data_placettes_lidR[which(!is.na(data_placettes_lidR$diff_jours)),]

data_placettes_IGN_2 = data_placettes_IGN[which(!is.na(data_placettes_IGN$diff_jours)),]


# data_placettes_lidR = subset(data_placettes_lidR, data_placettes_lidR$Quality < 4)
# data_placettes_IGN = subset(data_placettes_IGN, data_placettes_IGN$Quality < 4)


# ETUDE DES EFFETS ----

## DEPARTEMENT ----

x = data_placettes_IGN
main = "Effet du département, n = 353"

x$dep = as.character(x$dep)

p1 = ggplot(x) + aes(x= `0-0.5m`, y = Recouv005, color = dep) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7, 0.6), label.x = 0.9) + ggtitle("0-0.5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p2 = ggplot(x) + aes(x= `0.5-1m`, y = Recouv051, color = dep) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7, 0.6), label.x = 0.9) + ggtitle("0.5-1m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p3 = ggplot(x) + aes(x= `1-2m`, y = Recouv12, color = dep) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7, 0.6), label.x = 0.9) + ggtitle("1-2m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p4 = ggplot(x) + aes(x= `2-3m`, y = Recouv23, color = dep) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7, 0.6), label.x = 0.9) + ggtitle("2-3m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p5 = ggplot(x) + aes(x= `3-4m`, y = Recouv34, color = dep) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7, 0.6), label.x = 0.9) + ggtitle("3-4m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p6 = ggplot(x) + aes(x= `4-5m`, y = Recouv45, color = dep) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7, 0.6), label.x = 0.9) + ggtitle("4-5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p7 = ggplot(x) + aes(x= `>5m`, y = Recouv5, color = dep) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7, 0.6), label.x = 0.1) + ggtitle(">5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))


grid.arrange(p1,p2,p3,p4, p5, p6, p7,  ncol=3, nrow = 3, top = main, left = "Fiedl cover (%)", bottom = "ALS NRD (%)")


## PROTOCOLE ----

results_protocole = data.frame(matrix(NA, ncol = 3, nrow = 7))
colnames(results_protocole) = c( "Lozère", "pas Lozère", "Intéraction")
rownames(results_protocole) = c("005", "051", "12", "23", "34", "45", "5")

lozere = subset(data_placettes_IGN, data_placettes_IGN$protocole == "Lozère")
pas_lozere = subset(data_placettes_IGN, data_placettes_IGN$protocole == "pas Lozère")

for (i in 1:length(results_protocole)){
  if( i == 1){
    x = lozere
  } else if (i == 2){
    x = pas_lozere
  }
  lm005 = lm(x[,"0-0.5m"] ~ x$Recouv005)
  lm051 = lm(x[,"0.5-1m"] ~  x$Recouv051)
  lm12 = lm(x[,"1-2m"] ~  x$Recouv12)
  lm23 = lm( x[,"2-3m"] ~  x$Recouv23)
  lm34 = lm( x[,"3-4m"] ~  x$Recouv34)
  lm45 = lm( x[,"4-5m"] ~  x$Recouv45)
  lm5 = lm( x[,">5m"] ~  x$Recouv5)


  results_protocole[1,i] = summary(lm005)[["r.squared"]]
  results_protocole[2,i] = summary(lm051)[["r.squared"]]
  results_protocole[3,i] = summary(lm12)[["r.squared"]]
  results_protocole[4,i] = summary(lm23)[["r.squared"]]
  results_protocole[5,i] = summary(lm34)[["r.squared"]]
  results_protocole[6,i] = summary(lm45)[["r.squared"]]
  results_protocole[7,i] = summary(lm5)[["r.squared"]]
  
  if(i==3){
    x = data_placettes_IGN
    lm005 = lm(x[,"0-0.5m"] ~ x$Recouv005:x$protocole)
    lm051 = lm(x[,"0.5-1m"] ~  x$Recouv051:x$protocole)
    lm12 = lm(x[,"1-2m"] ~  x$Recouv12:x$protocole)
    lm23 = lm( x[,"2-3m"] ~  x$Recouv23:x$protocole)
    lm34 = lm( x[,"3-4m"] ~  x$Recouv34:x$protocole)
    lm45 = lm( x[,"4-5m"] ~  x$Recouv45:x$protocole)
    lm5 = lm( x[,">5m"] ~  x$Recouv5:x$protocole)
    
    
    results_protocole[1,i] = anova(lm005)$`Pr(>F)`[1]
    results_protocole[2,i] = anova(lm051)$`Pr(>F)`[1]
    results_protocole[3,i] = anova(lm12)$`Pr(>F)`[1]
    results_protocole[4,i] = anova(lm23)$`Pr(>F)`[1]
    results_protocole[5,i] = anova(lm34)$`Pr(>F)`[1]
    results_protocole[6,i] = anova(lm45)$`Pr(>F)`[1]
    results_protocole[7,i] = anova(lm5)$`Pr(>F)`[1]
  }
}



x = data_placettes_IGN
main = "Effet du protocole, n = 353"

p1 = ggplot(x) + aes(x= `0-0.5m`, y = Recouv005, color = protocole) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8), label.x = 0.9) + ggtitle("0-0.5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p2 = ggplot(x) + aes(x= `0.5-1m`, y = Recouv051, color = protocole) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8), label.x = 0.9) + ggtitle("0.5-1m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p3 = ggplot(x) + aes(x= `1-2m`, y = Recouv12, color = protocole) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8), label.x = 0.9) + ggtitle("1-2m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p4 = ggplot(x) + aes(x= `2-3m`, y = Recouv23, color = protocole) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8), label.x = 0.9) + ggtitle("2-3m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p5 = ggplot(x) + aes(x= `3-4m`, y = Recouv34, color = protocole) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8), label.x = 0.9) + ggtitle("3-4m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p6 = ggplot(x) + aes(x= `4-5m`, y = Recouv45, color = protocole) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8), label.x = 0.9) + ggtitle("4-5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p7 = ggplot(x) + aes(x= `>5m`, y = Recouv5, color = protocole) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8), label.x = 0.1) + ggtitle(">5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))


grid.arrange(p1,p2,p3,p4, p5, p6, p7,  ncol=3, nrow = 3, top = main, left = "Fiedl cover (%)", bottom = "ALS NRD (%)")

data_placettes_IGN = subset(data_placettes_IGN, data_placettes_IGN$dep != 48)
# data_placettes_lidR = subset(data_placettes_lidR, data_placettes_lidR$dep != 48)
# data_placettes_lidR_2 = subset(data_placettes_lidR_2, data_placettes_lidR_2$dep != 48)
data_placettes_IGN_2 = subset(data_placettes_IGN_2, data_placettes_IGN_2$dep != 48)
# data_placettes_lidR2 = subset(data_placettes_lidR2, data_placettes_lidR2$dep != 48)


## CLASSIFICATION

# results_classif = data.frame(matrix(NA, ncol = 2, nrow = 7))
# colnames(results_classif) = c( "IGN", "LidR")
# rownames(results_classif) = c("005", "051", "12", "23", "34", "45", "5")
# 
# for (i in 1:length(results_classif)){
#   if( i == 1){
#     x = data_placettes_IGN
#     main = "Classification IGN, n = 296"
#   } else if (i == 2){
#     x = data_placettes_lidR
#     main = "Classification lidR, n = 296"
#   }
#   lm005 = lm(x[,"0-0.5m"] ~ x$Recouv005)
#   lm051 = lm(x[,"0.5-1m"] ~  x$Recouv051)
#   lm12 = lm(x[,"1-2m"] ~  x$Recouv12)
#   lm23 = lm( x[,"2-3m"] ~  x$Recouv23)
#   lm34 = lm( x[,"3-4m"] ~  x$Recouv34)
#   lm45 = lm( x[,"4-5m"] ~  x$Recouv45)
#   lm5 = lm( x[,">5m"] ~  x$Recouv5)
#   
#   
#   results_classif[1,i] = summary(lm005)[["adj.r.squared"]]
#   results_classif[2,i] = summary(lm051)[["adj.r.squared"]]
#   results_classif[3,i] = summary(lm12)[["adj.r.squared"]]
#   results_classif[4,i] = summary(lm23)[["adj.r.squared"]]
#   results_classif[5,i] = summary(lm34)[["adj.r.squared"]]
#   results_classif[6,i] = summary(lm45)[["adj.r.squared"]]
#   results_classif[7,i] = summary(lm5)[["adj.r.squared"]]
#   
#   p1 = ggplot(x) + aes(x= `0-0.5m`, y = Recouv005) + geom_point() + geom_smooth(method = "lm") +
#     stat_poly_eq(use_label("R2"),  label.y = 70, label.x = 60) + ggtitle("0-0.5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))
#   
#   p2 = ggplot(x) + aes(x= `0.5-1m`, y = Recouv051) + geom_point() + geom_smooth(method = "lm") +
#     stat_poly_eq(use_label("R2"),  label.y = 70, label.x = 60) + ggtitle("0.5-1m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))
#   
#   p3 = ggplot(x) + aes(x= `1-2m`, y = Recouv12) + geom_point() + geom_smooth(method = "lm") +
#     stat_poly_eq(use_label("R2"),  label.y = 70, label.x = 60) + ggtitle("1-2m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))
#   
#   p4 = ggplot(x) + aes(x= `2-3m`, y = Recouv23) + geom_point() + geom_smooth(method = "lm") +
#     stat_poly_eq(use_label("R2"),  label.y = 70, label.x = 60) + ggtitle("2-3m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))
#   
#   p5 = ggplot(x) + aes(x= `3-4m`, y = Recouv34) + geom_point() + geom_smooth(method = "lm") +
#     stat_poly_eq(use_label("R2"),  label.y = 70, label.x = 60) + ggtitle("3-4m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))
#   
#   p6 = ggplot(x) + aes(x= `4-5m`, y = Recouv45) + geom_point() + geom_smooth(method = "lm") +
#     stat_poly_eq(use_label("R2"),  label.y = 70, label.x = 60) + ggtitle("4-5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))
#   
#   p7 = ggplot(x) + aes(x= `>5m`, y = Recouv5) + geom_point() + geom_smooth() +
#     stat_poly_eq(use_label("R2"),  label.y = 70, label.x = 60) + ggtitle(">5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))
#   
#   
#   grid.arrange(p1,p2,p3,p4, p5, p6, p7,  ncol=3, nrow = 3, top = main, left = "Fiedl cover (%)", bottom = "ALS NRD (%)")
# }

# par(mfrow = c(2,4))
# 
# for (i in 1:7){
#   x <- c("0-0.5m", "0.5-1m", "1-2m", "2-3m", "3-4m", "4-5m", ">5m")
#   y <- c("005", "051", "12", "23", "34", "45", "5")
#   
#   response_var <- x[i]  # Sélectionne la variable réponse en fonction de la colonne
#   predictor_var <- paste("Recouv", y[i], sep = "")  # Sélectionne la variable prédicteur en fonction de la ligne
#   
#   
#   lm_lidR <- lm(data_placettes_lidR[, response_var] ~ data_placettes_lidR[, predictor_var])
#   lm_IGN <- lm(data_placettes_IGN[, response_var] ~ data_placettes_IGN[, predictor_var])
#   
#   vioplot(
#     list(lm_lidR$residuals, lm_IGN$residuals),
#     names = c("lidR", "IGN"),
#     main = paste( x[i]),
#     col = "grey",  # Couleurs des violons
#     ylab = "Résidus"
#   )
# }


## PRESTATAIRE ----

x = subset(data_placettes_IGN, data_placettes_IGN$prestataire == "APEI_AVINEON" | data_placettes_IGN$prestataire == "SINTEGRA")
main = "Effet du prestataire, n = 296"

p1 = ggplot(x) + aes(x= `0-0.5m`, y = Recouv005, color = prestataire) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8), label.x = 0.9) + ggtitle("0-0.5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p2 = ggplot(x) + aes(x= `0.5-1m`, y = Recouv051, color = prestataire) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8), label.x = 0.9) + ggtitle("0.5-1m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p3 = ggplot(x) + aes(x= `1-2m`, y = Recouv12, color = prestataire) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8), label.x = 0.9) + ggtitle("1-2m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p4 = ggplot(x) + aes(x= `2-3m`, y = Recouv23, color = prestataire) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8), label.x = 0.9) + ggtitle("2-3m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p5 = ggplot(x) + aes(x= `3-4m`, y = Recouv34, color = prestataire) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8), label.x = 0.9) + ggtitle("3-4m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p6 = ggplot(x) + aes(x= `4-5m`, y = Recouv45, color = prestataire) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8), label.x = 0.9) + ggtitle("4-5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p7 = ggplot(x) + aes(x= `>5m`, y = Recouv5, color = prestataire) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8), label.x = 0.1) + ggtitle(">5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))


grid.arrange(p1,p2,p3,p4, p5, p6, p7,  ncol=3, nrow = 3, top = main, left = "Fiedl cover (%)", bottom = "ALS NRD (%)")


results_prestataire = data.frame(matrix(NA, ncol = 3, nrow = 7))
colnames(results_prestataire) = c( "apei_avineon", "sintegra", "intéraction")
rownames(results_prestataire) = c("005", "051", "12", "23", "34", "45", "5")

apei_avineon = subset(data_placettes_IGN, data_placettes_IGN$prestataire == "APEI_AVINEON")
sintegra = subset(data_placettes_IGN, data_placettes_IGN$prestataire == "SINTEGRA")

par(mfrow = c(2,4))

for (i in 1:length(results_prestataire)){
  if( i == 1){
    x = apei_avineon
    main = "Apei Avineon, n = 244"
  } else if (i == 2){
    x = sintegra
    main = "Sintegra, n = 52"
  }
  lm005 = lm(x[,"0-0.5m"] ~ x$Recouv005)
  lm051 = lm(x[,"0.5-1m"] ~  x$Recouv051)
  lm12 = lm(x[,"1-2m"] ~  x$Recouv12)
  lm23 = lm( x[,"2-3m"] ~  x$Recouv23)
  lm34 = lm( x[,"3-4m"] ~  x$Recouv34)
  lm45 = lm( x[,"4-5m"] ~  x$Recouv45)
  lm5 = lm( x[,">5m"] ~  x$Recouv5)


  results_prestataire[1,i] = summary(lm005)[["r.squared"]]
  results_prestataire[2,i] = summary(lm051)[["r.squared"]]
  results_prestataire[3,i] = summary(lm12)[["r.squared"]]
  results_prestataire[4,i] = summary(lm23)[["r.squared"]]
  results_prestataire[5,i] = summary(lm34)[["r.squared"]]
  results_prestataire[6,i] = summary(lm45)[["r.squared"]]
  results_prestataire[7,i] = summary(lm5)[["r.squared"]]

  if(i==3){
    x = data_placettes_IGN
    lm005 = lm(x[,"0-0.5m"] ~ x$Recouv005:x$prestataire)
    lm051 = lm(x[,"0.5-1m"] ~  x$Recouv051:x$prestataire)
    lm12 = lm(x[,"1-2m"] ~  x$Recouv12:x$prestataire)
    lm23 = lm( x[,"2-3m"] ~  x$Recouv23:x$prestataire)
    lm34 = lm( x[,"3-4m"] ~  x$Recouv34:x$prestataire)
    lm45 = lm( x[,"4-5m"] ~  x$Recouv45:x$prestataire)
    lm5 = lm( x[,">5m"] ~  x$Recouv5:x$prestataire)
    
    
    results_prestataire[1,i] = anova(lm005)$`Pr(>F)`[1]
    results_prestataire[2,i] = anova(lm051)$`Pr(>F)`[1]
    results_prestataire[3,i] = anova(lm12)$`Pr(>F)`[1]
    results_prestataire[4,i] = anova(lm23)$`Pr(>F)`[1]
    results_prestataire[5,i] = anova(lm34)$`Pr(>F)`[1]
    results_prestataire[6,i] = anova(lm45)$`Pr(>F)`[1]
    results_prestataire[7,i] = anova(lm5)$`Pr(>F)`[1]
  }
}

## PHENOLOGIE ----

par(mfrow = c(2,4))

for (i in 1:7){
  x <- c("0-0.5m", "0.5-1m", "1-2m", "2-3m", "3-4m", "4-5m", ">5m")
  y <- c("005", "051", "12", "23", "34", "45", "5")
  
  response_var <- x[i]  # Sélectionne la variable réponse en fonction de la colonne
  predictor_var <- paste("Recouv", y[i], sep = "")  # Sélectionne la variable prédicteur en fonction de la ligne
  
  
  lm_pheno <- lm(data_placettes_IGN_2[, response_var] ~ data_placettes_IGN_2[, predictor_var])
  
  p = ggplot() + aes(y = abs(lm_pheno$residuals), x = data_placettes_IGN_2$dist_saison) + geom_point()+ geom_smooth(method = "lm") +
    stat_poly_eq(use_label("R2")) + ylab("Résidus") + xlab("Distance phénologique") + ggtitle(x[i])
  print(p)
}

## DISTANCE TEMPS ----

par(mfrow = c(2,4))

for (i in 1:7){
  x <- c("0-0.5m", "0.5-1m", "1-2m", "2-3m", "3-4m", "4-5m", ">5m")
  y <- c("005", "051", "12", "23", "34", "45", "5")
  
  response_var <- x[i]  # Sélectionne la variable réponse en fonction de la colonne
  predictor_var <- paste("Recouv", y[i], sep = "")  # Sélectionne la variable prédicteur en fonction de la ligne
  
  
  lm_pheno <- lm(data_placettes_IGN_3[, response_var] ~ data_placettes_IGN_3[, predictor_var])
  
  p = ggplot() + aes(y = abs(lm_pheno$residuals), x = data_placettes_IGN_3$diff_jours) + geom_point()+ geom_smooth(method = "lm") +
    stat_poly_eq(use_label("R2")) + ylab("Résidus") + xlab("Différence en jours") + ggtitle(x[i])
  print(p)
}

## SCORE QUALITY ----

results_quality = data.frame(matrix(NA, ncol = 5, nrow = 7))
colnames(results_quality) = c( "1", "2", "3", "4", "interaction")
rownames(results_quality) = c("005", "051", "12", "23", "34", "45", "5")

qual1 = subset(data_placettes_IGN, data_placettes_IGN$Quality == 1)
qual12 = subset(data_placettes_IGN, data_placettes_IGN$Quality == 2)
qual123 = subset(data_placettes_IGN, data_placettes_IGN$Quality == 3)
qual1234 = subset(data_placettes_IGN, data_placettes_IGN$Quality == 4 | data_placettes_IGN$Quality == 5)

for (i in 1:length(results_quality)){
  if( i == 1){
    x = qual1
    main = "Qualité 1, n = 101"
  } else if (i == 2){
    x = qual12
    main = "Qualité 1 et 2, n = 185"
  }else if (i == 3){
    x = qual123
    main = "Qualité 1, 2 et 3, n = 228"
  }else if (i == 4){
    x = qual1234
    main = "Toutes qualités, n = 296"
  }

  lm005 = lm(x[,"0-0.5m"] ~ x$Recouv005)
  lm051 = lm(x[,"0.5-1m"] ~  x$Recouv051)
  lm12 = lm(x[,"1-2m"] ~  x$Recouv12)
  lm23 = lm( x[,"2-3m"] ~  x$Recouv23)
  lm34 = lm( x[,"3-4m"] ~  x$Recouv34)
  lm45 = lm( x[,"4-5m"] ~  x$Recouv45)
  lm5 = lm( x[,">5m"] ~  x$Recouv5)


  results_quality[1,i] = summary(lm005)[["r.squared"]]
  results_quality[2,i] = summary(lm051)[["r.squared"]]
  results_quality[3,i] = summary(lm12)[["r.squared"]]
  results_quality[4,i] = summary(lm23)[["r.squared"]]
  results_quality[5,i] = summary(lm34)[["r.squared"]]
  results_quality[6,i] = summary(lm45)[["r.squared"]]
  results_quality[7,i] = summary(lm5)[["r.squared"]]

  if(i==5){
    x = data_placettes_IGN
    lm005 = lm(x[,"0-0.5m"] ~ x$Recouv005:x$Quality)
    lm051 = lm(x[,"0.5-1m"] ~  x$Recouv051:x$Quality)
    lm12 = lm(x[,"1-2m"] ~  x$Recouv12:x$Quality)
    lm23 = lm( x[,"2-3m"] ~  x$Recouv23:x$Quality)
    lm34 = lm( x[,"3-4m"] ~  x$Recouv34:x$Quality)
    lm45 = lm( x[,"4-5m"] ~  x$Recouv45:x$Quality)
    lm5 = lm( x[,">5m"] ~  x$Recouv5:x$Quality)
    
    
    results_quality[1,i] = anova(lm005)$`Pr(>F)`[1]
    results_quality[2,i] = anova(lm051)$`Pr(>F)`[1]
    results_quality[3,i] = anova(lm12)$`Pr(>F)`[1]
    results_quality[4,i] = anova(lm23)$`Pr(>F)`[1]
    results_quality[5,i] = anova(lm34)$`Pr(>F)`[1]
    results_quality[6,i] = anova(lm45)$`Pr(>F)`[1]
    results_quality[7,i] = anova(lm5)$`Pr(>F)`[1]
  }
}

x = data_placettes_IGN
x$Quality[which(x$Quality == 5)] = 4
x$Quality = as.character(x$Quality)
main = "Effet de la qualité de recalage, n = 296"

p1 = ggplot(x) + aes(x= `0-0.5m`, y = Recouv005, color = Quality) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7, 0.6), label.x = 0.9) + ggtitle("0-0.5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p2 = ggplot(x) + aes(x= `0.5-1m`, y = Recouv051, color = Quality) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7, 0.6), label.x = 0.9) + ggtitle("0.5-1m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p3 = ggplot(x) + aes(x= `1-2m`, y = Recouv12, color = Quality) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7, 0.6), label.x = 0.9) + ggtitle("1-2m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p4 = ggplot(x) + aes(x= `2-3m`, y = Recouv23, color = Quality) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7, 0.6), label.x = 0.9) + ggtitle("2-3m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p5 = ggplot(x) + aes(x= `3-4m`, y = Recouv34, color = Quality) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7, 0.6), label.x = 0.9) + ggtitle("3-4m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p6 = ggplot(x) + aes(x= `4-5m`, y = Recouv45, color = Quality) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7, 0.6), label.x = 0.9) + ggtitle("4-5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p7 = ggplot(x) + aes(x= `>5m`, y = Recouv5, color = Quality) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7, 0.6), label.x = 0.1) + ggtitle(">5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))


grid.arrange(p1,p2,p3,p4, p5, p6, p7,  ncol=3, nrow = 3, top = main, left = "Fiedl cover (%)", bottom = "ALS NRD (%)")


## ESSENCES ----

data_placettes_tree <- data_placettes_IGN %>%
  group_by(Latin_Name_supra_Dominant) %>%
  filter(n() >= 5)

data_placettes_tree = subset(data_placettes_tree, !is.na(data_placettes_tree$Latin_Name_supra_Dominant))


plots <- data_placettes_tree %>%
  ggplot() +
  geom_point(aes(x = `1-2m`, y = Recouv12, color = "1-2m")) +  # Ajouter des points
  geom_smooth(aes(x = `1-2m`, y = Recouv12), method = "lm", color ="aquamarine4", linetype = "solid") +
  stat_cor(aes( x = `1-2m`, y = Recouv12, label = ..rr.label..), color = "aquamarine4",
           label.x = 60, label.y = 0) +
  geom_point(aes(x = `>5m`, y = Recouv5, color = ">5m")) +
  geom_smooth(aes(x = `>5m`, y = Recouv5), method = "lm", linetype = "solid", color ="darkorange3") +
  stat_cor(aes( x = `>5m`, y = Recouv5, label = ..rr.label..), color = "darkorange3",
           label.x = 60, label.y = 15) +
  facet_wrap(~Latin_Name_supra_Dominant) +  # Facetter par Esp1_code
  labs(x = "ALS NRD (%)", y = "Field cover (%)") +
  theme_classic()+
  # Ajouter ceci
  scale_color_manual(name = "Strata",
                     values = c("1-2m" = "aquamarine4", ">5m" = "darkorange3")) +
  theme(legend.position = "right")


print(plots)


## FEUILLAISON ----

decidu = subset(data_placettes_IGN, data_placettes_IGN$decidu == "OUI" & !is.na(data_placettes_IGN$feuillaison))
main = "Effet de la différence de feuillaison entre le terrain et le vol LiDAR, n = 127"

x = decidu
x$feuillaison[which(x$feuillaison == "NON")] = "pas de différence"

p1 = ggplot() + aes(x= x$`0-0.5m`, y = x$Recouv005, color = x$feuillaison) + geom_point() + geom_smooth(method = "lm") +
   stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7), label.x = 0.9) + ggtitle("0-0.5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p2 = ggplot(x) + aes(x= `0.5-1m`, y = Recouv051, color = feuillaison) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7), label.x = 0.9) + ggtitle("0.5-1m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p3 = ggplot(x) + aes(x= `1-2m`, y = Recouv12, color = feuillaison) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7), label.x = 0.9) + ggtitle("1-2m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p4 = ggplot(x) + aes(x= `2-3m`, y = Recouv23, color = feuillaison) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7), label.x = 0.9) + ggtitle("2-3m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p5 = ggplot(x) + aes(x= `3-4m`, y = Recouv34, color = feuillaison) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7), label.x = 0.9) + ggtitle("3-4m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p6 = ggplot(x) + aes(x= `4-5m`, y = Recouv45, color = feuillaison) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7), label.x = 0.9) + ggtitle("4-5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))

p7 = ggplot(x) + aes(x= `>5m`, y = Recouv5, color = feuillaison) + geom_point() + geom_smooth(method = "lm") +
  stat_poly_eq(use_label("R2"),  label.y = c(0.9, 0.8, 0.7), label.x = 0.1) + ggtitle(">5m") + xlab(NULL) + ylab(NULL) + xlim(c(0,100)) + ylim(c(0,100))


grid.arrange(p1,p2,p3,p4, p5, p6, p7,  ncol=3, nrow = 3, top = main, left = "Fiedl cover (%)", bottom = "ALS NRD (%)")


results_feuillaison = data.frame(matrix(NA, ncol = 5, nrow = 7))
colnames(results_feuillaison) = c( "Feuillaison LiDAR", "Feuillaison terrain", "Pas de différence", "Différence", "intéraction")
rownames(results_feuillaison) = c("005", "051", "12", "23", "34", "45", "5")

feuill_lidar = subset(decidu, feuillaison == "feuilles lidar")
feuill_terrain = subset(decidu, feuillaison == "feuilles terrain")
pas_diff = subset(decidu, feuillaison == "NON")
diff_feuill = subset(decidu, feuillaison == "feuilles lidar" | feuillaison == "feuilles terrain")


for (i in 1:length(results_feuillaison)){
  if( i == 1){
    x = feuill_lidar
    main = "Feuillaison LiDAR, n = 66"
  } else if (i == 2){
    x = feuill_terrain
    main = "Feuillaison terrain, n = 12"
  }else if (i == 3){
    x = pas_diff
    main = "Pas de différence, n = 19"
  }else if (i == 4){
    x = diff_feuill
    main = "Différence de feuillaison, n = 78"
  }

  lm005 = lm(x[,"0-0.5m"] ~ x$Recouv005)
  lm051 = lm(x[,"0.5-1m"] ~  x$Recouv051)
  lm12 = lm(x[,"1-2m"] ~  x$Recouv12)
  lm23 = lm( x[,"2-3m"] ~  x$Recouv23)
  lm34 = lm( x[,"3-4m"] ~  x$Recouv34)
  lm45 = lm( x[,"4-5m"] ~  x$Recouv45)
  lm5 = lm( x[,">5m"] ~  x$Recouv5)


  results_feuillaison[1,i] = summary(lm005)[["r.squared"]]
  results_feuillaison[2,i] = summary(lm051)[["r.squared"]]
  results_feuillaison[3,i] = summary(lm12)[["r.squared"]]
  results_feuillaison[4,i] = summary(lm23)[["r.squared"]]
  results_feuillaison[5,i] = summary(lm34)[["r.squared"]]
  results_feuillaison[6,i] = summary(lm45)[["r.squared"]]
  results_feuillaison[7,i] = summary(lm5)[["r.squared"]]

  if(i==5){
    x = data_placettes_IGN
    lm005 = lm(x[,"0-0.5m"] ~ x$Recouv005:x$feuillaison)
    lm051 = lm(x[,"0.5-1m"] ~  x$Recouv051:x$feuillaison)
    lm12 = lm(x[,"1-2m"] ~  x$Recouv12:x$feuillaison)
    lm23 = lm( x[,"2-3m"] ~  x$Recouv23:x$feuillaison)
    lm34 = lm( x[,"3-4m"] ~  x$Recouv34:x$feuillaison)
    lm45 = lm( x[,"4-5m"] ~  x$Recouv45:x$feuillaison)
    lm5 = lm( x[,">5m"] ~  x$Recouv5:x$feuillaison)
    
    
    results_feuillaison[1,i] = anova(lm005)$`Pr(>F)`[1]
    results_feuillaison[2,i] = anova(lm051)$`Pr(>F)`[1]
    results_feuillaison[3,i] = anova(lm12)$`Pr(>F)`[1]
    results_feuillaison[4,i] = anova(lm23)$`Pr(>F)`[1]
    results_feuillaison[5,i] = anova(lm34)$`Pr(>F)`[1]
    results_feuillaison[6,i] = anova(lm45)$`Pr(>F)`[1]
    results_feuillaison[7,i] = anova(lm5)$`Pr(>F)`[1]
  }
}

