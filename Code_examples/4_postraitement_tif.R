library(terra)

# Post traitement et assemblage de plusieurs les tif de 500mx500m pour produire le raster de la carte final
ls_files <- list.files("~pathtofolderwithtif", pattern = ".tif", full.names = T)

# Post traitement pour surrpimer rempalcer les -1 par des NA et selectionner les varaible d'intérêt dans une liste regroupant l'ensemble des .tif
ls_raster <- list()
for (i in 1:length(ls_files)) {
  print(i)
  x <- rast(ls_files[i])
  x <- subst(x, -1, NA)
  x <- subst(x, Inf, NA)
  x <- x[[c("FL_0_1", "MFL", "GSFL", "CFL", "TFL", "FL_1_3", "CBH", "FSG", "H_Bush", "Height", "Profil_Type_L", "PAI_tot", "VCI_PAD", "date")]]
  ls_raster[[i]] <- x
}

# Faire une collection de raster à partir de la lsite
collec <- sprc(x = ls_raster)
# Merger la collection
my_map <- merge(collec)
my_map_raw <- my_map
# Ecrire le raster brut (c à d sans seuil de valeur)
writeRaster(my_map_raw, filename = "~pathforrawmap", overwrite = T)

# Mettre un seuil de valeur pour eviter les valeurs aberrantes
my_map$FL_0_1 <- clamp(my_map$FL_0_1, upper = quantile(values(my_map$FL_0_1), seq(0, 1, 0.01), na.rm = T)[100])
my_map$MFL <- clamp(my_map$MFL, upper = quantile(values(my_map$MFL), seq(0, 1, 0.01), na.rm = T)[100])
my_map$GSFL <- clamp(my_map$GSFL, upper = quantile(values(my_map$GSFL), seq(0, 1, 0.01), na.rm = T)[100])
my_map$CFL <- clamp(my_map$CFL, upper = quantile(values(my_map$CFL), seq(0, 1, 0.01), na.rm = T)[100])
my_map$TFL <- clamp(my_map$TFL, upper = quantile(values(my_map$TFL), seq(0, 1, 0.01), na.rm = T)[100])
my_map$FL_1_3 <- clamp(my_map$FL_1_3, upper = quantile(values(my_map$FL_1_3), seq(0, 1, 0.01), na.rm = T)[100])
my_map$CBH <- clamp(my_map$CBH, upper = quantile(values(my_map$CBH), seq(0, 1, 0.01), na.rm = T)[100])
my_map$FSG <- clamp(my_map$FSG, upper = quantile(values(my_map$FSG), seq(0, 1, 0.01), na.rm = T)[100])
my_map$H_Bush <- clamp(my_map$H_Bush, upper = quantile(values(my_map$H_Bush), seq(0, 1, 0.01), na.rm = T)[100])
my_map$Height <- clamp(my_map$Height, upper = quantile(values(my_map$Height), seq(0, 1, 0.01), na.rm = T)[100])
my_map$PAI_tot <- clamp(my_map$PAI_tot, upper = quantile(values(my_map$PAI_tot), seq(0, 1, 0.01), na.rm = T)[100])
my_map$VCI_PAD <- clamp(my_map$VCI_PAD, upper = quantile(values(my_map$VCI_PAD), seq(0, 1, 0.01), na.rm = T)[100])
my_map$FL_0_MFL <- my_map$FL_0_1 + my_map$MFL

# ecrire la map finale
writeRaster(my_map_raw, filename = "~pathforfinalmap", overwrite = T)
