library(sf)
library(terra)
library(data.table)

# Charger un shape des departement
dep <- st_read("D:/PostDocFire_RES/Other_Shp/France/departements-20180101-shp/departements-20180101.shp")
# Selectionner les departement
dep <- dep[which(dep$code_insee %in% c("33", "40")), ]
# Format des coordonées
dep <- st_transform(dep, "EPSG:2154")
# Charger le raster de la carte de typologie ONF france entiere
carte_France_ONF <- terra::rast("D:/PostDocFire_RES/Typologie/Raster_Sensbilite_France_Entiere/carte_France_ONF_agg_20.tif")
# Charger le fichier de correspondance entre les code ONF et les valeurs de LMA
code_classif <- fread("D:/PostDocFire_RES/Typologie/Raster_Sensbilite_France_Entiere/code_classif_LMA.txt")

# Associer la valeur LMA au raster de la typo ONF pour le departement concerné. Attention trop lourd pour le faire à l'echelle de la france entiére.
carte_France_ONF_dep <- crop(carte_France_ONF, ext(ext(dep)))
carte_France_ONF_dep$description <- code_classif$Description[match(terra::values(carte_France_ONF_dep$france26), code_classif$Code)]
carte_France_ONF_dep$LMA <- code_classif$LMA[match(terra::values(carte_France_ONF_dep$france26), code_classif$Code)]

# ecrire le raster
writeRaster(carte_France_ONF_dep, "D:/LMA_MAP_33_40.tif")
