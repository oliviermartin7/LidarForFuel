library(FactoMineR)
library(terra)
library(sf)
library(tidyr)
library(data.table)
library(ggpubr)
library(stringr)
library(ggthemes)



# Charger carte combustible de la zone/departement
Aquitaine_Lidarforfuel <- rast("D:/PostDocFire_RES/Largescale_Mapping/Aquitaine_Fuel_LiDAR_FL_corrected.tif")

# Charger carte OS (cosia ou OCSGE)  ici deux departement fourni par JL-Kicin

Kicin_OS_33 <- rast("D:/PostDocFire_RES/Carte_OCS/JL_Kicin/test_cosia_33/cosiad33grd10.tif")
Kicin_OS_40 <- rast("D:/PostDocFire_RES/Carte_OCS/JL_Kicin/codia_dep40_2021/cosiad40grd11.tif")
# Assemble les deux departement
Aquitaine_OS <- merge(Kicin_OS_33, Kicin_OS_40)

# REsampler OS basé sur carte fuel
Aquitaine_OS <- resample(Aquitaine_OS, Aquitaine_Lidarforfuel, method = "near")
# On garde que les OS de vegetation
Aquitaine_OS[Aquitaine_OS > 8] <- NA
Aquitaine_OS[Aquitaine_OS < 4] <- NA

# On renome la carte OS
rasterized <- Aquitaine_OS

# ON concate la carte de fuel metrics avec l'OS
Aquitaine_Lidarforfuel_mask_OCS <- mask(Aquitaine_Lidarforfuel, rasterized)
Aquitaine_Lidarforfuel_mask_OCS <- c(Aquitaine_Lidarforfuel_mask_OCS, rasterized)

# ON fait un tableau => Attention trés gros tableau surement difficile de traiter plus de deux departement
Aquitaine_Lidarforfuel_mask_OCS_df <- as.data.frame(Aquitaine_Lidarforfuel_mask_OCS, xy = T, cells = T, na.rm = F)
# QQe manip sur le tableau pour enlever les case vide (NA dans height), renommer les classes OS ...
Aquitaine_Lidarforfuel_mask_OCS_df <- subset(Aquitaine_Lidarforfuel_mask_OCS_df, !is.na(Aquitaine_Lidarforfuel_mask_OCS_df$Height))
Aquitaine_Lidarforfuel_mask_OCS_df <- data.table(Aquitaine_Lidarforfuel_mask_OCS_df)
Aquitaine_Lidarforfuel_mask_OCS_df[is.na(MFL)]$MFL <- 0
Aquitaine_Lidarforfuel_mask_OCS_df$FL_0_MFL <- Aquitaine_Lidarforfuel_mask_OCS_df$FL_0_1 + Aquitaine_Lidarforfuel_mask_OCS_df$MFL
Aquitaine_Lidarforfuel_mask_OCS_df$classe <- "no"

Aquitaine_Lidarforfuel_mask_OCS_df[cosiad33grd10 == 4]$classe <- "Low shrubland"
Aquitaine_Lidarforfuel_mask_OCS_df[cosiad33grd10 == 5]$classe <- "High shrubland"
Aquitaine_Lidarforfuel_mask_OCS_df[cosiad33grd10 == 6]$classe <- "Broadleaves"
Aquitaine_Lidarforfuel_mask_OCS_df[cosiad33grd10 == 7]$classe <- "Coniferous"
Aquitaine_Lidarforfuel_mask_OCS_df[cosiad33grd10 == 8]$classe <- "Wine"

# ON ne garde que les varaibles d'intérêt pour le cluster
Aquitaine_Lidarforfuel_mask_OCS_df_4clustering <- Aquitaine_Lidarforfuel_mask_OCS_df[, c("FL_0_1", "FL_0_MFL", "FSG", "CFL", "Height", "Profil_Type_L", "classe")]
Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$Profil_Type_L <- as.factor(Aquitaine_Lidarforfuel_mask_OCS_df$Profil_Type_L)

# Perform Factor Analysis for Mixed Data (FAMD)
famd_res <- FactoMineR::FAMD(Aquitaine_Lidarforfuel_mask_OCS_df_4clustering[, 1:7], graph = F)

# Get the factor scores
famd_scores <- famd_res$ind$coord

# Perform clustering on the factor scores (e.g., k-means clustering)
# set.seed(123)
kmeans_res <- kmeans(famd_scores, centers = 15)

# Add cluster assignments to the original data

# Function to obtain the
get_most_represented_factor <- function(factor_var) {
  # Check if the input is a factor
  if (!is.factor(factor_var)) {
    stop("Input variable is not a factor.")
  }

  # Calculate the frequency of each level using table
  freq_table <- table(factor_var)

  # Identify the level with the highest frequency
  most_represented_level <- names(freq_table)[which.max(freq_table)]

  return(most_represented_level)
}
# On ajoute les cluster au tableau
Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$cluster_15 <- as.factor(kmeans_res$cluster)
# ON ordonne les clusters
Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$classe <- as.factor(Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$classe)
clust_order <- Aquitaine_Lidarforfuel_mask_OCS_df_4clustering[, .(order_H_clus = mean(Height), Order_FL_0_MFL = mean(FL_0_MFL), get_most_represented_factor(classe)), by = cluster_15]

setorder(clust_order, V3, order_H_clus)

# CI dessous pour remanier l'ordre des cluster si necessaire
clust_order <- clust_order[c(15, 14, 13, 12, 6:11, 1:5), ]
clust_order <- clust_order[c(
  1, 2, 4, 3, 6,
  5, 7, 8, 9, 10,
  11, 14, 13, 12, 15
), ]
Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$True_Cluster_15 <- as.factor(match(Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$cluster_15, clust_order$cluster_15))

# On ajoute la variable cell (numero de la cellule du raster)
Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$cell <- Aquitaine_Lidarforfuel_mask_OCS_df$cell


kmeans_res <- kmeans(famd_scores, centers = 30)

# On backup la table
fwrite(Aquitaine_Lidarforfuel_mask_OCS_df_4clustering, "D:/LiDARandField_Data_Fuel/Data_LiDAR_IGN/Typo_LiDAR_OCS/Aquitaine_Lidarforfuel_mask_OCS_df_4clustering.txt")
# On reload la table
Aquitaine_Lidarforfuel_mask_OCS_df_4clustering <- fread("D:/LiDARandField_Data_Fuel/Data_LiDAR_IGN/Typo_LiDAR_OCS/Aquitaine_Lidarforfuel_mask_OCS_df_4clustering.txt")

Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$Profil_Type_L <- as.factor(Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$Profil_Type_L)
Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$True_Cluster_15 <- as.factor(Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$True_Cluster_15)

# A few plots
sample_4_plots <- sample(1:nrow(Aquitaine_Lidarforfuel_mask_OCS_df_4clustering), 3000000)
Height <- ggplot(data = Aquitaine_Lidarforfuel_mask_OCS_df_4clustering[sample_4_plots, ], aes(x = True_Cluster_15, y = Height)) +
  geom_boxplot() +
  xlab("Fuel Type") +
  ylab("Canopy Height (m)") +
  theme_few()
FL_0_1 <- ggplot(data = Aquitaine_Lidarforfuel_mask_OCS_df_4clustering[sample_4_plots, ], aes(x = True_Cluster_15, y = FL_0_1)) +
  geom_boxplot() +
  xlab("Fuel Type") +
  ylab("Fuel load 0-1m (Kg.m²)") +
  theme_few()

FL_0_MFL <- ggplot(data = Aquitaine_Lidarforfuel_mask_OCS_df_4clustering[sample_4_plots, ], aes(x = True_Cluster_15, y = FL_0_MFL)) +
  geom_boxplot() +
  xlab("Fuel Type") +
  ylab("Understory Fuel load (Kg.m²)") +
  theme_few()

FSG <- ggplot(data = Aquitaine_Lidarforfuel_mask_OCS_df_4clustering[sample_4_plots, ], aes(x = True_Cluster_15, y = FSG)) +
  geom_boxplot() +
  xlab("Fuel Type") +
  ylab("Fuel Strata Gap (m)") +
  theme_few()

CFL <- ggplot(data = Aquitaine_Lidarforfuel_mask_OCS_df_4clustering[sample_4_plots, ], aes(x = True_Cluster_15, y = CFL)) +
  geom_boxplot() +
  xlab("Fuel Type") +
  ylab("Canopy Fuel load (Kg.m²)") +
  theme_few()

Profil_Type_L <- ggplot(Aquitaine_Lidarforfuel_mask_OCS_df_4clustering[sample_4_plots, ]) +
  aes(x = True_Cluster_15, fill = Profil_Type_L) +
  geom_bar(position = "fill") +
  labs(fill = "FPT") +
  xlab("Fuel Type") +
  scale_fill_viridis_d(labels = c("1" = "A: Mono", "2" = "B: Bi", "3" = "C: Multi", "4" = "D: Continu")) + # Set custom labels)+
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "white", color = NA), # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA),
  )

classe <- ggplot(Aquitaine_Lidarforfuel_mask_OCS_df_4clustering[sample_4_plots, ]) +
  aes(x = True_Cluster_15, fill = classe) +
  geom_bar(position = "fill") +
  xlab("Fuel Type") +
  labs(fill = "Land use") +
  scale_fill_manual(na.value = "white", values = c("Low shrubland" = colors[2], "High shrubland" = colors[3], "Coniferous" = colors[8], "Broadleaves" = colors[13], "Vineyard" = colors[1])) +
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "white", color = NA), # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA),
  )


tab_freq <- data.table(table(Aquitaine_Lidarforfuel_mask_OCS_df_4clustering[sample_4_plots, ]$True_Cluster_15) / nrow(Aquitaine_Lidarforfuel_mask_OCS_df_4clustering[sample_4_plots, ]))
names(tab_freq) <- c("True_Cluster_15", "Proportion")
tab_freq$True_Cluster_15 <- factor(tab_freq$True_Cluster_15, levels = as.factor(1:15))


Frequency <- ggplot(data = tab_freq, aes(x = True_Cluster_15, y = Proportion)) +
  geom_bar(stat = "identity") +
  ylab("Frequency on Aquitaine territory") +
  xlab("Fuel Type") +
  # scale_fill_viridis_d()+
  theme_few()

my_plot <- ggarrange(Height, FL_0_1, FL_0_MFL, FSG, CFL, Profil_Type_L, classe, Frequency, ncol = 2, nrow = 4)

gridExtra::grid.arrange(Height, FSG, FL_0_MFL, CFL, # bar plot spaning two columns
  classe, Frequency, # box plot and scatter plot
  ncol = 2, nrow = 4,
  layout_matrix = rbind(c(1, NA), c(2, 5), c(3, 6), c(4, NA))
)

# Attribute cluster to the map ---

Aquitaine_Lidarforfuel_mask_OCS_df_4clustering <- fread("D:/LiDARandField_Data_Fuel/Data_LiDAR_IGN/Typo_LiDAR_OCS/Aquitaine_Lidarforfuel_mask_OCS_df_4clustering.txt")

Aquitaine_Lidarforfuel_mask_OCS_df$cell

cluster <- rep(NA, ncell(Aquitaine_Lidarforfuel_mask_OCS))

cluster[Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$cell] <- Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$True_Cluster_15
Aquitaine_Lidarforfuel_mask_OCS$cluster_15 <- as.factor(cluster)

cluster <- rep(NA, ncell(Aquitaine_Lidarforfuel_mask_OCS))
cluster[Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$cell] <- as.character(Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$classe)
Aquitaine_Lidarforfuel_mask_OCS$classe <- as.factor(cluster)

cluster <- rep(NA, ncell(Aquitaine_Lidarforfuel_mask_OCS))
cluster[Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$cell] <- Aquitaine_Lidarforfuel_mask_OCS_df_4clustering$True_Cluster_30
Aquitaine_Lidarforfuel_mask_OCS$cluster_30 <- as.factor(cluster)

writeRaster(Aquitaine_Lidarforfuel_mask_OCS, "D:/PostDocFire_RES/Largescale_Mapping/Aquitaine_Typo_LiDAR_OCS.tif", overwrite = T)

cult_agg <- aggregate(Aquitaine_Lidarforfuel_mask_OCS$cluster, fact = 5, fun = modal)

myrast <- Aquitaine_Lidarforfuel_mask_OCS$cluster == 1

plot(Aquitaine_Lidarforfuel_mask_OCS$cluster)


sf_Aquitaine_cluster <- st_as_sf(sf_Aquitaine_cluster)


# Définition des couleurs après permutation de 11 et 14
colors <- c(
  "#9B59B6", # Vineyard (Violet)
  "#ADFF2F", "#C4D631", "#D6B932", "#F0A500", # Shrubland (Jaune-vert à rouge)
  "#228B22", "#3B7223", "#545B24", "#6D4425", "#862D26", # Coniferous (Vert foncé à rouge)
  # "#A0AD6D", "#3A89A0", "#50BDA5", "#00CED1",  # Broadleaves (Bleu-vert)
  "#3A89A0", "#00CED1", "#50BDA5", "#A0AD6D", # Broadleaves (Bleu-vert)
  "#A9A9A9" # Multistrate (Gris, temporary)
)

# Labels des catégories
labels <- c(
  "Vineyard",
  "Low shrubland", "Low-load-high shrubland", "Med-load-high shrubland or immat. coniferous", "High-load shrubland or immat. trees",
  "Low-load immat. coniferous", "Med-load immat. coniferous", "Transitional coniferous", "Mature coniferous with low underst.", "Mature coniferous with high underst.",
  "Low-load dominated mixed broadleaves", "Med-load dominated mixed broadleaves", "Mature broadleaves with low underst.", "Mature broadleaves with high underst.",
  "Multistrata mature stand (to be refined)"
)






library(tidyterra)
ggplot() +
  geom_spatraster(data = Aquitaine_Lidarforfuel_mask_OCS, aes(fill = cluster_15)) +
  coord_fixed() +
  coord_sf(crs = 2154) +
  geom_sf(data = dep_Aquitaine, fill = NA, size = 2, color = "black") +
  scale_fill_manual(values = colors, labels = labels, na.value = "white") + # Custom colors
  labs(fill = "Fuel type") +
  theme_void() +
  theme(legend.position = "none")
# scale_fill_hypso_d(palette = "etopo1")+


colorRampPalette(c("#FFFFCC", "#FFD700", "#FFCC00"))

Aquitaine_Lidarforfuel_mask_OCS$classe <- NA
Aquitaine_Lidarforfuel_mask_OCS
ggplot() +
  geom_spatraster(data = Aquitaine_Lidarforfuel_mask_OCS, aes(fill = classe)) +
  coord_fixed() +
  geom_sf(data = dep_Aquitaine, fill = NA, size = 2, color = "black") +
  coord_sf(crs = 2154) +
  labs(fill = "Land use") +
  scale_fill_manual(na.value = "white", values = c("Low shrubland" = colors[2], "High shrubland" = colors[3], "Coniferous" = colors[8], "Broadleaves" = colors[13], "Vineyard" = colors[1])) +
  # labels = c("Broussaille" = "Shrubland", "Feuillu" = "Broadleaves", "Conifère" = "Needleleaves","Coupe"="Cut","Vigne"="Wine"))+
  theme_few()


# GRaphics of variable 4 plot
dep <- st_read("P:/Olivier/Work/Post_doc_INRAe_Fire-res/Other_Shp/France/departements-20180101-shp/departements-20180101.shp")
dep <- st_transform(dep, "epsg:2154")
dep_Aquitaine <- dep[dep$code_insee %in% c(33, 40), ]
library(tidyterra)

library(tidyterra)
ggplot() +
  geom_spatraster(data = Aquitaine_Lidarforfuel_mask_OCS, aes(fill = cluster_15)) +
  coord_fixed() +
  coord_sf(crs = 2154) +
  geom_sf(data = dep_Aquitaine, fill = NA, size = 2, color = "black") +
  scale_fill_manual(values = colors, labels = labels, na.value = "white") + # Custom colors
  labs(fill = "Fuel type") +
  theme_void() +
  theme(legend.position = "right")
# scale_fill_hypso_d(palette = "etopo1")+

OSO_map <- ggplot() +
  geom_spatraster(data = Aquitaine_Lidarforfuel_mask_OCS, aes(fill = classe)) +
  coord_fixed() +
  geom_sf(data = dep_Aquitaine, fill = NA, size = 2, color = "black") +
  coord_sf(crs = 2154) +
  # ggtitle("Land use 20m")+
  labs(fill = "Land use") +
  scale_fill_manual(na.value = "white", values = c("Low shrubland" = colors[2], "High shrubland" = colors[3], "Coniferous" = colors[8], "Broadleaves" = colors[13], "Vineyard" = colors[1])) +
  # labels = c("Broussaille" = "Shrubland", "Feuillu" = "Broadleaves", "Conifère" = "Needleleaves","Coupe"="Cut","Vigne"="Wine"))+
  theme_void() +
  theme(legend.position = "bottom")
#         legend.title = element_text(size = 16),  # Increase legend title size
#         legend.text = element_text(size = 13))


FL_0_1_map <- ggplot() +
  geom_spatraster(data = Aquitaine_Lidarforfuel_mask_OCS, aes(fill = FL_0_1)) +
  coord_fixed() +
  coord_sf(crs = 2154) +
  geom_sf(data = dep_Aquitaine, fill = NA, size = 2, color = "black") +
  # ggtitle("Fuel load 0-1m (Kg.m²). 20m")+
  labs(fill = "FL0-1m (Kg.m²)") +
  scale_fill_hypso_c(palette = "wiki-schwarzwald-cont") +
  theme_void() +
  theme(legend.position = "bottom")
#         legend.title = element_text(size = 16),  # Increase legend title size
#         legend.text = element_text(size = 13))

FL_0_1_MFL_map <- ggplot() +
  geom_spatraster(data = Aquitaine_Lidarforfuel_mask_OCS, aes(fill = FL_0_MFL)) +
  coord_fixed() +
  coord_sf(crs = 2154) +
  # ggtitle("Understorey fuel load (Kg.m²). 20m")+
  geom_sf(data = dep_Aquitaine, fill = NA, size = 2, color = "black") +
  labs(fill = "UFL (Kg.m²)") +
  scale_fill_hypso_c(palette = "wiki-schwarzwald-cont") +
  theme_void() +
  theme(legend.position = "bottom")
# legend.title = element_text(size = 16),  # Increase legend title size
# legend.text = element_text(size = 13))
CFL_map <- ggplot() +
  geom_spatraster(data = Aquitaine_Lidarforfuel_mask_OCS, aes(fill = CFL)) +
  coord_fixed() +
  coord_sf(crs = 2154) +
  # ggtitle("Canopy Fuel load (Kg.m²). 20m")+
  geom_sf(data = dep_Aquitaine, fill = NA, size = 2, color = "black") +
  labs(fill = "CFL (Kg.m²)") +
  scale_fill_hypso_c(palette = "wiki-schwarzwald-cont") +
  theme_void() +
  theme(legend.position = "bottom")
# legend.title = element_text(size = 16),  # Increase legend title size
# legend.text = element_text(size = 13))

FSG_map <- ggplot() +
  geom_spatraster(data = Aquitaine_Lidarforfuel_mask_OCS, aes(fill = FSG)) +
  coord_fixed() +
  coord_sf(crs = 2154) +
  # ggtitle("Fuel strata gap (m). 20m")+
  geom_sf(data = dep_Aquitaine, fill = NA, size = 2, color = "black") +
  labs(fill = "FSG (m)") +
  scale_fill_hypso_c(palette = "wiki-schwarzwald-cont") +
  theme_void() +
  theme(legend.position = "bottom")
# legend.title = element_text(size = 16),  # Increase legend title size
# legend.text = element_text(size = 13))

Height_map <- ggplot() +
  geom_spatraster(data = Aquitaine_Lidarforfuel_mask_OCS, aes(fill = Height)) +
  coord_fixed() +
  coord_sf(crs = 2154) +
  # ggtitle("Height (m). 20m")+
  geom_sf(data = dep_Aquitaine, fill = NA, size = 2, color = "black") +
  labs(fill = "Height (m)") +
  scale_fill_hypso_c(palette = "wiki-schwarzwald-cont") +
  theme_void() +
  theme(legend.position = "bottom")
# legend.title = element_text(size = 16),  # Increase legend title size
# legend.text = element_text(size = 13))

FPT_map <- ggplot() +
  geom_spatraster(data = Aquitaine_Lidarforfuel_mask_OCS, aes(fill = Profil_Type_L)) +
  coord_fixed() +
  coord_sf(crs = 2154) +
  # ggtitle("Fuel profile type. 20m")+
  geom_sf(data = dep_Aquitaine, fill = NA, size = 2, color = "black") +
  labs(fill = "FPT") +
  scale_fill_viridis_d(labels = c("1" = "A: Mono", "2" = "B: Bi", "3" = "C: Multi", "4" = "D: Continu")) +
  theme_void() +
  theme(legend.position = "bottom")
# legend.title = element_text(size = 16),  # Increase legend title size
# legend.text = element_text(size = 13))

my_plot <- ggarrange(Height_map, FSG_map, FL_0_1_MFL_map, CFL_map, FPT_map, OSO_map, nrow = 1)

my_plot <- ggarrange(Height, FSG, FL_0_1_MFL, CFL, ncol = 2, nrow = 2)
