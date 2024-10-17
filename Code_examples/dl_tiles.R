library(stringr)
library(lidR)
library(future)

rm(list=ls())

all_url = read.delim("~/Downloads/Aude_url_2_dl.txt")
indices_erreurs <- c()  

options(timeout = 9999999999)
for (i in 1:length(all_url[,1])){
  
  tile_ID=str_split(all_url[i,1],"/")[[1]][8]
  download.file(url = all_url[i,1],destfile = paste0("/Volumes/LaCie/Aude/",tile_ID),quiet=F,mode="wb")
}

# for (i in 1:length(all_url[,1])) {
#   tryCatch({ 
#     
#     tile_ID=str_split(all_url[i,],"/")[[1]][8]
#     download.file(url = all_url[i,],destfile = paste0("~/Documents/subsample/",tile_ID),quiet=F,mode="wb")
#   },error = function(err) {
#     # Code exécuté en cas d'erreur
#     cat("Erreur détectée à l'itération", i, ":", conditionMessage(err), "\n")
#     
#     # Stocke l'indice de l'itération où l'erreur s'est produite
#     indices_erreurs <- c(indices_erreurs, i)
#     
#     # Passe à l'itération suivante
#     next
#   })
#   
# }
