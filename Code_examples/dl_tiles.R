library(stringr)
library(lidR)
library(future)

rm(list=ls())

all_url = read.delim("~/path_to_a_list_of_url_of_ALS_data")

options(timeout = 9999999999)
for (i in 1:length(all_url[,1])){
  
  tile_ID=str_split(all_url[i,1],"/")[[1]][8]
  download.file(url = all_url[i,1],destfile = paste0("~pathtodestination",tile_ID),quiet=F,mode="wb")
}

