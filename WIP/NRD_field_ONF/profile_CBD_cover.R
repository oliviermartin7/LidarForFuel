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
library(ggthemes)

rm(list = ls())

data_placettes = read.csv("C:/Users/ateurnier/OneDrive - INRAE SharePoint SE/CDD/data_placettes_04_05_8413_42_48.csv", sep = ";")

pdf("D:/test_profile.pdf")
for (j in unique(data_placettes$Num_Plac)){
  
  plot_i=ggplot(data_placettes[ID_plot==j&H>1],aes(y=CBD_rollM,x=H))+
    geom_line(lwd=2)+
    theme_few()+
    geom_vline(aes(xintercept = All_plot_portugal_metrics_sp[ID_plot==j][1,]$CBH),col="gray66",show.legend = T,lwd=1.2)+
    geom_vline(aes(xintercept = All_plot_portugal_metrics_sp[ID_plot==j][1,]$`CBH (m)`),col="red",show.legend = T,lwd=1.2)+
    # geom_vline(aes(xintercept = CBH_BP),col=4)+
    # geom_vline(aes(xintercept = CBH_minus),col=5)+
    coord_flip()+
    xlim(c(0,NA))+
    # facet_wrap(~Threshold_value)+
    theme(axis.text.x=element_text(size=8))+
    ggtitle(j)
  print(plot_i)
}
dev.off()
dev.off()
dev.off()
