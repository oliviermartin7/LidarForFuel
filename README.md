# lidarForFuel

This readme aim at describing how to use a set of tool (R functions and soon a package) to run the method described in the paper [Unlocking the Potential of Als Data for Direct Assessment of Fuel Load and Vertical Structure](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4779351)

## Global description of the approach

lidarforfuel aims to compute fuel metrics from airborne LiDAR data and map them at large scale. In the present  form two R functions have been developed 1) pretreatment of a point cloud and 2) computing fuel metrics. The function can be used either at the plot scale for specific analysis on small areas or at large scale using catalog of LiDAR tile of the lidR package.

![Illustration summarising the global approach!](img/readme_1_general.png)

## fPCpretreatment
to do


## fCBDprofile_fuelmetrics

to do


## Install LidarForFuel


The latest version from Github (in development):
```{r}
install.packages("remotes")
remotes::install_github('oliviermartin7/LidarForFuel')
```

To use it :
```{r}
library("LidarForFuel")
```
