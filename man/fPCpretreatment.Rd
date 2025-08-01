% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fPCpretreatment.R
\name{fPCpretreatment}
\alias{fPCpretreatment}
\title{Point cloud pre-treatment for using fCBDprofile_fuelmetrics in pixels}
\usage{
fPCpretreatment(
  chunk,
  classify = F,
  LMA = 140,
  WD = 591,
  WD_bush = 591,
  LMA_bush = 140,
  H_strata_bush = 2,
  Height_filter = 60,
  start_date = "2011-09-14 00:00:00",
  season_filter = FALSE,
  deviation_days = "Infinity",
  plot_hist_days = FALSE
)
}
\arguments{
\item{chunk}{character. path to a las (laz) file. Can be apply to a catalog see lidr catalog apply)}

\item{classify}{logical (default is FALSE). Make a ground classification. Only if the original point cloud is not classified}

\item{LMA}{character or numeric. Default = 140. If available path to a LMA map (.tif) of the area if available or a single LMA value in g.m² (e.g 140. cf: Martin-Ducup et al. 2024)}

\item{WD}{character or numeric. Default = 591. If available, path to a WD map (.tif) of the area if available or a single WD value in kg.m3 (e.g 591 cf: Martin-Ducup et al. 2024).}

\item{WD_bush}{character or numeric. Default = 591. similar to WD but for the understorey strata 0 to 2m}

\item{LMA_bush}{character or numeric.Default = 140. similar to LMA but for the understorey strata 0 to 2m}

\item{H_strata_bush}{numeric. Default = 2. Height of the strata to consider for separating LMA and WD between canopy and bush.}

\item{Height_filter}{numeric. Default = 80. Height limit to remove noise point}

\item{start_date}{date. The absolute starting date to retrieve date from relative gpstime of the laz. Default is "2011-09-14 00:00:00"}

\item{season_filter}{logical. Should the point cloud be filtered by season. Default is FALSE, if TRUE, only may to October (theoretically leaf-on..)  returns are kept}

\item{deviation_days}{numeric. Maximum number of days tolerated between the acquisition in a given point cloud (a tile or plot). Deactivated by default}

\item{plot_hist_days}{logical. Should the histogram of dates of acquisition be displayed. Default =FALSE}
}
\value{
a Normalized point cloud (.laz) with several new attributes need to run fCBDprofile_fuelmetrics
}
\description{
Function for preprocessing las (laz) files for use in fCBDprofile_fuelmetrics. This can be used in the catalog_apply lidR function. The pretreatment consists of normalizing the point cloud and adding various attributes: Plane position for each point (easting, northing, elevation), LMA (leaf mass area) and wood density (WD) by intersecting the point cloud with an LMA and WD map or by providing LMA and WD values.
}
\details{
The attributes added to the laz are LMA : LMA value of each point. Zref :original Z; Easting, Northing, Elevation, Time that are the X,Y,Z position of the plane and the its GPStime for each point (obtained from lidR::track_sensor()). In a following version it will be possible to directly load a trajectory file if available.
}
\examples{
\donttest{
path2laz=system.file("extdata","M30_FontBlanche.laz", package="lidarforfuel")
 #LMA value selected = 120.6 that is the LMA for Pinus halepensis, the dominant species of the plot
M30_FontBlanche_pretreated<-fPCpretreatment(path2laz,LMA=120.6)
# displaying the new attributes in the las
names(M30_FontBlanche_pretreated)
}
}
