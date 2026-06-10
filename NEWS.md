# lidarforfuel (development version)

## Add

* function `get_traj` to compute trajectory from point cloud
* function `add_traj_to_las` to add sensor coordinates
  to each point of LAS object.
* new arg `traj` for `fPCpretreatment`: trajectory should be computed
  separately with buffer.
* add functions `lasrmdup` and `lasrenumber` associated to `get_traj` compuation.
* add tests
* add seaprate functions for PAD compuation:
  * `pad_metrics` (PR #17) : changes default from `limit_N_points=400` to `limit_N_points=0`
  * `parse_pad_heights` to get the strata heights from the PAD names.
* add new metrics with function `ffuelmetrics2`

## Change

* reformat NAMESPACE (issue #13)
* reformat package R files for readability
* change default reference datetime for gpstime from
  "2011-09-14 00:00:00" to "2011-09-14 01:46:40"
* `fPCpretreatment`
  * modularized internal functions
  * removed args `LMA`, `WD`, `LMA_bush`, `WD_bush`, `H_strata_bush`.
    Layer `FMA` is directly expected as input arg of `ffuelmetrics2`.
  * removed `Height_filter`, the maximum height can be defined in `pad_metrics` through `nlayers`
  * change `start_date` for `gpstime_ref` to be more explicit
  * add args
    * `exclude_classes` to have some classes excluded from the point cloud, typically class 66 for french LIDAR-HD
    * `dtm` to use raster DTM for height normalization.
* args in `fCBDprofile_fuelmetrics`:
  * `Height_Cover` --> `height_cover`
  * `limit_flightheight` --> `limit_flight_height`
  * `limit_vegetationheight` --> `limit_vegetation_height`

* remove alternative trajectory when not enough multi-returns to compute: 
  * before: the sensor location was set to the average of ground point plus 1400m
  * now: an error is returned

## Fix

* fix `fPCpretreatment` (issue #11)
* fix missing dependencies (issue #12)
