# lidarforfuel (development version)

## Add

* function `get_traj` to compute trajectory from point cloud
* function `add_traj_to_las` to add sensor coordinates
  to each point of LAS object.
* new arg `traj` for `fPCpretreatment`: trajectory should be computed
  separately with buffer.
* add tests

## Change

* reformat NAMESPACE (issue #13)
* reformat package R files for readability
* change default reference datetime for gpstime from
  "2011-09-14 00:00:00" to "2011-09-14 01:46:40"
* modularized internal functions of `fPCpretreatment`
* args in `fCBDprofile_fuelmetrics`:
  * `Height_Cover` --> `height_cover`
  * `limit_flightheight` --> `limit_flight_height`
  * `limit_vegetationheight` --> `limit_vegetation_height`

## Fix

* fix `fPCpretreatment` (issue #11)
* fix missing dependencies (issue #12)
