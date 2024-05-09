# NOAA CBOFS
# load the `ncdf4` and the `CFtime` packages
library(ncdf4)
library(CFtime)
library(lattice)
library(RColorBrewer)
library(dplyr)
library(raster)

# set path and filename
ncpath <- "./data/nc_files/"
ncname <- "nos.cbofs.vibrioprob.20200818.t00z"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "sal1mday1"  # note: tmp means temperature (not temporary)

# open a netCDF file
ncin <- nc_open(ncfname)
print(ncin)

# get longitude and latitude
lon <- ncvar_get(ncin,"lon_rho")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat_rho")
nlat <- dim(lat)
head(lat)

# lat_lon = 

# convert ROMS XI, ETA curvilinear coords into lat, long 
# following: https://www.myroms.org/forum/viewtopic.php?t=295
# c(lat, long) = c(xi, eta)*cos(angle(i,j)) -c(xi, eta) *sin(angle(i,j))

# troms = angstroms::romscoords(ncfname, spatial=c("lat_rho", "lon_rho"), ncdf = TRUE)


print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin, "ocean_timeday1")
time

tunits <- ncatt_get(ncin,"ocean_timeday1","units")
tunits

nt <- dim(time)
nt

# get temperature
tmp_array <- ncvar_get(ncin, dname)

dlname <- ncatt_get(ncin, dname, "long_name")
dunits <- ncatt_get(ncin, dname, "units")

dllat <- ncatt_get(ncin, dname, "coordinates")

fillvalue <- ncatt_get(ncin, dname, "_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

ls()

# decode time
cf <- CFtime(tunits$value, calendar = "proleptic_gregorian", time) # convert time to CFtime class
cf

timestamps <- CFtimestamp(cf) # get character-string times
timestamps

class(timestamps)
time_cf <- CFparse(cf, timestamps) # parse the string into date components
time_cf
class(time_cf)

# replace netCDF fill values with NA's
tmp_array[tmp_array==fillvalue$value] <- NA

length(na.omit(as.vector(tmp_array[,,1])))


times_vec = list(rep(c(""), 24))

# loop through time
for(t in 1:2) {
  
  # get a single slice or layer (January)
  # m <- 1
  var <- tmp_array[,,t]
  dim(var)
  
  # vectorize lat, long?
  lat_vec = c()
  for(r in 1:nrow(lat)) {
    for(c in 1:ncol(lat)) {
      lat_vec = c(lat_vec, lat[r,c])
    }
  }
  
  # vectorize lon, long?
  lon_vec = c()
  for(r in 1:nrow(lon)) {
    for(c in 1:ncol(lon)) {
      lon_vec = c(lon_vec, lon[r,c])
    }
  }
  
  # vectorize var, varg?
  var_vec = c()
  for(r in 1:nrow(var)) {
    for(c in 1:ncol(var)) {
      var_vec = c(var_vec, var[r,c])
    }
  }
  
  # replace netCDF fill values with NA's
  lon_vec[lon_vec==fillvalue$value] <- NA
  length(na.omit(as.vector(lon_vec)))
  lon_vec_clean = na.omit(as.vector(lon_vec))
  
  # replace netCDF fill values with NA's
  lat_vec[lat_vec==fillvalue$value] <- NA
  length(na.omit(as.vector(lat_vec)))
  lat_vec_clean = na.omit(as.vector(lat_vec))
  
  # replace netCDF fill values with NA's
  var_vec[var_vec==fillvalue$value] <- NA
  length(na.omit(as.vector(var_vec)))
  var_vec_clean = na.omit(as.vector(var_vec))
  
  # create dataframe -- reshape data
  # matrix (nlon*nlat rows by 2 cols) of lons and lats
  lonlat <- cbind(lon_vec, lat_vec, var_vec)
  dim(lonlat)
  
  lonlat <- as.data.frame(lonlat)
  colnames(lonlat) <- c("lon", "lat", "var")
  
  lonlat_order <- lonlat %>%
    arrange(lon, lat, var) %>%
    filter(!is.na(lon))
  
  print(paste("saving into time vec for time:", t))
  times_vec[[t]] = lonlat_order
  
  colnames(lonlat_order) <- c("x", "y", "z")
  var_sf <- sf::st_as_sf(x = as.data.frame(lonlat_order), 
                     coords = c("x", "y"),
                     crs = 4326)
  
  # plot(etd_sf, main = NULL)
  # title("\n\nChesapeake Bay Salinity, August 18 2020\nNOAA CBOFS from THREDDS Archive netCDF\n\n\n\n\n\n\n\n")
  
  ext <- floor(extent(var_sf))
  rr <- raster(ext, res=0.019)
  rr <- rasterize(var_sf, rr, field="z")
  
  plot(rr)
}
