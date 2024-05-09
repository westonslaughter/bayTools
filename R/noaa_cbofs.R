# NOAA CBOFS
# load the `ncdf4` and the `CFtime` packages
library(ncdf4)
library(CFtime)
library(lattice)
library(RColorBrewer)

# set path and filename
ncpath <- "./data/nc_files/"
ncname <- "nos.cbofs.vibrioprob.20200818.t00z"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "temp1mday1"  # note: tmp means temperature (not temporary)

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

lat_lon = 

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

# get a single slice or layer (January)
m <- 1
tmp_slice <- tmp_array[,,m]
dim(tmp_slice)

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
var = tmp_slice
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


# quick map
image(lon_vec_clean, lat_vec_clean, var_vec) #, col=rev(brewer.pal(10,"RdBu")))


# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- cbind(lon_vec, lat_vec, var_vec)
dim(lonlat)

# lonlat <- as.data.frame(lonlat)
colnames(lonlat) <- c("lon", "lat", "var")

library(dplyr)
lonlat_order <- lonlat %>%
  arrange(lon, lat, var) %>%
  filter(!is.na(lon))

# quick map
image(lonlat_order$lon, lonlat_order$lat, lonlat_order$var) #, col=rev(brewer.pal(10,"RdBu")))

tmp_cheat = tmp_array[,,1]

rownames(tmp_cheat) <- lon_vec
colnames(tmp_cheat) <- lat_vec

levelplot(tmp_array[,,1], ~ lon_vec * lat_vec)

colnames(lonlat_order) <- c("x", "y", "z")

library(raster)

etd_sf <- sf::st_as_sf(x = as.data.frame(lonlat_order), 
                   coords = c("x", "y"),
                   crs = 4326)

plot(etd_sf, main = NULL)
title("\n\nChesapeake Bay Salinity, August 18 2020\nNOAA CBOFS from THREDDS Archive netCDF\n\n\n\n\n\n\n\n")

ext <- floor(extent(etd_sf))
rr <- raster(ext, res=0.021)
rr <- rasterize(etd_sf, rr, field="z")

plot(rr)

dfr <- rasterFromXYZ(etd_sf, res=c(NA, 1))

levelplot(z~x*y, data = lonlat_order, region = TRUE)



          