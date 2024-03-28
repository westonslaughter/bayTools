#' Script for "internal" functions which are from other sources,
#' but are copied, and in some cases modified, to this script.
#'
#'
#'
#' @details
#'
#'
#'
#'
#'
#' @keywords internal
#'


# NOTE: air pressure from nearest NOAA gauge
# source: https://github.com/streampulse/StreamPULSE/blob/master/R/sp_internals.R
# streamPULSE R package
FindandCollect_airpres = function(lat, long, start_datetime, end_datetime) {

    #get df of all available air pressure stations
    tf = tempfile()
    download.file("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt",tf,mode="wb")
    noaa.sites <- read.fwf(tf, skip = 22, header = F,
        # widths = c(6,-1,5,-1,30, 5, 3, 6, 8, 9, 8, 9, 8), comment.char = "",
        widths = c(6,-1,5,-45, 8, 9,-8, 9, 8), comment.char = "",
        col.names = c("USAF", "WBAN", "LAT", "LON", "BEGIN", "END"),
        # col.names = c("USAF", "WBAN", "STATION NAME", "CTRY", "ST", "CALL", "LAT", "LON", "ELEV(M)", "BEGIN", "END"),
        flush = TRUE, colClasses=c('USAF'='character', 'WBAN'='character'))
    noaa.sites <- na.omit(noaa.sites)

    #narrow them down to those within 5 lats/longs
    noaa.sites <- noaa.sites %>%
        mutate(LAT = as.numeric(as.character(LAT))) %>%
        mutate(LON = as.numeric(as.character(LON))) %>%
        filter(LAT < (lat + 5) & LAT > (lat - 5) & LON < (long + 5) & LON > (long - 5))

    #filter by coverage, order by distance
    pt1 <- cbind(rep(long, length.out = length(noaa.sites$LAT)),
        rep(lat, length.out = length(noaa.sites$LAT)))
    pt2 <- cbind(noaa.sites$LON, noaa.sites$LAT)
    dist <- diag(geosphere::distm(pt1, pt2, fun=geosphere::distHaversine))/1000
    noaa.sites$dist <- dist
    tmp <- which((as.numeric(substr(noaa.sites$END,1,4)) >=
        as.numeric(substr(end_datetime, 1, 4))) &
        as.numeric(substr(noaa.sites$BEGIN,1,4)) <=
        as.numeric(substr(start_datetime, 1, 4)))
    noaa.sites <- noaa.sites[tmp,]
    noaa.sites <- noaa.sites[with(noaa.sites, order(dist)),]

    yrs <- seq(as.numeric(substr(start_datetime, 1, 4)),as.numeric(substr(end_datetime, 1, 4)), by = 1)
    for (i in 1:length(noaa.sites$dist)) {
        k <- i
        available <- vector(mode = 'logical', length = length(yrs))
        USAF <- as.character(noaa.sites$USAF[i])
        if(nchar(as.character(noaa.sites$WBAN[i])) == 5){
            WBAN <- as.character(noaa.sites$WBAN[i])
        } else {
            WBAN <- paste0(0,as.character(noaa.sites$WBAN[i]))
        }
        y <- as.data.frame(matrix(NA, nrow = 1, ncol = 12))
        for(j in 1:length(yrs)){
            tf = tempfile()
            res = tryCatch(suppressWarnings(download.file(paste0("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/",
                yrs[j], "/", USAF, "-", WBAN, "-", yrs[j], ".gz"), tf, mode="wb")),
                error=function(e){
                    # message('NCDC download failed; trying next closest station')
                    return('download failed')
                })
            if(exists('res') && res == 'download failed'){
                break #try next station
            }

            x = read.table(tf)
            x[x==-9999] = NA
            if(length(which(!is.na(x$V7))) >= 0.9 * length(x$V7)) {
                available[j] <- TRUE
                y <- rbind(x,y)
            }else {
                break #too many NAs, move to next station
            }
        }
        if(length(yrs) == length(which(available))){
            break #got one
        }
    }
    y <- y[!is.na(y$V1),]
    colnames(y) = c("y","m","d","h","air_temp","dewtemp","air_kPa","winddir","sindspeed","skycover","precip1h","precip6h")
    y$air_kPa = y$air_kPa/100
    y$air_temp = y$air_temp/10
    y$DateTime_UTC = readr::parse_datetime(paste0(y$y,"-",
        sprintf("%02d",y$m),"-",sprintf("%02d",y$d)," ",sprintf("%02d",y$h),
        ":00:00"), "%F %T")
    y <- y[with(y, order(DateTime_UTC)),]
    y = tibble::as_tibble(y) %>% select(DateTime_UTC,air_temp,air_kPa)
    ss = tibble::tibble(DateTime_UTC=seq(y$DateTime_UTC[1],
        y$DateTime_UTC[nrow(y)], by=900))
    xx = left_join(ss, y, by = "DateTime_UTC")
    xx = mutate(xx, air_temp=zoo::na.approx(air_temp),
        air_kPa=zoo::na.approx(air_kPa))
    daterng = c(start_datetime, end_datetime)
    xtmp = xx %>% filter(DateTime_UTC>=daterng[1] & DateTime_UTC<=daterng[2])
    # select(xtmp, DateTime_UTC, air_kPa, air_temp)
    # print(noaa.sites[k,])
    return(select(xtmp, DateTime_UTC, air_kPa, air_temp))
}

# NOTE: site AVERAGE air pressure from elevation
# source: https://github.com/DOI-USGS/streamMetabolizer/blob/main/R/calc_air_pressure.R
# streamMetabolizer R package
# Calculates the average air pressure for a site
calc_air_pressure <- function(temp.air=u(15, "degC"), elevation=u(762, "m"), attach.units=deprecated()) {

  # check units-related arguments
  if (lifecycle::is_present(attach.units)) {
    unitted_deprecate_warn("calc_air_pressure(attach.units)")
  } else {
    attach.units <- FALSE
  }

  # assume units if not provided
  if(!is.unitted(temp.air)) temp.air <- u(temp.air, "degC")
  if(!is.unitted(elevation)) elevation <- u(elevation, "m")

  # check units
  verify_units(temp.air, "degC")
  verify_units(elevation, "m")

  # compute pressure. eqn also at https://en.wikipedia.org/wiki/Barometric_formula
  Pb <- u(760, "mmHg") # standard pressure
  g0 <- u(9.80665, "m s^-2") # gravitational acceleration
  M <- u(0.0289644, "kg mol^-1") # molar mass of Earth's air
  Rst <- u(8.31447, "N m mol^-1 K^-1") * u(1, "kg m s^-2 N^-1") # universal gas constant for air: 8.31432 N*m /(mol*K)
  Ta <- u(273.15, "K") + temp.air*u(1, "K degC^-1") # actual temperature in Kelvins
  baro <- Pb * exp((-1 * g0 * M * elevation)/(Rst * Ta)) * u(1.33322368, "mb mmHg^-1")

  # return
  if(attach.units) baro else v(baro)
}

# NOTE: DO saturation based off equilibria
# source: https://github.com/DOI-USGS/streamMetabolizer/tree/main/R
# streamMetabolizer R package
# Calculates the equilibrium saturation concentration of oxygen in water at the
# supplied conditions
calc_DO_sat <- calc_DO_at_sat <- function(temp.water, pressure.air, salinity.water = u(0,'PSU'), model='garcia-benson', ...){

  if(as.character(sys.call()[[1]]) == 'calc_DO_at_sat') {
    .Deprecated('calc_DO_sat')
  }

  with.units <- any(sapply(list(temp.water, pressure.air), is.unitted)) || (if(!missing(salinity.water)) is.unitted(salinity.water) else FALSE)

  if (with.units){
    # if any units are set, they all must be set and must be correct
    verify_units(temp.water, "degC")
    verify_units(pressure.air, "mb")
    verify_units(salinity.water, "PSU")
  }

  # units are stripped regardless
  temp.water <- v(temp.water)
  pressure.air <- v(pressure.air)
  salinity.water <- v(salinity.water)

  o2.at.sat <- LakeMetabolizer::o2.at.sat.base(temp = temp.water, baro = pressure.air, salinity = salinity.water, model = model, ...)

  if (with.units) {
    return(u(o2.at.sat, 'mgO2 L^-1'))
  } else {
    return(o2.at.sat)
  }

}
