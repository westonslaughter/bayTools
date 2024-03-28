#' Script for functions which, based on a lat/long, can retrieve desired site info
#'
#'
#'
#' @details
#'
#'
#'
#'
#'
#' @keywords site

require(dplyr)

site_gather_wqm <- function(
                            sheet_name = "nhd" # options are "nhd" "site_data" and "site_data_ext"
                            ) {

      site_info_wqm <- googlesheets4::read_sheet(
                   ss = "https://docs.google.com/spreadsheets/d/1Ez07N7a6Qh-vAWBu1CVS1S2UuI_58gcKvwrZDDKMsbI/edit#gid=906777155",
                   sheet = sheet_name
                    )

      return(site_info_wqm)
    }

site_gather_nhd <- function(
                            long_lat = c(-77.038166, 38.790702),
                            terminal_point = c(-76.369374, 38.000710)
                            ) {}


## terminal_point_sf <- sf::st_sfc(sf::st_point(terminal_point), crs = 4326)
## x.terminal <- nhdplusTools::discover_nhdplus_id(terminal_point_sf)

## point <- sf::st_sfc(sf::st_point(long_lat), crs = 4326)
## x <- nhdplusTools::discover_nhdplus_id(point)

## x.nhd <- nhdplusTools::get_nhdplus(comid = x)

## x.nhd.p <- nhdplusTools::prepare_nhdplus(x.nhd) %>%
##   mutate(
##     toCOMID = x.terminal
##   )

## fl <- dplyr::select(x.nhd.p,
##                     ID = COMID,
##                     toID = toCOMID,
##                     length = LENGTHKM)

## outlet <- fl$ID[which(!fl$toID %in% fl$ID)]
## terminal_comid <- nhdplusTools::get_terminal(fl, outlet)

## nhdplusTools::get_pathlength(fl)
