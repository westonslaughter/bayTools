#’ retrieve Wes Slaughter's UMD water quality monitoring (WQM) data 
#’
#' @author Wes Slaughter
#’ 
#' @param sheet_name character. identify the target sheet for download, defaults are WMS sheets 
#' @return returns a \code{data.frame} of the combined WQ field, lab, weather sheet
#' @export

bt_get_wqm_data <- function(
    wq.field.ss = "https://docs.google.com/spreadsheets/d/1ASyFVJU3UBVRAC9Ys-Vz8PuVIsJWiFMuJJab42NgEbQ/edit#gid=1906575016", 
    wq.sites.ss = "https://docs.google.com/spreadsheets/d/1Ez07N7a6Qh-vAWBu1CVS1S2UuI_58gcKvwrZDDKMsbI/edit#gid=0",
    wq.lab.ss = "https://docs.google.com/spreadsheets/d/17CMguyAioEv3_qEkUUXe67UbBmJ6ACnNxCKu3W2kmx0/edit#gid=0"
                            ) {

  print('wq field sheet read')
  wq.field <- googlesheets4::read_sheet(ss = wq.field.ss)
  
  
  print('wq site sheet read')
  wq.sites <- googlesheets4::read_sheet(ss = wq.sites.ss, 
                         sheet = "site_data_ext")
  
  # summarize wq field data to get relevant info (start/end date)
  wq.field.stat <- wq.field %>%
    group_by(Site) %>%
    summarize(
      start_date = min(Date),
      end_date = max(Date)
    ) %>%
    rename(
      site_code = Site
    ) %>% 
    filter(
        start_date > "1940-01-01",
        end_date < as.character(Sys.Date()),
      )
  
  wq.weather <- wq.sites %>%
    select(site_code, lat, long) %>%
    left_join(wq.field.stat, by = "site_code") 
  
  wq.weather.run <- wq.weather %>%
    filter(!is.na(start_date)) %>%
    mutate(
      start_date = as.Date(start_date),
      end_date = as.Date(end_date)
    ) 
  
  # cleaning
  wq.field.weather <- wq.field %>%
    rename(
      site_code = Site,
      date = Date
    )
  
  
  # rm(wq.weather.data)
  
  # retrieve weather
  for(i in 1:nrow(wq.weather.run)) {
    
    site_row    =  wq.weather.run[i,]
    site        =  site_row$site_code
    site_coords =  c(site_row$lat, site_row$long)
    site_start  =  site_row$start_date - 7 # one week before start
    site_end    =  site_row$end_date - 1 # one day before end (in case end is today)
    
    print(paste('querying openmeteo weather data for site:', site))
    site.weather <- openmeteo::weather_history(
                        location = site_coords,
                        start = site_start,
                        end = site_end,
                        daily = c('temperature_2m_mean', 
                                  'precipitation_sum', 
                                  'rain_sum',
                                  'snowfall_sum',
                                  'windspeed_10m_mean',
                                  'shortwave_radiation_sum'
                                  )
                      ) %>%
                    mutate(
                      site_code = site,
                      precip_3day_sum = zoo::rollsum(daily_precipitation_sum, 3, fill = NA),
                      rain_3day_sum = zoo::rollsum(daily_rain_sum, 3, fill = NA),
                      snow_3day_sum = zoo::rollsum(daily_snowfall_sum, 3, fill = NA),
                      temp_2day_mean = zoo::rollmean(daily_temperature_2m_mean, 3, fill = NA)
                    )
    
    site.field.weather.row = left_join(wq.field.weather, site.weather, by = c('site_code', 'date')) %>%
      filter(site_code == site)
    
    if(!exists("wq.weather.data")) {
      wq.weather.data <- site.field.weather.row
    } else {
      wq.weather.data <- rbind(wq.weather.data, site.field.weather.row)
    }
    
  }
  
  wq.weather.data <- wq.weather.data %>%
    distinct()
  
  # rm(wq.weather.data)
  # write_sheet(wq.weather.data,
  #             ss = "https://docs.google.com/spreadsheets/d/1BB2YELUcibd1c8N9EfHr3OGz1gAhNZwy-O6I2wAAkNc/edit#gid=0",
  #             sheet = "wq_weather")
  
  
  # merge with lab data
  wq.lab <- googlesheets4::read_sheet(ss = wq.lab.ss)
  
  wq.lab.f <- wq.lab %>%
    mutate(
      date = as.Date(Date, format = "%y%m%d"),
      site_code = Site
    )
  
  wq.lab.f <- wq.lab.f %>%
    distinct()
  
  # NOTE: add "dup" duplicate handling, this is source of many-to-many warning on join
  wq.all <- left_join(wq.weather.data, wq.lab.f, by = c("site_code", "date"))
  
  # googlesheets4::write_sheet(wq.all, 
  #             ss = "https://docs.google.com/spreadsheets/d/1BB2YELUcibd1c8N9EfHr3OGz1gAhNZwy-O6I2wAAkNc/edit#gid=0", 
  #             sheet = "wq_field_lab_weather")
  
  return(wq.all)  
}
