require(dplyr)

# clean and save ICP data
df.raw <- readxl::read_xlsx('./data/ICP/20231222_WMS_PRKNPostProcessed_230920_230927_results/20231222_WMS_PRKNPostProcessed_230920_230927.xlsx',
                            sheet = 'postprocessed')

## retrieve avaerage concentraiton estimates for [Element] mg/L
df.f <- df.raw[grep("^[A-Z]{1}[a-z]{0,1} Quant Average", names(df.raw))]
df.meta <- df.raw[,1:5]
df.elements <- stringr::str_extract(names(df.f), "^[A-Z]{1}[a-z]{0,1}")
names(df.f) <- paste(df.elements, "mg/L")

df.icp <- cbind(df.meta, df.f)

## general filter for checks, blanks, standards
df.icp.f <- df.icp[!grepl("check|blank|std_", stringr::str_to_lower(df.icp$`Sample Name`)),]

# NOTE: comment out if not needed
# optional filter if need custom exclusions
df.icp.f <- df.icp.f[!grepl("WB1|SP18", df.icp.f$`Sample Name`),]

## dilution correction
# seperate _dilX.X into own column, remove from sample name
## df.icp.f <- df.icp[grepl("_dil[0-9]\\.[0-9]+", df.icp$`Sample Name`),]


df.icp.f$dilution <- stringr::str_extract(df.icp.f$`Sample Name`, "_dil[0-9]\\.[0-9]+")
df.icp.f$`Sample Name` <- gsub("_dil[0-9]\\.[0-9]+", "", df.icp.f$`Sample Name`)

# NOTE: comment out if not needed
# custom code for if site/date order is backwards, optional
# split to site on "d" character
df.icp.f$site <- stringr::str_extract(df.icp.f$`Sample Name`, "[^d]*")
# split to date on "d" character
df.icp.f$date <- stringr::str_extract(df.icp.f$`Sample Name`, "d[0-9]*") %>%
  stringr::str_replace("d", "") %>%
  stringr::str_replace("[0-9][0-9]$", "")
# reaarange date to have year (YY) in front
df.icp.f$date <- paste0("23", df.icp.f$date)
# reglue smaple name with correct order([site]d[YYMMDD])
df.icp.f$sample_name <- paste0(df.icp.f$site, "d", df.icp.f$date)

## pull in main data gsheet
main.icp <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1IYyiz5nXaaNizOTHF80uwJSPu2TAzC9VScrlt9aZ9V4/edit#gid=0",
                                      sheet = "data")

all.icp <- main.icp %>%
  left_join(df.icp.f, by = 'sample_name')


if(exists("df.icp.all")) {
    df.icp.all <- all.icp
  }

for(var in names(df.f)) {

  print(paste('merging: ', var))
  var.x = paste0(var,".x")
  var.y = paste0(var,".y")

  if(var.x %in% names(all.icp)) {

    if(!exists("df.icp.all")) {
      icp.i <- all.icp
    } else {
      icp.i <- df.icp.all
    }

    var__flag = paste0(var, "__flag")

     df.icp.all <- icp.i %>%
       mutate(
            !!var := ifelse(is.na(.data[[var.x]]), .data[[var.y]], .data[[var.x]]),
            !!var__flag := ifelse(grepl(" L", .data[[var]]), "BDL", NA),
            !!var__flag := ifelse(grepl(" H", .data[[var]]), "ADL", NA),
            !!var := gsub(" L", "", .data[[var]]),
            !!var := gsub(" H", "", .data[[var]]),
            !!var := as.numeric(.data[[var]], na.pass = TRUE),
        ) %>%
       select(-.data[[var.x]], -.data[[var.y]])

  } else {
    print(paste("Skipping", var))
  }
}

googlesheets4::write_sheet(df.icp.all,
                              ss = "https://docs.google.com/spreadsheets/d/17CMguyAioEv3_qEkUUXe67UbBmJ6ACnNxCKu3W2kmx0/edit#gid=0",
                              sheet = "data")
