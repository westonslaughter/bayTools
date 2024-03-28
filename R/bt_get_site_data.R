#’ retrieve Wes Slaughter's UMD water quality monitoring (WQM) site data 
#’
#’ 
#' @param sheet_name character. identify the target sheet for download, options are "site_data", "nhd", and "site_data_ext"
#'
#' @author Wes Slaughter
#' @return returns a \code{data.frame} of the selected sheet
#' @export


bt_get_site_data <- function(sheet_name = "site_data_ext") {
  
  site_info <- googlesheets4::read_sheet(
                    ss = "https://docs.google.com/spreadsheets/d/1Ez07N7a6Qh-vAWBu1CVS1S2UuI_58gcKvwrZDDKMsbI/edit#gid=906777155",
                    sheet = sheet_name
                  )
  
  return(site_info)
}