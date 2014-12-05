#' Exoplanet Name
#'
#' This will return infomation about the requested exoplanet, 
#' @title Search for exoplanet infomation
#' @param exoplanet a string of an existing exoplanet
#' @return JSON object with infomation about the queried exoplanet
#' @keywords Name
#' @examples
#' \dontrun{
#' library(RCurl)
#' library(RJSONIO)
#' ###Return Infomation about the exoplanet 11 Com
#' exoplanetname("11 Com")
#' ###Return Infomation about the exoplanet Kepler-9
#' exoplanetname("Kepler-9")
#' ###Return Infomation about all exoplants in the system
#' exoplanetname("")
#' }
#' @export

exoplanetname <- function (exoplanet) {
  internetcheck <- url.exists("http://star-api.herokuapp.com", timeout = 10)
  if( internetcheck != TRUE)
    stop('Hacktheuniverse or your internet connection is down')
  urldata <- paste('http://star-api.herokuapp.com/api/v1/exo_planets/', URLencode(exoplanet), sep = "")
  data <- getURL(urldata)
  dataFrame <- RJSONIO::fromJSON(data)
  return (dataFrame)
}
