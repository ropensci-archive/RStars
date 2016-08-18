#' Query the database for stars with specific attributes
#' 
#' Sends a paginated query to the database and compiles the results back into a
#' \code{data.frame}
#' 
#' @param queries A \code{list} of queries to send. Refer to
#'   \url{http://hacktheuniverse.github.io/star-api/} and example below.
#' @param max_results Maximum number of results to obtain (default 1000)
#'   
#' @return A \code{data.frame} of query results
#'   
#' @importFrom httr modify_url
#' @importFrom RCurl url.exists getURL
#' @importFrom jsonlite fromJSON
#' @export
#' 
#' @examples
#' \dontrun{
#' ## find the (naked eye) visible stars (max 1750)
#' queries <- list("min[appmag]" = -2.5, "max[appmag]" = 6.5)
#' sq <- starquery(queries, max_results = 1750)
#' }
starquery <- function(queries, max_results = 1000) {
  
  base_url <- "http://star-api.herokuapp.com/api/v1/stars"
  
  page <- 1
  query_list <- append(queries, list("page" = page))
  query_url  <- httr::modify_url(base_url, query = query_list)
  
  internetcheck <- try(RCurl::url.exists("http://star-api.herokuapp.com", timeout = 10))
  if (!internetcheck) stop("Hacktheuniverse or your internet connection is down")
  emptypage <- FALSE
  message(paste("Page:", page, "... "), appendLF = FALSE)
  data <- RCurl::getURL(query_url)
  dataFrame <- jsonlite::fromJSON(data)
  if (nrow(dataFrame) > max_results) dataFrame <- dataFrame[1:max_results, ]
  
  nresults <- nrow(dataFrame)
  while ((!emptypage) & (nresults <= max_results)) {
    page <- page + 1
    query_list <- append(queries, list("page" = page))
    query_url <- httr::modify_url(base_url, query = query_list)
    message(paste(page, "... "), appendLF = FALSE)
    data <- RCurl::getURL(query_url)
    moredataFrame <- jsonlite::fromJSON(data)
    emptypage <- nrow(moredataFrame) == 0
    dataFrame <- rbind(dataFrame, moredataFrame)
    nresults <- nrow(dataFrame)
  }
  
  ifelse(nresults > max_results, return(dataFrame[1:max_results, ]), return(dataFrame))
  
}

