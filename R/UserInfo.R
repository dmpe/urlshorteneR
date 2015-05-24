#'
#' Return or update information about a user.
#'
#'
#'
#' @export
user.info <- function() {
  user.info.url <- "https://api-ssl.bitly.com/v3/user/info"
  doRequest(user.info.url)
}

#' @import httr
#' @import jsonlite
#' @export
doRequest <- function(url) {
  createdUrl <- paste(user.info.url, rbitly.api.auth_token, sep = "?access_token=" )
  
  returnGetRequest <- GET(createdUrl)
  content(returnGetRequest)
  
  text_response <- httr::content(returnGetRequest, as="text")
  return(jsonlite::fromJSON(text_response, simplifyVector=TRUE))
}