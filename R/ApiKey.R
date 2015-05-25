rbitly.api.auth_token <- NA
rbitly.api.version <- "v3"

#' Assign rbitly token automatically if I know it
#' @param auth_token Optionally passed parameter to set Bit.ly Generic Access Token \code{auth_token}.
#' @return Returns invisibly the currently set \code{auth_token}.
#' @examples {
#' rbitlyApi('foobar')
#' }
#' 
#' @export 
rbitlyApi <- function(auth_token) {
  if (!missing(auth_token)) {
    assignInMyNamespace('rbitly.api.auth_token', auth_token)
  }
  invisible(rbitly.api.auth_token)
}


#' This method is for the case if user doesn't know what is his "Generic Access Token". When inserting username and password it will
#' return the key and assign it using \code{auth_token} method in the namespace
#' 
#' @usage http://dev.bitly.com/rate_limiting.html
#' 
#' @param username
#' @param password
#' @examples {
#' returnApiKey("YourUsername", "YourPassword")
#' }
#' 
#' @import httr
#' @export
returnApiKey <- function(username, password) {
  
  access_token.url <- "https://api-ssl.bitly.com/oauth/access_token"
  autenticate <-  authenticate(username, password)
  
  req <- POST(access_token.url, autenticate, encode = "multipart")
  
  valueOfApiKey <- content(req)
  return(valueOfApiKey)
}

#' Generalized function for executing GET Requests
#' 
#' @param url which is used for the request
#' @param authcode calls the rbitlyApi \code{auth_token}
#' @import httr
#' @import jsonlite
doRequest <- function(url, authcode = rbitlyApi()) {
  createdUrl <- paste(url, authcode, sep = "&access_token=")
  
  returnGetRequest <- GET(createdUrl)
  text_response <- content(returnGetRequest, as = "text")
  json_response <- fromJSON(text_response)
}


