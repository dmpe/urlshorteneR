rbitly.api.auth_token <- NA
rbitly.api.version <- "v3"

#' Assign rbitly token
#' 
#' @param auth_token - Passed parameter to set Bit.ly Generic Access Token \code{\link{rbitlyApi}}.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' 
#' @export 
rbitlyApi <- function(auth_token) {
  if (!missing(auth_token)) {
    assignInMyNamespace('rbitly.api.auth_token', auth_token)
  }
  invisible(rbitly.api.auth_token)
}


#' This method is for the case when the user doesn't know what is his/her "Generic Access Token". When inserting username/email and password it will
#' return the key and assign it using \code{\link{rbitlyApi}} method in the namespace.
#' 
#' @seealso See \url{http://dev.bitly.com/rate_limiting.html}
#' 
#' @param username - the username or an email
#' @param password - the password
#' 
#' @return api key - user's API Key. As described, it is not necessary to use rbitlyApi("api key") as this is done automatically
#' 
#' @examples 
#' \donttest{
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
  rbitlyApi(valueOfApiKey)
  
  return(valueOfApiKey)
}

#' Generalized function for executing GET requests by always appending user's Bit.ly API Key.
#' 
#' @param url - which is used for the request
#' @param authcode - calls the rbitlyApi \code{\link{rbitlyApi}}
#' 
#' @import httr
#' @import jsonlite
#' @import stringr
doRequest <- function(url, authcode = rbitlyApi()) {
  
  if (is.na(authcode)) {
    stop("Please assign your API Key ('Generic Access Token') ")
  } else {
    createdUrl <- paste(url, authcode, sep = "&access_token=")
    
    # fail to parse content correctly
    if(str_detect(url, "v3/user/info")) {
      
      json_response <- fromJSON(createdUrl)
      return(json_response)
      
    } else {

      returnGetRequest <- GET(createdUrl)
      text_response <- content(returnGetRequest, as = "text")
      json_response <- fromJSON(text_response)
      return(json_response)
    }
  }
}


