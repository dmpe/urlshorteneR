rbitly_api_auth_token <- NA
rbitly_api_version <- "v3"

#' @title Assign Bit.ly API token
#' 
#' @param auth_token - Passed parameter to set Bit.ly Generic Access Token \code{\link{rbitlyApi}}.
#' 
#' @seealso See \url{http://dev.bitly.com/rate_limiting.html}
#' @seealso See \url{http://dev.bitly.com/authentication.html}
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' 
#' @export 
rbitlyApi <- function(auth_token) {
  if (!missing(auth_token)) {
    assignInMyNamespace('rbitly_api_auth_token', auth_token)
  }
  invisible(rbitly_api_auth_token)
}


#' @title Get & return & assign Bit.ly API token using username and password
#' 
#' @description This method is for the case when the user doesn't know what is his/her "Generic Access Token". 
#' When inserting username/email and password it will return the key and assign it using 
#' \code{\link{rbitlyApi}} method in the namespace.
#' 
#' @seealso See \url{http://dev.bitly.com/rate_limiting.html}
#' @seealso See \url{http://dev.bitly.com/authentication.html}
#' 
#' @param username - the username or an email
#' @param password - the password
#' 
#' @return api key - user's API Key. As described, it is not necessary to use rbitlyApi("api key") 
#' as this is done automatically.
#' 
#' @examples 
#' \donttest{
#' rbitlyApi_up("YourUsername", "YourPassword")
#' }
#' 
#' @import httr
#' @export
rbitlyApi_up <- function(username, password) {
  
  access_token_url <- "https://api-ssl.bitly.com/oauth/access_token"
  autenticate <- authenticate(username, password)
  
  req <- POST(access_token_url, autenticate, encode = "multipart")
  
  API_Key <- content(req)
  rbitlyApi(API_Key)
  
  return(API_Key)
}

#' @title Generalized function for executing GET requests by always appending user's Bit.ly API Key.
#' 
#' @param url - which is used for the request
#' @param authcode - calls the rbitlyApi \code{\link{rbitlyApi}}
#' 
#' @import httr
#' @import jsonlite
#' 
#' @noRd
doRequest <- function(url, queryParameters = NULL, auth_code = rbitlyApi()) {
  
  if (is.na(auth_code)) {
    # actually unnecessary; flawn logic because queryParameters will always contain API Key. 
    # Yet for making sure that the user has set it, I'll let it go
    stop("Please assign your API Key ('Generic Access Token') ")
  } else {
    
    return_request <- GET(url, query = queryParameters)
    stop_for_status(return_request)
    text_response <- content(return_request, as = "text")
    json_response <- fromJSON(text_response)
    return(json_response)
  }
}


