# rbitly_api_auth_token <- NA
# 
# Bitly_api_version <- "v3"
# Owly_api_version <- "v1.1"
# Googl_api_version <- "v1"

#' @title Assign Bit.ly/Ow.ly/Goo.gl API token
#' 
#' @param auth_token/x - parameter to set Bit.ly Generic Access Token \code{\link{rbitlyApi}}, 
#' Ow.ly API key \url{http://ow.ly/api-docs} or Goo.gl API Key  \url{https://console.developers.google.com/project}
#' 
#' @seealso See \url{http://dev.bitly.com/rate_limiting.html}
#' @seealso See \url{http://dev.bitly.com/authentication.html}
#' @seealso See \url{http://ow.ly/api-docs}
#' @seealso See \url{https://developers.google.com/url-shortener/v1/getting_started#APIKey}
#' 
#' @examples
#' ## rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909") # now deprecated
#' options(Bit.ly = "0906523ec6a8c78b33f9310e84e7a5c81e500909", Ow.ly = "F1QH-Q64B-BSBI-JASJ", 
#' Goo.gl = "") # use this for Bit.ly access
#' 
#' @export 
#' @importFrom utils assignInMyNamespace
rbitlyApi <- function(auth_token) {
  .Deprecated(new = "auth_bitly", msg = "This method is now deprecated; please use auth_bitly or auth_owly. Thank you!")
  #   if (!missing(auth_token)) {
  #     assignInMyNamespace("rbitly_api_auth_token", auth_token)
  #   }
  #   invisible(rbitly_api_auth_token)
}

#' @rdname rbitlyApi
#' @export
auth_bitly <- function(auth_token) {
  tmp <- if (is.null(auth_token)) {
    Sys.getenv("Bit.ly", "")
  } else auth_token
  
  if (tmp == "") {
    getOption("Bit.ly", stop("you need to set up your bit.ly api key"))
  } else tmp
}

#' @rdname rbitlyApi
#' @export
auth_owly <- function(x) {
  tmp <- if (is.null(x)) {
    Sys.getenv("Ow.ly", "")
  } else x
  
  if (tmp == "") {
    getOption("Ow.ly",  stop("you need to set up your ow.ly api key"))
  } else tmp
}

#' @rdname rbitlyApi
#' @export
auth_googl <- function(x) {
  tmp <- if (is.null(x)) {
    Sys.getenv("Goo.gl", "")
  } else x
  
  if (tmp == "") {
    getOption("Goo.gl",  stop("you need to set up your Goo.gl api key"))
  } else tmp
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
#' @return api key - user's API Key. As described, it is not necessary to use auth_bitly("api key") 
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
  auth_bitly(API_Key)
  
  return(API_Key)
}

#' @title Generalized function for executing GET requests by always appending user's API Key.
#' 
#' @param url - which is used for the request
#' @param authcode - calls the rbitlyApi \code{\link{rbitlyApi}}
#' @param queryParameters - parameters that are used for building a URL
#' @param showURL - for debugging purposes only: it shows what URL has been called
#' 
#' @import httr
#' @import jsonlite
#' 
#' @noRd
doRequest <- function(url, queryParameters = NULL, showURL = NULL) {
  
    return_request <- GET(url, query = queryParameters)
    stop_for_status(return_request)
    text_response <- content(return_request, as = "text")
    json_response <- fromJSON(text_response)
    
    if (identical(showURL, TRUE)) {
      # was return_request$request$opts$url
      cat("The requested URL has been this: ", return_request$request$url, "\n") 
    }
    return(json_response)
  
}


#' @title Generalized function for executing POST requests (mostly for Goo.gl)
#' 
#' @param url - which is used for the request
#' @param authcode - calls the rbitlyApi \code{\link{rbitlyApi}}
#' @param queryParameters - parameters that are used for building a URL
#' @param showURL - for debugging purposes only: it shows what URL has been called
#' 
#' @import httr
#' @import jsonlite
#' 
#' @noRd
doRequestPOST <- function(url, queryParameters = NULL, showURL = NULL) {
  
    return_request <- POST(url, encode = "json", content_type_json(), body = queryParameters)
    stop_for_status(return_request)
    text_response <- content(return_request, as = "text")
    json_response <- fromJSON(text_response)
    
    if (identical(showURL, TRUE)) {
      # was return_request$request$opts$url
      cat("The requested URL has been this: ", return_request$request$url, "\n") 
    }
    return(json_response)
  
}
