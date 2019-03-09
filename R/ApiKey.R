.state <- new.env(parent = emptyenv())
globalVariables(c("bitly_token"))

# Bitly_api_version <- "v4"
# Isgd_api_version <- "v2019"

#' @title Assign API tokens using OAuth2.0
#' 
#' @description You should register an application in order to get Client ID and Client Secret code. 
#' For Bit.ly, go to \url{https://bitly.com/a/oauth_apps} and in the field \code{Redirect URIs:} 
#' type for example "http://localhost:1410". 
#' 
#' @param key - Client ID
#' @param secret - Client Secret
#' 
#' @seealso See \url{http://dev.bitly.com/rate_limiting.html}
#' @seealso See \url{http://dev.bitly.com/authentication.html}
#' 
#' @examples
#' \dontrun{
#' bitly_token <-
#'   bitly_auth(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8",
#'              secret = "e12dfc2482c76512b9a497e965abf4e082d1ffeb")
#' }
#' 
#' @import httr
#' @export
bitly_auth <- function(key = "", secret = "") {
  bitly_token <- httr::oauth2.0_token(httr::oauth_endpoint(authorize = "https://bitly.com/oauth/authorize",
                                                           access = "https://api-ssl.bitly.com/oauth/access_token"),
                                      httr::oauth_app("bitly", key = key, secret = secret),
                                      cache = TRUE)
  .state$token <- bitly_token
  return(bitly_token)
}

#' @title Generalized function for executing GET/POST requests
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
#' @keywords internal
doRequest <- function(verb, url, queryParameters = NULL, showURL = NULL) {

  switch(verb,
         "GET" = {
           return_request <- httr::GET(url, query = queryParameters, httr::config(token = bitly_token))
         },
         "POST" = {
           return_request <- httr::POST(url, body = queryParameters, encode = "json", 
                                        httr::content_type_json(), httr::config(token = bitly_token))
         }
  )
  
  if (http_error(return_request) == FALSE) {
    text_response <- content(return_request, as = "text")
    json_response <- fromJSON(text_response)
    
    if (is.null(json_response$status_code) == FALSE && json_response$status_code >= 400) {
      message(sprintf("Code: %s - %s", json_response$status_code, json_response$status_txt))
    }
      
    if (identical(showURL, TRUE)) {
      cat("The requested URL has been this: ", return_request$request$url, "\n") 
    }
    
  } else {
    stop_for_status(return_request)
  }
  
  return(json_response)
}



