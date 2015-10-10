# Bitly_api_version <- "v3"
# Googl_api_version <- "v1"
# Isgd_api_version <- "v2015"

#' @title Assign Bit.ly/Goo.gl API tokens using OAUTH 2
#' 
#' @description You must register an application in order to get Client ID and Client Secret code. 
#' For Bit.ly, go to \url{https://bitly.com/a/oauth_apps} and in the field \code{Redirect URIs:} type
#' for example "http://localhost:1410". 
#' For Goo.gl API Keys you must go to the \url{https://console.developers.google.com/project}, select
#' "APIs & auth", then "Credentials", then "add OAUTH2 client ID" and lastly you select "Type:Other". 
#' 
#' @param key - Client ID
#' @param secret - Client Secret
#' 
#' @seealso See \url{http://dev.bitly.com/rate_limiting.html}
#' @seealso See \url{http://dev.bitly.com/authentication.html}
#' @seealso See \url{https://developers.google.com/url-shortener/v1/getting_started#APIKey}
#' 
#' @examples
#' google_token <- google_auth(
#' key = "806673580943-78jdskus76fu7r0m21erihqtltcka29i.apps.googleusercontent.com", 
#' secret = "qItL-PZnm8GFxUOYM0zPVr_t")
#' bitly_token <- bitly_auth(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8", 
#' secret = "b7e4abaf8b26ec4daa92b1e64502736f5cd78899")
#' 
#' @import httr
#' @export
google_auth <- function(key = "", secret = "") {
  google_token <- httr::oauth2.0_token(httr::oauth_endpoints("google"),
                                       httr::oauth_app("google", key = key, secret = secret),
                                       scope = "https://www.googleapis.com/auth/urlshortener",
                                       cache = TRUE)
  return(google_token)
}


#' @rdname google_auth
#' @export
bitly_auth <- function(key = "", secret = "") {
  bitly_token <- httr::oauth2.0_token(httr::oauth_endpoint(authorize = "https://bitly.com/oauth/authorize",
                                                           access = "https://api-ssl.bitly.com/oauth/access_token"),
                                      httr::oauth_app("bitly", key = key, secret = secret),
                                      cache = TRUE)
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
doRequest <- function(verb, url, service = "", queryParameters = NULL, showURL = NULL) {
  
  if (service == "bitly") {
    service_token <- bitly_token
  } else if (service == "googl") {
    service_token <- google_token
  } else {
    service_token <- NULL
  }
  
  switch(verb,
         "GET" = {
           return_request <- httr::GET(url, query = queryParameters, config(token = service_token))
         },
         "POST" = {
           return_request <- httr::POST(url, body = queryParameters, encode = "json", 
                                        httr::content_type_json(), config(token = service_token))
         }
  )
  
  stop_for_status(return_request)
  text_response <- content(return_request, as = "text")
  json_response <- fromJSON(text_response)
  
  if (identical(showURL, TRUE)) {
    # was return_request$request$opts$url
    cat("The requested URL has been this: ", return_request$request$url, "\n") 
  }
  return(json_response)
}



