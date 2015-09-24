# Bitly_api_version <- "v3"
# Owly_api_version <- "v1.1"
# Googl_api_version <- "v1"
# Isgd_api_version <- "v2015"

#' @title Assign Bit.ly/Ow.ly/Goo.gl API token or authenticate using OAUTH 2
#' 
#' @param auth_token - parameter to set Bit.ly Generic Access Token \code{\link{rbitlyApi}}, 
#' Ow.ly API key \url{http://ow.ly/api-docs} or Goo.gl API Key \url{https://console.developers.google.com/project}
#' @param key - Client ID
#' @param secret - Client Secret
#' 
#' @seealso See \url{http://dev.bitly.com/rate_limiting.html}
#' @seealso See \url{http://dev.bitly.com/authentication.html}
#' @seealso See \url{http://ow.ly/api-docs}
#' @seealso See \url{https://developers.google.com/url-shortener/v1/getting_started#APIKey}
#' 
#' @examples
#' options(Bit.ly = "0906523ec6a8c78b33f9310e84e7a5c81e500909", Ow.ly = "F1QH-Q64B-BSBI-JASJ", Goo.gl = "")
#' ## OR
#' google_auth(key = "806673580943-78jdskus76fu7r0m21erihqtltcka29i.apps.googleusercontent.com", secret = "qItL-PZnm8GFxUOYM0zPVr_t")
#' bitly_auth(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8", secret = "b7e4abaf8b26ec4daa92b1e64502736f5cd78899")
#' 
#' @import httr
#' @export
google_auth <- function(key = "", secret = "") {
  google_token <- httr::oauth2.0_token(httr::oauth_endpoints("google"),
                                       httr::oauth_app("google", key = key, secret = secret),
                                       scope = "https://www.googleapis.com/auth/urlshortener",
                                       cache = TRUE)
  google_token <- readRDS('.httr-oauth')
}

#' @rdname google_auth
#' @export
bitly_auth <- function(key = "", secret = "") {
  bitly_token <- httr::oauth2.0_token(httr::oauth_endpoint(authorize = "https://bitly.com/oauth/authorize",
                                                           access = "https://api-ssl.bitly.com/oauth/access_token"),
                                      httr::oauth_app("bitly", key = key, secret = secret),
                                      cache = TRUE)
  bitly_token <- readRDS('.httr-oauth')
  
}

#' @rdname google_auth
#' @export
auth_bitly <- function(auth_token) {
  tmp <- if (is.null(auth_token)) {
    Sys.getenv("Bit.ly", "")
  } else auth_token
  
  if (tmp == "") {
    getOption("Bit.ly", stop("you need to set up your bit.ly api key"))
  } else tmp
}

#' @rdname google_auth
#' @export
auth_owly <- function(auth_token) {
  tmp <- if (is.null(auth_token)) {
    Sys.getenv("Ow.ly", "")
  } else auth_token
  
  if (tmp == "") {
    getOption("Ow.ly",  stop("you need to set up your ow.ly api key"))
  } else tmp
}

#' @rdname google_auth
#' @export
auth_googl <- function(auth_token) {
  tmp <- if (is.null(auth_token)) {
    Sys.getenv("Goo.gl", "")
  } else auth_token
  
  if (tmp == "") {
    getOption("Goo.gl",  stop("you need to set up your Goo.gl api key"))
  } else tmp
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
doRequest <- function(verb, url, queryParameters = NULL, showURL = NULL) {
  
  switch(verb,
         "GET" = {
           return_request <- httr::GET(url, query = queryParameters)
         },
         "POST" = {
           return_request <- httr::POST(url, body = queryParameters, encode = "json", httr::content_type_json())
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









