bitly_token <- NULL

# Bitly_api_version <- "v4"
# Isgd_api_version <- "v2019"

#' @title Assign bit.ly API tokens using OAuth2.0
#' 
#' @description 
#' There are 2 ways of how you can authenticate using this package. 
#' 
#' 1. The recommended practise for the end-user of this package is to use default API keys which 
#' are provided using this method. 
#' 
#' 2. Alternatively, you can register an application via the web in order to get Client ID 
#' and Client Secret code. 
#' 
#' Thus, go first to \url{https://bitly.com/a/oauth_apps}. Click \code{REGISTERED OAUTH APPLICATIONS}, 
#' then \code{REGISTER NEW APPLICATION} followed by \code{GET REGISTRATION CODE}. 
#' Open your email that you will receive and click \code{COMPLETE REGISTRATION}.
#' Make up an \code{APPLICATION NAME} that is unique. Unless you know to do otherwise, 
#' type "http://localhost:1410/" (slash at the end is important) in \code{REDIRECT URIs}. For 
#' \code{APPLICATION LINK} and \code{APPLICATION DESCRIPTION} you can type whatever you like. 
#' 
#' @section Important Information:
#' Before choosing registering new application yourself, you can try using my API keys (the default option). No 
#' worries, no information is exposed to me: neither what you shorten nor who does it, etc. 
#' In fact, quote: "If you are shortening URLs on behalf of end-users, we ask that you use our OAuth 2 
#' implementation to authenticate end-users before shortening. URLs shortened in this manner 
#' will be added to the specified end-user's history, allowing the end-user to manage and 
#' track the shortened URLs".  
#' 
#' @section WARNING:
#' If using Rstudio in the browsers via RStudio Server, then authentication may not work well. In
#' such case, use desktop RStudio application. See \href{https://support.rstudio.com/hc/en-us/articles/217952868-Generating-OAuth-tokens-for-a-server-using-httr}{RStudio Help}
#' 
#' 
#' @param key - Client ID
#' @param secret - Client Secret
#' 
#' @seealso See \url{https://dev.bitly.com/v4_documentation.html}
#' 
#' @examples
#' \dontrun{
#' # overwrite keys - Variant 2
#' bitly_token <-
#'  bitly_auth(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8",
#'              secret = "c7eb384b2f4ce4b109fe616f1c9455e4f7735917")
#'              
#'  # default variant
#'  bitly_token <- bitly_auth()
#' }
#' 
#' @import httr
#' @export
bitly_auth <- function(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8", 
                       secret = "c7eb384b2f4ce4b109fe616f1c9455e4f7735917") {
  token <- httr::oauth2.0_token(httr::oauth_endpoint(authorize = "https://bitly.com/oauth/authorize",
                                                           access = "https://api-ssl.bitly.com/oauth/access_token"),
                                      httr::oauth_app("bitly", key = key, secret = secret),
                                      cache = TRUE)
  message(paste0("urlshorteneR: You have been authorized as ", token$credentials$login))
  assign("bitly_token", token, envir = parent.frame())
  return(token)
}

#' @title Generalized function for executing GET/POST requests
#' 
#' @param verb - GET/POST/UPDATE/DELETE, etc.
#' @param url - which is used for the request
#' @param queryParameters - parameters that are used for building a URL
#' @param showURL - for debugging purposes only: it shows what URL has been called
#' 
#' @import httr
#' @import jsonlite
#' 
#' @return json data
#' 
#' @noRd
#' @keywords internal
doRequest <- function(verb, url, queryParameters = NULL, patch_body = NULL, showURL = NULL, verbose = F) {

  switch(verb,
         "PATCH" = {
           return_request <- suppressMessages(httr::PATCH(url, query = queryParameters, body = patch_body, 
                                                          encode = "json", content_type_json(), 
                                         httr::config(token = bitly_token)))
         },
         "GET" = {
           return_request <- suppressMessages(httr::GET(url, query = queryParameters, 
                                                        httr::config(token = bitly_token), verbose()))
         },
         "POST" = {
           return_request <- suppressMessages(httr::POST(url, body = queryParameters, encode = "json", 
                                        httr::content_type_json(), httr::config(token = bitly_token)))
         }
  )
  
  if (http_error(return_request) == FALSE) {
    stop_for_status(return_request, "you are not a premium account holder, or internet connection does not work properly (check with IT admins) ")
    
    text_response <- content(return_request, as = "text", encoding = "utf-8")
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






