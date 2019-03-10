.state <- new.env(parent = emptyenv())
globalVariables(c("bitly_token"))

# Bitly_api_version <- "v4"
# Isgd_api_version <- "v2019"

#' @title Assign API tokens using OAuth2.0
#' 
#' @description You should register an application in order to get Client ID and Client Secret code. 
#' For Bit.ly, go to \url{https://bitly.com/a/oauth_apps}. Click \code{REGISTERED OAUTH APPLICATIONS}, 
#' then \code{GET REGISTRATION CODE}. Open the email you will receive and click \code{COMPLETE REGISTRATION}.
#' Make up an \code{APPLICATION NAME:} that is unique. Unless you know to do otherwise, 
#' type "http://localhost:1410/" (slash at the end is important) in \code{REDIRECT URIs:}. For 
#' \code{APPLICATION LINK:} and \code{APPLICATION DESCRIPTION:} you can type what you like. 
#' 
#' @section Important Information
#' Nonetheless, you can also use my API keys (default option). No information is exposed 
#' that way to me: neither what you shorten nor who does it. 
#' 
#' In fact, this is a recommended practise for the end-user. 
#' Quote: If you are shortening URLs on behalf of end-users, we ask that you use our OAuth 2 
#' implementation to authenticate end-users before shortening. URLs shortened in this manner 
#' will be added to the specified end-user’s history, allowing the end-user to manage and 
#' track the shortened URLs.  
#' 
#' @section WARNING
#' If using Rstudio in the browsers via RStudio Server, then authentication may not work well. In
#' such case, use desktop RStudio. See \link{https://support.rstudio.com/hc/en-us/articles/217952868-Generating-OAuth-tokens-for-a-server-using-httr}
#' 
#' 
#' @param key - Client ID
#' @param secret - Client Secret
#' 
#' @seealso See \url{https://dev.bitly.com/v4_documentation.html}
#' 
#' @examples
#' \dontrun{
#' bitly_token <-
#'   bitly_auth(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8",
#'              secret = "c7eb384b2f4ce4b109fe616f1c9455e4f7735917")
#'              
#'  bitly_token <- bitly_auth()
#' }
#' 
#' @import httr
#' @export
bitly_auth <- function(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8", secret = "c7eb384b2f4ce4b109fe616f1c9455e4f7735917") {
  bitly_token <- httr::oauth2.0_token(httr::oauth_endpoint(authorize = "https://bitly.com/oauth/authorize",
                                                           access = "https://api-ssl.bitly.com/oauth/access_token"),
                                      httr::oauth_app("bitly", key = key, secret = secret),
                                      cache = TRUE)
  .state$token <- bitly_token
  return(bitly_token)
}

#' @title Generalized function for executing GET/POST requests
#' @param verb - GET/POST/UPDATE/DELETEetc.
#' @param url - which is used for the request
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
         "PATCH" = {
           return_request <- httr::PATCH(url, body = queryParameters, encode = "json", httr::config(token = bitly_token))
         },
         "GET" = {
           return_request <- httr::GET(url, query = queryParameters, httr::config(token = bitly_token))
         },
         "POST" = {
           return_request <- httr::POST(url, body = queryParameters, encode = "json", 
                                        httr::content_type_json(), httr::config(token = bitly_token))
         }
  )
  
  if (http_error(return_request) == FALSE) {
    stop_for_status(return_request)
    
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



