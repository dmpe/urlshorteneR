# Bitly_api_version <- "v4"
# Isgd_api_version <- "v2019"
.urlshorteneREnv <- new.env(parent = emptyenv())

#' @title Assign bit.ly API tokens using OAuth2.0
#'
#' @param debug - whether to print additional debug messages
#'
#' @description
#' There are 2 ways of how you can authenticate using this package.
#'
#' 1. The recommended practise for the end-user of this package is to use default API keys which
#' are provided using this method.
#'
#' 2. Alternatively, you can register your own application via the web in order to get Client ID
#' and Client Secret code.
#'
#' For that go first to \url{https://bitly.com/a/oauth_apps}. Click \code{REGISTERED OAUTH APPLICATIONS},
#' then \code{REGISTER NEW APPLICATION} followed by \code{GET REGISTRATION CODE}.
#' Open your email that you will receive and click \code{COMPLETE REGISTRATION}.
#' Make up an \code{APPLICATION NAME} that is unique. Unless you know to do otherwise,
#' type "http://localhost:1410/" (slash at the end is important) in \code{REDIRECT URIs}. For
#' \code{APPLICATION LINK} and \code{APPLICATION DESCRIPTION} you can type whatever you like.
#'
#' @section However Important Information:
#' Before choosing registering new application yourself, you can try using my API keys (the default option). No
#' worries, no information is exposed to me at all: neither what you shorten nor who does it, etc.
#' In fact, quote: "If you are shortening URLs on behalf of end-users, we ask that you use our OAuth 2
#' implementation to authenticate end-users before shortening. URLs shortened in this manner
#' will be added to the specified end-user's history, allowing the end-user to manage and
#' track the shortened URLs".
#'
#' @section WARNING:
#' If using Rstudio in the browsers via RStudio Server, then authentication may not work well. In
#' such case, use desktop RStudio application. See \href{https://support.rstudio.com/hc/en-us/articles/217952868-Generating-OAuth-tokens-for-a-server-using-httr}{RStudio Help}
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
#'   bitly_auth(
#'     key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8",
#'     secret = "f9c6a3b18968e991e35f466e90c7d883cc176073"
#'   )
#'
#' # default variant
#' bitly_token <- bitly_auth()
#' }
#'
#' @import httr
#' @export
bitly_auth <- function(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8",
                      secret = "f9c6a3b18968e991e35f466e90c7d883cc176073", debug = F) {
  token <- httr::oauth2.0_token(
    httr::oauth_endpoint(
      authorize = "https://bitly.com/oauth/authorize",
        access = "https://api-ssl.bitly.com/oauth/access_token"
    ),
    httr::oauth_app("bitly", key = key, secret = secret),
    cache = TRUE
  )
  if (isTRUE(debug)) {
    message(paste0("urlshorteneR: You have been authorized as ", token$credentials$login,
                   " with access token ", token$credentials$access_token))
  }

  .urlshorteneREnv$token <- token
  invisible(token)
}


#' Get Bitly access token
#'
#' Extract token from \code{bitly_auth} method
#'
#' @noRd
#' @keywords internal
bitly_auth_access <- function() {

  if (interactive()) {
    #setwd("~/Documents/R-package-urlshortener")
    bitly_token <- bitly_auth()
  } else {
    #setwd("~/main/")
    #print(getwd())
    bitly_token <- readRDS("../bitly_local_token.rds")
  }
  .urlshorteneREnv$acc_token <- bitly_token$credentials$access_token
  .urlshorteneREnv$token <- bitly_token

  return(.urlshorteneREnv$acc_token)
}


#' @title Generalized function for executing REST requests
#'
#' @param verb - REST verb
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
doRequest <- function(verb = "", url = NULL, queryParameters = NULL, patch_body = NULL, showURL = NULL) {
  switch(verb,
    "PATCH" = {
      return_request <- suppressMessages(httr::PATCH(url,
        query = queryParameters, body = patch_body,
        encode = "json", content_type_json(),
        httr::config(token = .urlshorteneREnv$token)
      ))
    },
    "GET" = {
      return_request <- httr::GET(url, query = queryParameters, httr::config(token = .urlshorteneREnv$token))
    },
    "POST" = {
      return_request <- suppressMessages(httr::POST(url,
        body = queryParameters, encode = "json",
        httr::content_type_json(), httr::config(token = .urlshorteneREnv$token)
      ))
    }
  )

  if (http_error(return_request) == FALSE) {
    stop_for_status(return_request, "you are not a premium account holder, or internet connection does
                    not work properly")

    text_response <- content(return_request, as = "text", encoding = "utf-8")
    json_response <- fromJSON(text_response)

    if (is.null(return_request$status_code) == FALSE && return_request$status_code >= 400) {
      message(sprintf("Code: %s - %s", json_response$message, json_response$description))
    }

    if (identical(showURL, TRUE)) {
      cat("The requested URL has been this: ", return_request$request$url, "\n")
    }
  } else {
    text_response <- content(return_request, as = "text", encoding = "utf-8")
    json_response <- fromJSON(text_response)
    message(sprintf("Code: %s - %s", json_response$message, json_response$description))
    cat("The requested URL has been this: ", return_request$request$url, "\n")
    stop_for_status(return_request)
  }

  return(json_response)
}
