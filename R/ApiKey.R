# Bitly_api_version <- "v4"
# Isgd_api_version <- "v2019"
.urlshorteneREnv <- new.env(parent = emptyenv())

#' @noRd
#' @keywords internal
processResponse <- function(resp) {
  if (resp_is_error(resp) == TRUE) {
    cat("you are not a premium account holder, or internet connection does not work properly")
  } else {
    text_response <- resp |> resp_body_string()
    json_response <- fromJSON(text_response)
    message(sprintf("Code: %s - %s", json_response$message, json_response$description))
    cat("The requested URL has been this: ", resp$request$url, "\n")
  }
}


#' @title Generalized function for executing non-auth REST requests
#'
#' @param verb - REST verb
#' @param url - which is used for the request
#' @param queryParameters - parameters that are used for building a URL
#' @param showURL - for debugging purposes only: it shows what URL has been called
#'
#' @import httr2
#' @import jsonlite
#'
#' @return json data
#'
#' @noRd
#' @keywords internal
doNoAuthRequest <- function(verb = "", url = NULL, queryParameters = NULL, patch_body = NULL, showURL = NULL) {
  req <- httr2::request(url)
  switch(verb,
         "PATCH" = {
           resp <- req |>
             req_url_query(!!!queryParameters) |>
             req_method("PATCH") |>
             req_body_json(list(patch_body)) |>
             req_headers(
               Accept = "application/json"
             ) |>
             req_perform()
         },
         "GET" = {
           resp <- req |> 
             req_url_query(!!!queryParameters) |> 
             req_method("GET") |>
             req_perform()
         },
         "POST" = {
           resp <- req |>
             req_url_query(!!!queryParameters) |>
             req_method("POST") |>
             req_body_json(list(patch_body)) |>
             req_headers(
               Accept = "application/json"
             ) |>
             req_perform()
         }
  )
  
  json_response <- processResponse(resp)
  
  return(json_response)
}


#' @title Generalized function for executing bearer token REST requests
#'
#' @param verb - REST verb
#' @param url - which is used for the request
#' @param access_token - Bearer token
#' @param queryParameters - parameters that are used for building a URL
#' @param showURL - for debugging purposes only: it shows what URL has been called
#'
#' @import httr2
#' @import jsonlite
#'
#' @return json data
#'
#' @noRd
#' @keywords internal
doBearerTokenRequest <- function(verb = "", url = NULL, access_token=NULL, queryParameters = NULL, patch_body = NULL, showURL = NULL) {
  req <- httr2::request(url)
  switch(verb,
    "PATCH" = {
      resp <- req |>
          req_url_query(!!!queryParameters) |>
          req_method("PATCH") |>
          req_body_json(list(patch_body)) |>
          req_auth_bearer_token(access_token) |>
          req_headers(
            Accept = "application/json"
          ) |>
          req_perform()
    },
    "GET" = {
      resp <- req |> 
        req_url_query(!!!queryParameters) |> 
        req_method("GET") |>
        req_auth_bearer_token(access_token) |>
        req_perform()
    },
    "POST" = {
      resp <- req |>
        req_url_query(!!!queryParameters) |>
        req_method("POST") |>
        req_body_json(list(patch_body)) |>
        req_auth_bearer_token(access_token) |>
        req_headers(
          Accept = "application/json",
          "Content-Type" = "application/json"
        ) |>
        req_perform()
    }
  )

  json_response <- processResponse(resp)

  return(json_response)
}
