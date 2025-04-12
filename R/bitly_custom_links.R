#' @title Add Custom Bitlink (Premium)
#'
#' @description
#' Add a Keyword to a Bitlink
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#addCustomBitlink}
#' @inheritParams bitly_add_cust_bitlink
#'
#' @param bitlink_id - string
#' @inheritParams bitly_retrieve_destination_metrics
#' @inheritParams bitly_retrieve_metrics_by_referrers
#'
#' @section Custom Bitlinks:
#' Custom Bitlinks have both a branded short domain (BSD) AND a customized backend.
#' For example, bit.ly/bitlinks would not be considered a Custom Bitlink because it
#' does not have a branded short domain. es.pn/2yxklu would not be considered a custom
#' Bitlink because while it has a branded short domain, it doesn't have a customized
#' backhalf. An example of a link that would live in this section is es.pn/SuperBowl
#'
#' @examples
#' \dontrun{
#' bitly_add_cust_bitlink(custom_bitlink = "es.pn/SuperBowl", bitlink_id = "")
#' }
#'
#' @import httr2 jsonlite lubridate
#'
#' @export
bitly_add_cust_bitlink <- function(access_token, bitlink_id = NULL, custom_bitlink = NULL, showRequestURL = FALSE) {
  cust_post_url <- "https://api-ssl.bitly.com/v4/custom_bitlinks"

  body_req_query <- list(access_token = bitly_auth_access(), bitlink_id = bitlink_id,
                         custom_bitlink = custom_bitlink
  )

  df_add_keywords <- doBearerTokenRequest("POST", cust_post_url, queryParameters = body_req_query,
                               showURL = showRequestURL)
  return(df_add_keywords)
}

#' Get Metrics for a Custom Bitlink by destination (Premium)
#' @seealso \url{https://dev.bitly.com/api-reference/#getCustomBitlinkMetricsByDestination}
#'
#' @description
#' Get Click Metrics for a Custom Bitlink by historical Bitlink destinations
#'
#' @param custom_bitlink - A Custom Bitlink made of the domain and keyword
#'
#' @inheritParams bitly_retrieve_group_pref
#'
#' @examples
#' \dontrun{
#' bitly_retrieve_destination_metrics(custom_bitlink = "es.pn/SuperBowl")
#' }
#'
#' @import httr2 jsonlite lubridate
#'
#' @export
bitly_retrieve_destination_metrics <- function(access_token, custom_bitlink = NULL, showRequestURL = FALSE) {

  if (is.string(custom_bitlink)) {
    metrics_url <- paste0("https://api-ssl.bitly.com/v4/custom_bitlinks/", custom_bitlink, "/clicks_by_destination")
  } else {
    stop("custom_bitlink must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access(), custom_bitlink = custom_bitlink)

  df_cust_metrics <- doBearerTokenRequest("GET", url = metrics_url, queryParameters = query, showURL = showRequestURL)
  df_cust_metrics <- data.frame(df_cust_metrics, stringsAsFactors = FALSE)

  return(df_cust_metrics)
}


#' Update Custom Bitlink (Premium)
#'
#' @description
#' Move a Keyword to a different Bitlink
#'
#' @inheritParams bitly_add_cust_bitlink
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#updateCustomBitlink}
#'
#' @examples
#' \dontrun{
#' bitly_update_cust_bitlink(custom_bitlink = "es.pn/SuperBowl", bitlink_id = "")
#' }
#'
#' @import httr2 jsonlite lubridate
#'
#' @export
bitly_update_cust_bitlink <- function(access_token, custom_bitlink = NULL, bitlink_id = NULL, showRequestURL = FALSE) {

  if (is.string(custom_bitlink)) {
    patch_url <- paste0("https://api-ssl.bitly.com/v4/custom_bitlinks/", custom_bitlink)
  } else {
    stop("custom_bitlink must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access())
  body_req_query <- list(bitlink_id = bitlink_id)

  df_update_cost_link <- doBearerTokenRequest("PATCH", patch_url, queryParameters = query, patch_body = body_req_query,
                               showURL = showRequestURL)

  return(df_update_cost_link)
}


#' Retrieve Custom Bitlink (Premium)
#'
#' @description
#' Retrieve the details and history of a Custom Bitlink
#'
#' @inheritParams bitly_add_cust_bitlink
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getCustomBitlink}
#'
#' @examples
#' \dontrun{
#' bitly_retrieve_cust_bitlink(custom_bitlink = "es.pn/SuperBowl")
#' }
#'
#' @import httr2 jsonlite lubridate
#'
#' @export
bitly_retrieve_cust_bitlink <- function(access_token, custom_bitlink = NULL, showRequestURL = FALSE) {

  if (is.string(custom_bitlink)) {
    get_url <- paste0("https://api-ssl.bitly.com/v4/custom_bitlinks/", custom_bitlink)
  } else {
    stop("custom_bitlink must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access(), custom_bitlink = custom_bitlink)
  df_cust_metrics <- doBearerTokenRequest("GET", url = get_url, queryParameters = query, showURL = showRequestURL)
  return(df_cust_metrics)
}

#' Get Clicks for a Custom Bitlink's Entire History (Premium)
#'
#' @description
#' Returns the click counts for the specified link. This returns an array with clicks based on a date.
#'
#' @inheritParams bitly_add_cust_bitlink
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getClicksForCustomBitlink}
#'
#' @examples
#' \dontrun{
#' bitly_retrieve_cust_bitlink_clicks_history(custom_bitlink = "es.pn/SuperBowl")
#' }
#'
#' @import httr2 jsonlite lubridate
#'
#' @export
bitly_retrieve_cust_bitlink_clicks_history <- function(access_token, custom_bitlink = NULL, showRequestURL = FALSE) {
  
  if (is.string(custom_bitlink)) {
    get_url <- paste0("https://api-ssl.bitly.com/v4/custom_bitlinks/", custom_bitlink, "/clicks")
  } else {
    stop("custom_bitlink must not be empty string, NA or NULL")
  }
  
  query <- list(access_token = bitly_auth_access(), custom_bitlink = custom_bitlink)
  df_cust_metrics_history <- doBearerTokenRequest("GET", url = get_url, queryParameters = query, showURL = showRequestURL)
  return(df_cust_metrics_history)
}

#' Get Metrics for a Custom Bitlink by Destination (Premium)
#'
#' @description
#' Returns click metrics for the specified link by its historical destinations.
#'
#' @inheritParams bitly_add_cust_bitlink
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getCustomBitlinkMetricsByDestination}
#'
#' @examples
#' \dontrun{
#' bitly_retrieve_cust_bitlink_metrics_destination(custom_bitlink = "es.pn/SuperBowl")
#' }
#'
#' @import httr2 jsonlite lubridate
#'
#' @export
bitly_retrieve_cust_bitlink_metrics_destination <- function(access_token, custom_bitlink = NULL, showRequestURL = FALSE) {
  
  if (is.string(custom_bitlink)) {
    get_url <- paste0("https://api-ssl.bitly.com/v4/custom_bitlinks/", custom_bitlink, "/clicks_by_destination")
  } else {
    stop("custom_bitlink must not be empty string, NA or NULL")
  }
  
  query <- list(access_token = bitly_auth_access(), custom_bitlink = custom_bitlink)
  df_cust_metrics_dest <- doBearerTokenRequest("GET", url = get_url, queryParameters = query, showURL = showRequestURL)
  return(df_cust_metrics_dest)
}

