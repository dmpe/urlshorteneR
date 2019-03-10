#' @title Retrieve information for the current authenticated user
#' 
#' @param showRequestURL - show URL which has been build and requested from server. 
#' For debug purposes.
#'
#' @description See \url{https://dev.bitly.com/v4/#operation/getUser}
#'
#' @return login - the specified bitly login or the login of the authenticated user.
#' @return name - the user's full/display name
#' @return default_group_guid	
#' @return created - Timestamp for the moment the user signed up.
#' @return is_created
#' @return modified
#' @return is_sso_user
#' @return is_2fa_enabled
#' @return email 
#' 
#' @note Both returned columns (!) are character type.
#'
#' @examples 
#' \dontrun{ 
#'      bitly_token <- bitly_auth()
#'      ui <- bitly_UserInfo(showRequestURL = TRUE) 
#' }
#' 
#' @import stringr
#' 
#' @export
bitly_UserInfo <- function(showRequestURL = FALSE) {
  
  user_info_url <- "https://api-ssl.bitly.com/v4/user"
  
  query <- list(access_token = bitly_token$credentials$access_token)
  
  df_user_info <- doRequest("GET", user_info_url, query, showURL = showRequestURL)
  
  df_user_info_data <- data.frame(df_user_info, stringsAsFactors = FALSE)

  # convert to readable format 
  df_user_info_data[5, 1] <- as.character(as.POSIXct(as.integer(df_user_info_data[5, 1]), 
                                                     origin = "1970-01-01", tz = "UTC"))
  
  return(df_user_info_data)
}

#' @title Returns entries from a user's link history in reverse chronological order.
#' 
#' @description See \url{http://dev.bitly.com/user_info.html#v3_user_link_history}
#' 
#' @param limit - optional integer in the range 1 to 100; default: 100, specifying the max 
#' number of results to return.
#' @param expand_client_id - true or false (always default) whether to provide additional 
#' information about encoding application. 
#' @param archived - on, off (default) or both whether to include or exclude archived history 
#' entries. (on = return only archived history entries)
#' @param private - on, off and both (default) whether to include or exclude private history 
#' entries. (on = return only private history entries)
#' @param showRequestURL - show URL which has been build and requested from server. 
#' For debug purposes.
#'
#' @return link - the Bitlink specific to this user and this long_url.
#' @return aggregate_link - the global bitly identifier for this long_url.
#' @return long_url - the original long URL.
#' @return archived - a true/false value indicating whether the user has archived this link.
#' @return private - a true/false value indicating whether the user has made this link private.
#' @return created_at - an integer unix epoch indicating when this link was shortened/encoded.
#' @return user_ts - a user-provided timestamp for when this link was shortened/encoded, 
#' used for backfilling data.
#' @return modified_at - an integer unix epoch indicating when this link's metadata was last edited.
#' @return title - the title for this link.
#' @return note - the user-provided note, if set.
#' @return shares - a list of share actions (for the authenticated user only)
#' @return client_id - the oauth client ID of the app that shortened/saved this link on behalf of 
#' the user. If expand_client_id is set to false (only currently supported), this will be a string 
#' corresponding to the client_id of the encoding oauth application.
#' 
#' @examples 
#' \dontrun{ 
#' bitly_token <- bitly_auth(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8", secret = "")
#' lh <- bitly_UserLinkHistory() 
#' }
#' 
#' @export
bitly_UserLinkHistory <- function(limit = 100, private = "off", archived = "both", expand_client_id = "false", 
                             showRequestURL = FALSE) {
  
  user_linkHistory_url <- "https://api-ssl.bitly.com/v3/user/link_history"
  
  query <- list(access_token = bitly_token$credentials$access_token, limit = limit, private = private, 
                archived = archived, expand_client_id = expand_client_id)
  
  df_history <- doRequest("GET", user_linkHistory_url, query, service = "bitly", showURL = showRequestURL)
  df_history_data <- df_history$data$link_history
  
  df_history_data$user_ts <- as.POSIXct(df_history_data$user_ts, origin = "1970-01-01", tz = "UTC")
  df_history_data$created_at <- as.POSIXct(df_history_data$created_at, origin = "1970-01-01", tz = "UTC")
  df_history_data$modified_at <- as.POSIXct(df_history_data$modified_at, origin = "1970-01-01", tz = "UTC")
  df_history_data$tags <- NULL
  
  # sapply(df_history_data, class)
  return(df_history_data)
}

#' @title Returns a list of tracking domains a user has configured.
#' 
#' @param showRequestURL - show URL which has been build and requested from server. 
#' For debug purposes.
#'
#' @description See \url{http://dev.bitly.com/user_info.html#v3_user_tracking_domain_list}
#' 
#' @return tracking_domains - a list of tracking domains configured for the authenticated user.
#'
#' @examples 
#' \dontrun{ 
#' bitly_token <- bitly_auth(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8", secret = "")
#' bitly_UserTrackingDomains()
#' }
#' 
#' @export
bitly_UserTrackingDomains <- function(showRequestURL = FALSE) {
  
  user_tracking_domain_list_url <- "https://api-ssl.bitly.com/v3/user/tracking_domain_list"
  
  query <- list(access_token = bitly_token$credentials$access_token, showURL = showRequestURL)
  
  df_tracking_domain_list <- doRequest("GET", user_tracking_domain_list_url, query, service = "bitly")
  df_tracking_domain_list_data <- df_tracking_domain_list$data$tracking_domains
  
  if (!length(df_tracking_domain_list_data) == 0) {
    
    # rather guessing at the moment
    df_tracking_domain_list_data <- data.frame(t(sapply(df_tracking_domain_list_data, c)))
    
    # sapply(df_tracking_domain_list_data, class)
    
    return(df_tracking_domain_list_data)
    
  } else  {
    message("It seems that you don't have any tracking domains.")
  }
  
}
#' @title Validate given domain for the PRO features
#' 
#' @seealso See \url{http://dev.bitly.com/domains.html#v3_bitly_pro_domain}
#' 
#' @description Query whether a given domain is a valid bitly pro domain. Keep in mind that bitly 
#' custom short domains are restricted to less than 15 characters in length.
#'
#' @param domain - A short domain. ie: nyti.ms.
#' @param showRequestURL - show URL which has been build and requested from server. 
#' For debug purposes.
#'
#' @return bitly_pro_domain - 0 or 1 designating whether this is a current bitly domain.
#' @return domain - an echo back of the request parameter.
#' 
#' @examples
#' \dontrun{ 
#' bitly_token <- bitly_auth(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8", secret = "")
#' bitly_IsProDomain(domain = "nytidsfds.ms") 
#' bitly_IsProDomain(domain = "nyti.ms", showRequestURL = TRUE) 
#' }
#' 
#' @export
bitly_IsProDomain <- function(domain, showRequestURL = FALSE) {
  
  bitly_pro_domain_url <- "https://api-ssl.bitly.com/v3/bitly_pro_domain"
  
  query <- list(access_token = bitly_token$credentials$access_token, domain = domain)
  
  # call method from ApiKey.R
  df_bitly_pro_domain <- doRequest(verb = "GET", bitly_pro_domain_url, service = "bitly", query, showURL = showRequestURL)
  
  if (df_bitly_pro_domain$data$bitly_pro_domain == FALSE) {
    message("A short domain: ", df_bitly_pro_domain$data$domain, " is NOT a valid bitly pro domain")
  } else {
    message("A short domain: ", df_bitly_pro_domain$data$domain, " is a valid bitly pro domain")
  }
}
