#' @title Return or update information about a user.
#'
#' @seealso See \url{http://dev.bitly.com/user_info.html#v3_user_info}
#'
#' @return login - the specified bitly login or the login of the authenticated user.
#' @return profile_url - URL of user's profile page.
#' @return profile_image - URL of user's profile image.
#' @return member_since - Unix timestamp for the moment the user signed up.
#' @return full_name - (optional) the user's full name, if set.
#' @return display_name - (optional) the user's display name, if set.
#' @return share_accounts - (optional) a list of the share accounts (Twitter or Facebook) linked to 
#' the user's account.

#' @note Only included in requests for a user's own info
#'
#' @return apiKey - the user's bitly API key.
#' @return is_enterprise - 0 or 1 to indicate if this account is signed up for Bitly Brand Tools.
#' @return has_master - 0 or 1 to indicate if this account is a customer sub account.
#' @return custom_short_domain - A short domain registered with this account that can be used in 
#' place of bit.ly for shortening links.
#' @return tracking_domains - A list of domains configured for analytics tracking.
#' @return default_link_privacy - public or private indicating the default privacy setting for 
#' new links.
#' @return domain_preference_options - A list of the valid short domains that this account can 
#' choose as a default.
#'
#' @note Only included for enterprise accounts (is_enterprise == 1 or has_master == 1)
#' @note both columns (!) are character type
#' 
#' @return sub_accounts - (optional) list of accounts associated with this account.
#' @return e2e_domains - (optional) list of domains associated with this custom_short_domain.
#' @return tracking_url_prefixes - A list of owned 3rd party urls such as Facebook tracked for 
#' analytics
#' @return master_account - (optional) the login of a master account, if this is associated with
#' an enterprise account.
#' @return enterprise_permissions - (optional) list of enterprise permissions associated with this 
#' account.
#' @return bbt_start_date - (optional) the date for when this account became a Bitly Brand Tools 
#' account.
#' 
#' @examples 
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' user_Info() 
#' 
#' @import stringr
#' 
#' @export
user_Info <- function() {
  
  user_info_url <- "https://api-ssl.bitly.com/v3/user/info"
  
  query <- list(access_token = rbitlyApi())

  df_user_info <- doRequest(user_info_url, query)

  df_user_info_data <- data.frame(ReturnValues = unlist(df_user_info$data))
  df_user_info_data$ReturnValues <- str_trim(as.character(df_user_info_data$ReturnValues))
  df_user_info_data$ReturnValuesDescription <- rownames(df_user_info_data)
  rownames(df_user_info_data) <- NULL
  
  # convert to readable format 
  df_user_info_data[5, 1] <- as.character(as.POSIXct(as.integer(df_user_info_data[5, 1]), 
                                                    origin = "1970-01-01", tz = "UTC"))
  
  return(df_user_info_data)
}

#' @title Returns entries from a user's link history in reverse chronological order.
#' 
#' @seealso See \url{http://dev.bitly.com/user_info.html#v3_user_link_history}
#' 
#' @param limit - optional integer in the range 1 to 100; default: 100, specifying the max 
#' number of results to return.
#' 
#' @param expand_client_id - true or false (always default) whether to provide additional 
#' information about encoding application. 
#' @param archived - on, off (default) or both whether to include or exclude archived history 
#' entries. (on = return only archived history entries)
#' @param private - on, off and both (default) whether to include or exclude private history 
#' entries. (on = return only private history entries)
#' 
#' @return link - the Bitlink specific to this user and this long_url.
#' @return aggregate_link - the global bitly identifier for this long_url.
#' @return long_url - the original long URL.
#' @return archived - a true/false value indicating whether the user has archived this link.
#' @return private - a true/false value indicating whether the user has made this link private.
#' @return created_at - an integer unix epoch indicating when this link was shortened/encoded.
#' @return user_ts - a user-provided timestamp for when this link was shortened/encoded, used for backfilling data.
#' @return modified_at - an integer unix epoch indicating when this link's metadata was last edited.
#' @return title - the title for this link.
#' @return note - the user-provided note, if set.
#' @return shares - a list of share actions (for the authenticated user only)
#' @return client_id - the oauth client ID of the app that shortened/saved this link on behalf of 
#' the user. If expand_client_id is set to false (only currently supported), this will be a string 
#' corresponding to the client_id of the encoding oauth application.
#' 
#' @examples 
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' user_LinkHistory() 
#'
#' @export
user_LinkHistory <- function(limit = 100, private = "off", archived = "both", expand_client_id = "false") {
  
  user_linkHistory_url <- "https://api-ssl.bitly.com/v3/user/link_history"
  
  created_URL <- paste(user_linkHistory_url, "?limit=", limit, "&private=", private, "&archived=", archived, sep = "")
  created_URL <- paste(created_URL, "&expand_client_id=", expand_client_id, "&format=json", sep = "")
  
  df_history <- doRequest(created_URL)
  df_history_data <- df_history$data$link_history
  
  df_history_data$user_ts <- as.POSIXct(df_history_data$user_ts, origin = "1970-01-01", tz = "UTC")
  df_history_data$created_at <- as.POSIXct(df_history_data$created_at, origin = "1970-01-01", tz = "UTC")
  df_history_data$modified_at <- as.POSIXct(df_history_data$modified_at, origin = "1970-01-01", tz = "UTC")
  df_history_data$tags <- NULL
  
  return(df_history_data)
}

#' @title Returns a list of tracking domains a user has configured.
#' 
#' @seealso See \url{http://dev.bitly.com/user_info.html#v3_user_tracking_domain_list}
#' 
#' @return tracking_domains - a list of tracking domains configured for the authenticated user.
#'
#' @examples 
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' user_TrackingDomains()
#' 
#' @export
user_TrackingDomains <- function() {
  
  user_tracking_domain_list_url <- "https://api-ssl.bitly.com/v3/user/tracking_domain_list"
  
  created_URL <- paste(user_tracking_domain_list_url, "?format=json", sep = "")
  
  df_tracking_domain_list <- doRequest(created_URL)
  df_tracking_domain_list_data <- df_tracking_domain_list$data$tracking_domains
  
  if (!length(df_tracking_domain_list_data) == 0) {
    
    # create and return a data frame from a transposed list
    df_tracking_domain_list_data <- data.frame(t(sapply(df_tracking_domain_list_data,c)))
    return(df_tracking_domain_list_data)
    
  } else  {
    message("It seems that you don't have any tracking domains.")
  }
  
}