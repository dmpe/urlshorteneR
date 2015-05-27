#' Return or update information about a user.
#'
#' See http://dev.bitly.com/user_info.html#v3_user_info
#'
#' @return login - the specified bitly login or the login of the authenticated user.
#' @return profile_url - URL of user's profile page.
#' @return profile_image - URL of user's profile image.
#' @return member_since - Unix timestamp for the moment the user signed up.
#' @return full_name - (optional) the user's full name, if set.
#' @return display_name - (optional) the user's display name, if set.
#' @return share_accounts - (optional) a list of the share accounts (Twitter or Facebook) linked to the user's account.
#'
#' @note Only included in requests for a user's own info
#'
#' @return apiKey - the user's bitly API key.
#' @return is_enterprise - 0 or 1 to indicate if this account is signed up for Bitly Brand Tools.
#' @return has_master - 0 or 1 to indicate if this account is a customer sub account.
#' @return custom_short_domain - A short domain registered with this account that can be used in place of bit.ly for shortening links.
#' @return tracking_domains - A list of domains configured for analytics tracking.
#' @return default_link_privacy - public or private indicating the default privacy setting for new links.
#' @return domain_preference_options - A list of the valid short domains that this account can choose as a default.
#'
#' @note Only included for enterprise accounts (is_enterprise == 1 or has_master == 1)
#'
#' @return sub_accounts - (optional) list of accounts associated with this account.
#' @return e2e_domains - (optional) list of domains associated with this custom_short_domain.
#' @return tracking_url_prefixes - A list of owned 3rd party urls such as Facebook tracked for analytics
#' @return master_account - (optional) the login of a master account, if this is associated with an enterprise account.
#' @return enterprise_permissions - (optional) list of enterprise permissions associated with this account.
#' @return bbt_start_date - (optional) the date for when this account became a Bitly Brand Tools account.
#' 
#' @export
user.info <- function() {
  user.info.url <- "https://api-ssl.bitly.com/v3/user/info"
  createdUrl <- paste(user.info.url, "?format=json", sep = "")
  
  # dont use httr package, as it will fail into " Error in parse_string(txt, bigint_as_char) : lexical error: invalid char in json text."
  # parsedjson <- fromJSON(createdUrl)
  df.user.info.data <- doRequest(createdUrl)
  
  df.user.info.data <- data.frame(t(sapply(df.user.info.data$data,c)))
  df.user.info.data$member_since <- as.POSIXct(df.user.info.data$member_since, origin = "1970-01-01", tz = "UTC")
  
  return(df.user.info.data)
}

#' Returns entries from a user's link history in reverse chronological order.
#' 
#' See http://dev.bitly.com/user_info.html#v3_user_link_history
#' 
#' @param limit - optional integer in the range 1 to 100; default: 100, specifying the max number of results to return.
#' @export
user.linkHistory <- function(limit = 100){
  user.linkHistory.url <- "https://api-ssl.bitly.com/v3/user/link_history"
  
  createdUrl <- paste(user.linkHistory.url, "?limit=", limit, sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  df.all <- doRequest(createdUrl)
  df.all.history <- df.all$data$link_history
  df.all.history$user_ts <- as.POSIXct(df.all.history$user_ts, origin = "1970-01-01", tz = "UTC")
  df.all.history$created_at <- as.POSIXct(df.all.history$created_at, origin = "1970-01-01", tz = "UTC")
  df.all.history$modified_at <- as.POSIXct(df.all.history$modified_at, origin = "1970-01-01", tz = "UTC")
  
  return(df.all.history)
}

#' Returns a list of tracking domains a user has configured.
#' 
#' See http://dev.bitly.com/user_info.html#v3_user_tracking_domain_list
#' 
#' @return tracking_domains - a list of tracking domains configured for the authenticated user.
#' 
#' @export
user.tracking_domain_list <- function() {
  user.tracking_domain_list.url <- "https://api-ssl.bitly.com/v3/user/tracking_domain_list"
  
  createdUrl <- paste(user.tracking_domain_list.url, "?format=json", sep = "")
  
  df.tracking_domain_list <- doRequest(createdUrl)
  df.tracking_domain_list <- df.tracking_domain_list$data$tracking_domains
  
  if(!length(df.tracking_domain_list) == 0) {
    
    df.tracking_domain_list <- data.frame(t(sapply(df.tracking_domain_list,c)))
    return(df.tracking_domain_list)
    
  } else  {
    message("It seems that you don't have any tracking domains.")
  }
  
}