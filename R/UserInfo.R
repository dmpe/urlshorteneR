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
#' user.info() 
#' 
#' @import stringr
#' 
#' @export
user.info <- function() {
  user.info.url <- "https://api-ssl.bitly.com/v3/user/info"
  
  createdUrl <- paste(user.info.url, "?format=json", sep = "")
  
  # dont use httr package, as it will fail into "Error in parse_string(txt, bigint_as_char) : 
  # lexical error: invalid char in json text."
  df.user.info.data <- doRequest(createdUrl)
  
  df.user.info.data <- data.frame(ReturnValues = unlist(df.user.info.data$data))
  df.user.info.data$ReturnValues <- str_trim(as.character(df.user.info.data$ReturnValues))
  df.user.info.data$ReturnValuesDescription <- rownames(df.user.info.data)
  rownames(df.user.info.data) <- NULL
  
  # convert to readable format 
  df.user.info.data[5,1] <- as.character(as.POSIXct(as.integer(df.user.info.data[5, 1]), 
                                                    origin = "1970-01-01", tz = "UTC"))
  
  return(df.user.info.data)
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
#' user.linkHistory() 
#'
#' @export
user.linkHistory <- function(limit = 100, private = "off", archived = "both", expand_client_id = "false") {
  
  user.linkHistory.url <- "https://api-ssl.bitly.com/v3/user/link_history"
  
  createdUrl <- paste(user.linkHistory.url, "?limit=", limit, "&private=", private, "&archived=", archived, sep = "")
  createdUrl <- paste(createdUrl, "&expand_client_id=", expand_client_id, "&format=json", sep = "")
  
  df.all <- doRequest(createdUrl)
  df.all.history <- df.all$data$link_history
  
  df.all.history$user_ts <- as.POSIXct(df.all.history$user_ts, origin = "1970-01-01", tz = "UTC")
  df.all.history$created_at <- as.POSIXct(df.all.history$created_at, origin = "1970-01-01", tz = "UTC")
  df.all.history$modified_at <- as.POSIXct(df.all.history$modified_at, origin = "1970-01-01", tz = "UTC")
  df.all.history$tags <- NULL
  
  return(df.all.history)
}

#' @title Returns a list of tracking domains a user has configured.
#' 
#' @seealso See \url{http://dev.bitly.com/user_info.html#v3_user_tracking_domain_list}
#' 
#' @return tracking_domains - a list of tracking domains configured for the authenticated user.
#'
#' @examples 
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' user.tracking_domain_list()
#' 
#' @export
user.tracking_domain_list <- function() {
  user.tracking_domain_list.url <- "https://api-ssl.bitly.com/v3/user/tracking_domain_list"
  
  createdUrl <- paste(user.tracking_domain_list.url, "?format=json", sep = "")
  
  df.tracking_domain_list <- doRequest(createdUrl)
  df.tracking_domain_list <- df.tracking_domain_list$data$tracking_domains
  
  if (!length(df.tracking_domain_list) == 0) {
    
    # create and return a data frame from a transposed list
    df.tracking_domain_list <- data.frame(t(sapply(df.tracking_domain_list,c)))
    return(df.tracking_domain_list)
    
  } else  {
    message("It seems that you don't have any tracking domains.")
  }
  
}