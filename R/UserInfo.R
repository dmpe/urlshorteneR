#' Return or update information about a user.
#'
#' See http://dev.bitly.com/user_info.html#v3_user_info
#' 
#' @export
user.info <- function() {
  user.info.url <- "https://api-ssl.bitly.com/v3/user/info"
  createdUrl <- paste(user.info.url, "?format=json", sep = "")
  
  doRequest(createdUrl)
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
  df.tracking_domain_list <- data.frame(t(sapply(df.tracking_domain_list$data$tracking_domains,c)))

  return(df.tracking_domain_list)
  
}