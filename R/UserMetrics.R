#' Returns the aggregate number of clicks on all of the authenticated user's Bitlinks.
#' 
#' @seealso See \url{http://dev.bitly.com/user_metrics.html#v3_user_clicks}
#' 
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the maximum value for units is 60.
#' value for each period of time.
#' @param rollup - true or false. Return data for multiple units rolled up to a single result instead of a separate value for each period of time.
#' 
#' @return dt: a unix timestamp representing the beginning of this unit.
#' @return day_start: a unix timestamp representing the beginning of the specified day (ONLY returned if unit is not specified).
#' @return clicks: the number of clicks on this user's links in the specified timeframe.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' user.metrics.clicks(unit = "day", units = -1, limit = 100, rollup = "true")
#' user.metrics.clicks(unit = "day", units = -1, limit = 100, rollup = "false")
#' 
#' @note without the parameter unit this endpoint returns a legacy response format which assumes rollup=false, unit=day and units=7.
#' 
#' @import RCurl
#' @export
user.metrics.clicks <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"), units = -1, rollup = c("false", "true")) {
  unit.matched <- match.arg(unit)
  rollup.matched <- match.arg(rollup)
  
  user.metrics.clicks.url <- "https://api-ssl.bitly.com/v3/user/clicks"
  
  createdUrl <- paste(user.metrics.clicks.url, "?limit=", limit, "&unit=", unit.matched, "&units=", units, "&rollup=", rollup.matched, sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df.user.metrics.clicks <- doRequest(createdUrl)
  df.user.metrics.clicks.data <- df.user.metrics.clicks$data$user_clicks
  
  if(rollup == "true") {
    
    # no data frame
    return(df.user.metrics.clicks.data)
    
  } else {
    df.user.metrics.clicks.data$dt <- as.POSIXct(as.integer(df.user.metrics.clicks.data$dt), origin = "1970-01-01", tz = "UTC")
    
    df.user.metrics.clicks.data <- data.frame(df.user.metrics.clicks.data)
    return(df.user.metrics.clicks.data)
  }
}


#' Returns aggregate metrics about the countries referring click traffic to all of the authenticated user's Bitlinks.
#' 
#' @seealso See \url{http://dev.bitly.com/user_metrics.html#v3_user_countries}
#'
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the maximum value for units is 60.
#' value for each period of time.
#' @param rollup - true or false. Return data for multiple units rolled up to a single result instead of a separate value for each period of time.
#' 
#' @return clicks - the number of clicks referred from this country.
#' @return country - the two-letter code of the referring country.
#' 
#' @note When a unit is specified (always the case), rollup is always (!) true.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' user.metrics.countries(unit = "day", units = -1, limit = 100, rollup = "true")
#' 
#' @import RCurl
#' @export
user.metrics.countries <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"), rollup = c("false", "true"), units = -1) {
  unit.matched <- match.arg(unit)
  rollup.matched <- match.arg(rollup)
  
  user.metrics.countries.url <- "https://api-ssl.bitly.com/v3/user/countries"
  
  createdUrl <- paste(user.metrics.countries.url, "?limit=", limit, "&unit=", unit.matched, "&units=", units, "&rollup=", rollup.matched, sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df.user.metrics.countries <- doRequest(createdUrl)
  
  df.user.metrics.countries.data <- df.user.metrics.countries$data$user_countries
  
  if(rollup == "true") {
    # no data frame
    return(df.user.metrics.countries.data)
    
  } else {
    # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
    df.user.metrics.countries.data <- data.frame(sapply(df.user.metrics.countries.data,c))
    return(df.user.metrics.countries.data)
  }
  
}


#' Returns aggregate metrics about the pages referring click traffic to all of the authenticated user's Bitlinks.
#' 
#' @seealso See \url{http://dev.bitly.com/user_metrics.html#v3_user_referrers}
#'
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the maximum value for units is 60.
#' value for each period of time.
#' @param rollup - true or false. Return data for multiple units rolled up to a single result instead of a separate value for each period of time.
#' 
#' @return clicks - the number of clicks referred from this URL.
#' @return referrer - the URL referring clicks.
#' 
#' @note When a unit is specified (always the case), rollup is always (!) true.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' user.metrics.referrers(unit = "day", units = -1, limit = 100, rollup = "true")
#' 
#' @import RCurl
#' @export
user.metrics.referrers <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"), rollup = c("false", "true"), units = -1) {
  unit.matched <- match.arg(unit)
  rollup.matched <- match.arg(rollup)
  
  user.metrics.referrers.url <- "https://api-ssl.bitly.com/v3/user/referrers"
  
  createdUrl <- paste(user.metrics.referrers.url, "?limit=", limit, "&unit=", unit.matched, "&units=", units, "&rollup=", rollup.matched, sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  # call method from ApiKey.R
  user.metrics.referrers.url <- doRequest(createdUrl)
  
  user.metrics.referrers.url.data <- user.metrics.referrers.url$data$user_referrers
  # more testing required
  if(rollup == "true") {
    user.metrics.referrers.url.data <- data.frame(t(sapply(user.metrics.referrers.url.data,c)))
    return(user.metrics.referrers.url.data)
    
  } else {
    # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
    user.metrics.referrers.url.data <- data.frame(t(sapply(user.metrics.referrers.url.data,c)))
    return(user.metrics.referrers.url.data)
  }
  
}

#' Returns aggregate metrics about the domains referring click traffic to all of the authenticated user's Bitlinks. 
#' If the user is a master (ent.) account, or is a subaccount with full_reports permission, the user may choose to view the metrics of any account belonging to the master account.
#' 
#' @seealso See \url{http://dev.bitly.com/user_metrics.html#v3_user_referring_domains}
#'
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the maximum value for units is 60.
#' value for each period of time.
#' @param rollup - true or false. Return data for multiple units rolled up to a single result instead of a separate value for each period of time.
#' @param exclude_social_networks - true (default) or false. If true, exclude domains that are part of a social network that bitly tracks.
#' @param login - an optional string consisting of the account name used to report the appropriate statistics; defaults to the current user.
#' 
#' @return clicks - the number of clicks referred from this URL.
#' @return referrer - the URL referring clicks.
#' 
#' @note When a unit is specified (always the case), rollup is always (!) true.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' user.metrics.referring_domains(unit = "day", units = -1, limit = 100, rollup = "true")
#' user.metrics.referring_domains(unit = "day", units = -1, limit = 100, rollup = "false")
#' user.metrics.referring_domains(unit = "day", units = -1, limit = 100, rollup = "true", exclude_social_networks = "false")
#' user.metrics.referring_domains(unit = "day", units = -1, limit = 100, rollup = "true", exclude_social_networks = "true")
#' 
#' @import RCurl
#' @export
user.metrics.referring_domains <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"), rollup = c("false", "true"), 
                                           units = -1, exclude_social_networks = c("true", "false"), login) {
  
  unit.matched <- match.arg(unit)
  rollup.matched <- match.arg(rollup)
  exclude_social_networks.matched <- match.arg(exclude_social_networks)
  
  user.metrics.referring_domains <- "https://api-ssl.bitly.com/v3/user/referring_domains"
  
  createdUrl <- paste(user.metrics.referring_domains, "?limit=", limit, "&unit=", unit.matched, "&units=", units, "&rollup=", rollup.matched, sep = "")
  createdUrl <- paste(createdUrl, "&exclude_social_networks=", exclude_social_networks.matched, "&format=json", sep = "")
  
  # call method from ApiKey.R
  user.metrics.referring_domains.url <- doRequest(createdUrl)
  
  # if( exclude_social_networks = "false") {
  
  user.metrics.referring_domains.data <- user.metrics.referring_domains.url$data$user_referring_domains
  
  if(length(user.metrics.referring_domains.data) == 0) {
    user.metrics.referring_domains.data <- NULL
    message("You have zero referring domains given your function input.")
  }
  
  return(user.metrics.referring_domains.data)
  
  
}
