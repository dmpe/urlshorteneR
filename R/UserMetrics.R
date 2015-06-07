#' @title Returns the aggregate number of clicks on all of the authenticated user's Bitlinks.
#' 
#' @seealso See \url{http://dev.bitly.com/user_metrics.html#v3_user_clicks}
#' 
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return all 
#' units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the maximum 
#' value for units is 60.
#' @param rollup - true or false. Return data for multiple units rolled up to a single result instead 
#' of a separate value for each period of time.
#' 
#' @return dt - a unix timestamp representing the beginning of this unit.
#' @return day_start - a unix timestamp representing the beginning of the specified day (ONLY returned 
#' if unit is not specified).
#' @return clicks - the number of clicks on this user's links in the specified timeframe.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' user.metrics.clicks(unit = "day", units = -1, limit = 100, rollup = "true")
#' user.metrics.clicks(unit = "day", units = -1, limit = 100, rollup = "false")
#' 
#' @note without the parameter unit this endpoint returns a legacy response format which assumes 
#' rollup=false, unit=day and units=7.
#' 
#' @import RCurl
#' @export
user.metrics.clicks <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"), 
                                units = -1, rollup = c("false", "true")) {
  unit.matched <- match.arg(unit)
  rollup.matched <- match.arg(rollup)
  
  user.metrics.clicks.url <- "https://api-ssl.bitly.com/v3/user/clicks"
  
  createdUrl <- paste(user.metrics.clicks.url, "?limit=", limit, "&unit=", unit.matched, 
                      "&units=", units, "&rollup=", rollup.matched, sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df.user.metrics.clicks <- doRequest(createdUrl)
  df.user.metrics.clicks.data <- df.user.metrics.clicks$data$user_clicks
  
  if (rollup == "true") {
    
    # no data frame
    return(df.user.metrics.clicks.data)
    
  } else {
    df.user.metrics.clicks.data$dt <- as.POSIXct(as.integer(df.user.metrics.clicks.data$dt), 
                                                 origin = "1970-01-01", tz = "UTC")
    return(df.user.metrics.clicks.data)
  }
}


#' @title Returns aggregate metrics about the countries referring click traffic to all of the authenticated user's Bitlinks.
#' 
#' @seealso See \url{http://dev.bitly.com/user_metrics.html#v3_user_countries}
#'
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return 
#' all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the 
#' maximum value for units is 60.
#' @param rollup - true or false. Return data for multiple units rolled up to a single result 
#' instead of a separate value for each period of time.
#' 
#' @return clicks - the number of clicks referred from this country.
#' @return country - the two-letter code of the referring country.
#' 
#' @note When a unit is specified (always the case), rollup is always (!) true.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' user.metrics.countries(unit = "day", units = -1, limit = 100, rollup = "true")
#' user.metrics.countries(unit = "day", units = -1, limit = 100, rollup = "false")
#' 
#' @import RCurl
#' @export
user.metrics.countries <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"), 
                                   rollup = c("false", "true"), units = -1) {
  unit.matched <- match.arg(unit)
  rollup.matched <- match.arg(rollup)
  
  user.metrics.countries.url <- "https://api-ssl.bitly.com/v3/user/countries"
  
  createdUrl <- paste(user.metrics.countries.url, "?limit=", limit, "&unit=", unit.matched, 
                      "&units=", units, "&rollup=", rollup.matched, sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df.user.metrics.countries <- doRequest(createdUrl)
  
  df.user.metrics.countries.data <- df.user.metrics.countries$data$user_countries
  
  # More testing required
  if (rollup == "true") {
    return(df.user.metrics.countries.data)
    
  } else {
    # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
    df.user.metrics.countries.data <- data.frame(sapply(df.user.metrics.countries.data,c))
  }
  
  return(df.user.metrics.countries.data)
  
  
}

#' @title Returns the authenticated user's most-clicked Bitlinks (ordered by number of clicks) in a given time period.
#' 
#' @seealso See \url{http://dev.bitly.com/user_metrics.html#v3_user_popular_links}
#'
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the 
#' maximum value for units is 60.
#' 
#' @return link - a Bitlink.
#' @return clicks - the number of clicks on that Bitlink in the specified timeframe.
#' 
#' @note This replaces the realtime_links endpoint.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' user.metrics.popular_links(unit = "day", units = -1, limit = 100)
#' 
#' @import RCurl
#' @export
user.metrics.popular_links <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"),
                                       units = -1) {
  unit.matched <- match.arg(unit)
  
  user.metrics.popular_links.url <- "https://api-ssl.bitly.com/v3/user/popular_links"
  
  createdUrl <- paste(user.metrics.popular_links.url, "?limit=", limit, "&unit=", unit.matched, 
                      "&units=", units, sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  # call method from ApiKey.R
  user.metrics.popular_links <- doRequest(createdUrl)
  
  user.metrics.popular_links.data <- user.metrics.popular_links$data$popular_links
  
  return(user.metrics.popular_links.data)
  
}


#' @title Returns aggregate metrics about the pages referring click traffic to all of the authenticated user's Bitlinks.
#' 
#' @seealso See \url{http://dev.bitly.com/user_metrics.html#v3_user_referrers}
#'
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return 
#' all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the 
#' maximum value for units is 60.
#' @param rollup - true or false. Return data for multiple units rolled up to a single result 
#' instead of a separate value for each period of time.
#' 
#' @return clicks - the number of clicks referred from this URL.
#' @return referrer - the URL referring clicks.
#' 
#' @note When a unit is specified (always the case), rollup is always (!) true.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' user.metrics.referrers(unit = "day", units = -1, limit = 100, rollup = "true")
#' user.metrics.referrers(unit = "day", units = -1, limit = 100, rollup = "false")
#' 
#' @import RCurl
#' @export
user.metrics.referrers <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"), 
                                   rollup = c("false", "true"), units = -1) {
  unit.matched <- match.arg(unit)
  rollup.matched <- match.arg(rollup)
  
  user.metrics.referrers.url <- "https://api-ssl.bitly.com/v3/user/referrers"
  
  createdUrl <- paste(user.metrics.referrers.url, "?limit=", limit, "&unit=", unit.matched, 
                      "&units=", units, "&rollup=", rollup.matched, sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  # call method from ApiKey.R
  user.metrics.referrers.url <- doRequest(createdUrl)
  
  user.metrics.referrers.data <- user.metrics.referrers.url$data$user_referrers
  
  if (rollup == "true") {
    user.metrics.referrers.data <- data.frame(t(sapply(user.metrics.referrers.data,c)))
    
  } else {
    # No way I can check at the moment 
    
    # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
    user.metrics.referrers.data <- data.frame(t(sapply(user.metrics.referrers.data,c)))
  }
  
  return(user.metrics.referrers.data)
  
}

#' @title Returns aggregate metrics about the domains referring click traffic to all of the authenticated user's Bitlinks. 
#' 
#' @description If the user is a master (ent.) account, or is a subaccount with full_reports permission, the 
#' user may choose to view the metrics of any account belonging to the master account.
#' 
#' @seealso See \url{http://dev.bitly.com/user_metrics.html#v3_user_referring_domains}
#'
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return 
#' all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the 
#' maximum value for units is 60.
#' @param rollup - true or false. Return data for multiple units rolled up to a single result 
#' instead of a separate value for each period of time.
#' @param exclude_social_networks - true (default) or false. If true, exclude domains that are 
#' part of a social network that bitly tracks.
#' @param login - an optional string consisting of the account name used to report the appropriate
#' statistics; defaults to the current user.
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
#' user.metrics.referring_domains(unit = "day", units = -1, limit = 100, 
#' exclude_social_networks = "false")
#' user.metrics.referring_domains(unit = "day", units = -1, limit = 100, 
#' exclude_social_networks = "true")
#' 
#' @import RCurl
#' @export
user.metrics.referring_domains <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"),
                                           rollup = c("false", "true"), units = -1, 
                                           exclude_social_networks = c("true", "false"), login=NULL) {
  
  unit.matched <- match.arg(unit)
  rollup.matched <- match.arg(rollup)
  exclude_social_networks.matched <- match.arg(exclude_social_networks)
  
  user.metrics.referring_domains.url <- "https://api-ssl.bitly.com/v3/user/referring_domains"
  
  createdUrl <- paste(user.metrics.referring_domains.url, "?limit=", limit, "&unit=", unit.matched, 
                      "&units=", units, "&rollup=", rollup.matched, sep = "")
  createdUrl <- paste(createdUrl, "&exclude_social_networks=", exclude_social_networks.matched, 
                      "&login=", login, "&format=json", sep = "")
  
  # call method from ApiKey.R
  user.metrics.referring_domains <- doRequest(createdUrl)
  
  user.metrics.referring_domains.data <- user.metrics.referring_domains$data$user_referring_domains
  
  if (length(user.metrics.referring_domains.data) == 0) {
    user.metrics.referring_domains.data <- NULL
    message("You have zero referring domains given your function input.")
  }
  
  return(user.metrics.referring_domains.data)
  
}


#' @title Returns the number of Bitlinks created in a given time period by the authenticated user.
#' 
#' @seealso See \url{http://dev.bitly.com/user_metrics.html#v3_user_shorten_counts}
#'
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return all 
#' units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the 
#' maximum value for units is 60.
#' value for each period of time.
#' @param rollup - true or false (default). Return data for multiple units rolled up to a single 
#' result instead of a separate value for each period of time.
#' 
#' @return tz_offset - the offset for the specified timezone, in hours.
#' @return unit - an echo of the specified unit value.
#' @return units - an echo of the specified units value.
#' @return user_shorten_counts - the number of shortens made by the specified user in the specified time.
#' 
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' user.metrics.shorten_counts(unit = "day", units = -1, limit = 100, rollup = "true")
#' user.metrics.shorten_counts(unit = "day", units = -1, limit = 100, rollup = "false")
#' user.metrics.shorten_counts(unit = "day", units = -1, limit = 100)
#' 
#' @import RCurl
#' @export
user.metrics.shorten_counts <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"),
                                        rollup = c("false", "true"), units = -1) {
  
  unit.matched <- match.arg(unit)
  rollup.matched <- match.arg(rollup)
  
  user.metrics.shorten_counts.url <- "https://api-ssl.bitly.com/v3/user/shorten_counts"
  
  createdUrl <- paste(user.metrics.shorten_counts.url, "?limit=", limit, "&unit=", unit.matched, 
                      "&units=", units, "&rollup=", rollup.matched, sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  # call method from ApiKey.R
  user.metrics.shorten_counts <- doRequest(createdUrl)
  
  if (rollup.matched == "false") {
    user.metrics.shorten_counts.data <- user.metrics.shorten_counts$data$user_shorten_counts
    user.metrics.shorten_counts.data$dt <- as.POSIXct(as.integer(user.metrics.shorten_counts.data$dt), 
                                                      origin = "1970-01-01", tz = "UTC")
    
  } else {
    user.metrics.shorten_counts.data <- data.frame(t(sapply(user.metrics.shorten_counts$data,c)))
  }
  
  return(user.metrics.shorten_counts.data)
  
}




