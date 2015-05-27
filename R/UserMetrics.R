#' Returns the aggregate number of clicks on all of the authenticated user's Bitlinks.
#' 
#' @seealso See \url{http://dev.bitly.com/user_metrics.html#v3_user_clicks}
#' 
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the maximum value for units is 60.
#' value for each period of time.
#' 
#' @return dt: a unix timestamp representing the beginning of this unit.
#' @return day_start: a unix timestamp representing the beginning of the specified day (ONLY returned if unit is not specified).
#' @return clicks: the number of clicks on this user's links in the specified timeframe.
#' 
#' @examples
#' user.metrics.clicks(unit = "day", units = -1, limit = 100, rollup = "true")
#' user.metrics.clicks(unit = "day", units = -1, limit = 100, rollup = "false")
#' user.metrics.clicks(units = -1, limit = 100, rollup = "false")
#' 
#' @note without the parameter unit this endpoint returns a legacy response format which assumes rollup=false, unit=day and units=7.
#' 
#' @import RCurl
#' @export
user.metrics.clicks <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"), units = -1, rollup = "true") {
  unit.matched <- match.arg(unit)
  
  user.metrics.clicks.url <- "https://api-ssl.bitly.com/v3/user/clicks"
  
  createdUrl <- paste(user.metrics.clicks.url, "?limit=", limit, "&unit=", unit.matched, "&units=", units, "&rollup=", rollup, sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df.user.metrics.clicks <- doRequest(createdUrl)
  df.user.metrics.clicks.data <- df.user.metrics.clicks$data$user_clicks
  
  if(rollup == "true") {
    
    # no data frame
    return(df.user.metrics.clicks.data)
    
  } else {
    
    df.user.metrics.clicks.data$dt <- as.POSIXct(as.integer(df.user.metrics.clicks.data$dt), origin = "1970-01-01", tz = "UTC")
    
    if(is.null(unit)) {
      df.user.metrics.clicks.data$day_start <- as.POSIXct(as.integer(df.user.metrics.clicks.data$day_start), origin = "1970-01-01", tz = "UTC")
    }
    
    df.user.metrics.clicks.data <- data.frame(sapply(df.user.metrics.clicks.data,c))
    return(df.user.metrics.clicks.data)
  }
#   # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
#   df.user.metrics.clicks.data <- data.frame(sapply(df.user.metrics.clicks.data,c))
#   return(df.user.metrics.clicks.data)
}



#' Returns aggregate metrics about the countries referring click traffic to all of the authenticated user's Bitlinks.
#' 
#' @seealso See \url{http://dev.bitly.com/user_metrics.html#v3_user_countries}
#'
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the maximum value for units is 60.
#' value for each period of time.
#' 
#' @return clicks - the number of clicks referred from this country.
#' @return country - the two-letter code of the referring country.
#' 
#' @note without the parameter unit this endpoint returns a legacy response format which assumes rollup=false, unit=day and units=7. When a unit is specified, rollup is always true.
#' 
#' @examples
#' user.metrics.countries(unit = "day", units = -1, limit = 100)
#' 
#' @import RCurl
#' @export
user.metrics.countries <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"), units = -1) {
  unit.matched <- match.arg(unit)
  
  user.metrics.countries.url <- "https://api-ssl.bitly.com/v3/user/countries"
  
  createdUrl <- paste(user.metrics.countries.url, "?limit=", limit, "&unit=", unit.matched, "&units=", units, sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df.user.metrics.countries <- doRequest(createdUrl)
  
  df.user.metrics.countries.data <- df.user.metrics.countries$data$user_countries
  
  # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
  df.user.metrics.countries.data <- data.frame(sapply(df.user.metrics.countries.data,c))
  return(df.user.metrics.countries.data)
}
