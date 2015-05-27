#' Returns the number of clicks on a single Bitlink of the authenticated user.
#' 
#' See http://dev.bitly.com/link_metrics.html#v3_link_clicks
#' 
#' @param link - a Bitlink.
#' @param limit - 1 to 1000 (default=1000).
#' ## @param rollup - true (default) or false.  Return data for multiple units rolled up to a single result instead of a separate value for each period of time.
#' @param units - an integer representing the time units to query data for. Pass -1 to return all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the maximum value for units is 60.
#' value for each period of time.
#' 
#' @examples
#' link.metrics.clicks(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#' 
#' @import RCurl
#' @export
link.metrics.clicks <- function(link, limit = 1000, unit = c("minute", "hour", "day", "week", "month"), units = -1) {
  unit.matched <- match.arg(unit)
  
  link.metrics.clicks.url <- "https://api-ssl.bitly.com/v3/link/clicks"
  
  createdUrl <- paste(link.metrics.clicks.url, "?link=", curlEscape(link), "&limit=", limit, "&unit=", unit.matched, "&units=", units, sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df.link.metrics.clicks <- doRequest(createdUrl)
  
  df.link.metrics.clicks.data <- df.link.metrics.clicks$data
  
  if(!is.null(df.link.metrics.clicks.data$unit_reference_ts)) {
    df.link.metrics.clicks.data$unit_reference_ts <- as.POSIXct(df.link.metrics.clicks.data$unit_reference_ts, origin = "1970-01-01", tz = "UTC")
  }
  
  # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
  df.link.metrics.clicks.data <- data.frame(t(sapply(df.link.metrics.clicks.data,c)))
  return(df.link.metrics.clicks.data)
}


#' Returns metrics about the countries referring click traffic to a single Bitlink.
#' 
#' See http://dev.bitly.com/link_metrics.html#v3_link_countries
#' 
#' @param link - a Bitlink.
#' @param limit - 1 to 1000 (default=1000).
#' ## @param rollup - true (default) or false.  Return data for multiple units rolled up to a single result instead of a separate value for each period of time.
#' @param units - an integer representing the time units to query data for. Pass -1 to return all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the maximum value for units is 60.
#' value for each period of time.
#' @examples
#' link.metrics.countries(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#' 
#' @import RCurl
#' @export
link.metrics.countries <- function(link, limit = 1000, unit = c("minute", "hour", "day", "week", "month"), units = -1) {
  unit.matched <- match.arg(unit)
  
  link.metrics.countries.url <- "https://api-ssl.bitly.com/v3/link/countries"
  
  createdUrl <- paste(link.metrics.countries.url, "?link=", curlEscape(link), "&limit=", limit, "&unit=", unit.matched, "&units=", units, sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df.link.metrics.countries <- doRequest(createdUrl)
  
  df.link.metrics.countries.data <- df.link.metrics.countries$data$countries
  
  if(!is.null(df.link.metrics.countries.data$unit_reference_ts)) {
    df.link.metrics.countries.data$unit_reference_ts <- as.POSIXct(df.link.metrics.countries.data$unit_reference_ts, origin = "1970-01-01", tz = "UTC")
  }
  
  # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
  df.link.metrics.countries.data <- data.frame(sapply(df.link.metrics.countries.data,c))
  return(df.link.metrics.countries.data)
}


#' Returns the number of users who have shortened (encoded) a single Bitlink.
#' 
#' See http://dev.bitly.com/link_metrics.html#v3_link_encoders_count
#' 
#' @param link - a Bitlink.
#' @return aggregate_link - the aggregate (global) Bitlink for the provided Bitlink.
#' @return count - the number of bitly users who have shortened (encoded) this link.
#' @examples
#' link.metrics.encoders_count(link = "http://bit.ly/DPetrov")
#' 
#' @import RCurl
#' @export
link.metrics.encoders_count <- function(link) {
  link.metrics.encoders_count.url <- "https://api-ssl.bitly.com/v3/link/encoders_count"
  
  createdUrl <- paste(link.metrics.encoders_count.url, "?link=", curlEscape(link), sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df.link.metrics.encoders_count <- doRequest(createdUrl)
  
  df.link.metrics.encoders_count.data <- df.link.metrics.encoders_count$data
  
  # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
  df.link.metrics.encoders_count.data <- data.frame(t(sapply(df.link.metrics.encoders_count.data,c)))
  return(df.link.metrics.encoders_count.data)
}

#' Returns metrics about the domains referring click traffic to a single Bitlink.
#' 
#' See http://dev.bitly.com/link_metrics.html#v3_link_referring_domains
#' 
#' @param link - a Bitlink.
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the maximum value for units is 60.
#' value for each period of time.
#' @return clicks - the number of clicks referred from this domain.
#' @return domain - the domain referring clicks.
#' @return url - the complete URL of the domain referring clicks.

#' @examples
#' link.metrics.referring_domains(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#' 
#' @import RCurl
#' @export
link.metrics.referring_domains <- function(link, limit = 1000, unit = c("minute", "hour", "day", "week", "month"), units = -1) {
  unit.matched <- match.arg(unit)
  
  link.metrics.referring_domains.url <- "https://api-ssl.bitly.com/v3/link/referring_domains"
  
  createdUrl <- paste(link.metrics.referring_domains.url, "?link=", curlEscape(link), "&limit=", limit, "&unit=", unit.matched, "&units=", units, sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df.link.metrics.referring_domains <- doRequest(createdUrl)
  
  df.link.metrics.referring_domains.data <- df.link.metrics.referring_domains$data$referring_domains
  
  # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
  df.link.metrics.referring_domains.data <- data.frame(t(sapply(df.link.metrics.referring_domains.data,c)))
  return(df.link.metrics.referring_domains.data)
}





