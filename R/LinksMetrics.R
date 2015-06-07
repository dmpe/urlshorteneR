#' @title Returns numbers of click on a link.
#' 
#' @description Returns the number of clicks on a single Bitlink of the authenticated user.
#' 
#' @seealso \url{http://dev.bitly.com/link_metrics.html#v3_link_clicks}
#' 
#' @param link - a Bitlink.
#' @param limit - 1 to 1000 (default=1000).
#' @param rollup - true (always default) or false.  Return data for multiple units rolled up to a 
#' single result instead of a separate value for each period of time.
#' @param units - an integer representing the time units to query data for. Pass -1 to return all 
#' units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the 
#' maximum value for units is 60. 
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' link_Metrics_Clicks(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#' 
#' @importFrom curl curl_escape
#' @export
link_Metrics_Clicks <- function(link, limit = 1000, unit = c("minute", "hour", "day", "week", "month"),
                                units = -1, rollup="true") {
  unit_matched <- match.arg(unit)
  
  link_metrics_clicks_url <- "https://api-ssl.bitly.com/v3/link/clicks"
  
  created_URL <- paste(link_metrics_clicks_url, "?link=", curl_escape(link), "&limit=", limit, 
                      "&unit=", unit_matched, "&units=", units, "&rollup=", rollup, sep = "")
  created_URL <- paste(created_URL, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df.link_metrics_clicks <- doRequest(created_URL)
  
  df.link_metrics_clicks_data <- df.link_metrics_clicks$data
  
  if (!is.null(df.link_metrics_clicks_data$unit_reference_ts)) {
    df.link_metrics_clicks_data$unit_reference_ts <- as.POSIXct(df.link_metrics_clicks_data$unit_reference_ts, 
                                                                origin = "1970-01-01", tz = "UTC")
  }
  
  # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
  df.link_metrics_clicks_data <- data.frame(t(sapply(df.link_metrics_clicks_data, c)))
  return(df.link_metrics_clicks_data)
}


#' @title Returns metrics about the countries from a link.
#' 
#' @description Returns metrics about the countries referring click traffic to a single Bitlink.
#' 
#' @seealso \url{http://dev.bitly.com/link_metrics.html#v3_link_countries}
#' 
#' @param link - a Bitlink.
#' @param limit - 1 to 1000 (default = 1000).
#' @param rollup - true (always default) or false.  Return data for multiple units rolled up to a 
#' single result instead of a separate value for each period of time.
#' @param units - an integer representing the time units to query data for. Pass -1 to return 
#' all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the 
#' maximum value for units is 60.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' link_Metrics_Countries(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#' 
#' @importFrom curl curl_escape
#' @export
link_Metrics_Countries <- function(link, limit = 1000, unit = c("minute", "hour", "day", "week", "month"),
                                   units = -1, rollup="true") {
  unit_matched <- match.arg(unit)
  
  link_metrics_countries_url <- "https://api-ssl.bitly.com/v3/link/countries"
  
  created_URL <- paste(link_metrics_countries_url, "?link=", curl_escape(link), "&limit=", limit, "&unit=", 
                      unit_matched, "&units=", units, sep = "")
  created_URL <- paste(created_URL, "&rollup=", rollup, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df_link_metrics_countries <- doRequest(created_URL)
  
  df_link_metrics_countries_data <- df_link_metrics_countries$data$countries
  
  if (!is.null(df_link_metrics_countries_data$unit_reference_ts)) {
    df_link_metrics_countries_data$unit_reference_ts <- as.POSIXct(df_link_metrics_countries_data$unit_reference_ts,
                                                                   origin = "1970-01-01", tz = "UTC")
  }
  
  # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
  df_link_metrics_countries_data <- data.frame(sapply(df_link_metrics_countries_data, c))
  return(df_link_metrics_countries_data)
}

#' @title Returns users who have encoded this long URL.
#' 
#' @description Returns users who have encoded this long URL (optionally only those in the requesting user's social graph).
#' 
#' @seealso \url{http://dev.bitly.com/link_metrics.html#v3_link_encoders}
#' 
#' @note Some users may not be returned from this call depending on Bitlink privacy settings.
#' 
#' @param link - a Bitlink.
#' @param my_network (optional) true or false (default) - restrict to my network.
#' @param subaccounts (optional, only available to enterprise accounts) false (always default) - 
#' restrict to this enterprise account and its subaccounts
#' @param limit - (optional) integer in the range of 1 to 25 that specifies the number of records 
#' to return (default: 25).
#' @param expand_user (optional) true or false (default) - include display names of encoders.
#' 
#' @return entries - a mapping of link, user, and ts (when the Bitlink was created).
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' link_Metrics_Encoders(link = "http://bit.ly/DPetrov")
#' 
#' @importFrom curl curl_escape
#' @export
link_Metrics_Encoders <- function(link, my_network = "false", limit = 25, expand_user = "false", 
                                  subaccounts = "false") {
  link_metrics_encoders_url <- "https://api-ssl.bitly.com/v3/link/encoders"
  
  created_URL <- paste(link_metrics_encoders_url, "?link=", curl_escape(link), "&my_network=", my_network,
                      "&expand_user=", expand_user, "&subaccounts=", subaccounts, sep = "")
  created_URL <- paste(created_URL, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df_link_metrics_encoders <- doRequest(created_URL)
  
  df_link_metrics_encoders_data <- df_link_metrics_encoders$data$entries
  
  df_link_metrics_encoders_data$ts <- as.POSIXct(as.integer(df_link_metrics_encoders_data$ts),
                                                 origin = "1970-01-01", tz = "UTC")
  
  return(df_link_metrics_encoders_data)
}


#' @title Returns the number of users who have shortened a link.
#' 
#' @description Returns the number of users who have shortened (encoded) a single Bitlink.
#' 
#' @seealso \url{http://dev.bitly.com/link_metrics.html#v3_link_encoders_count}
#' 
#' @param link - a Bitlink.
#' 
#' @return aggregate_link - the aggregate (global) Bitlink for the provided Bitlink.
#' @return count - the number of bitly users who have shortened (encoded) this link.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' link_Metrics_EncodersCount(link = "http://bit.ly/DPetrov")
#' 
#' @importFrom curl curl_escape
#' @export
link_Metrics_EncodersCount <- function(link) {
  link_metrics_encoders_count_url <- "https://api-ssl.bitly.com/v3/link/encoders_count"
  
  created_URL <- paste(link_metrics_encoders_count_url, "?link=", curl_escape(link), sep = "")
  created_URL <- paste(created_URL, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df_link_metrics_encoders_count <- doRequest(created_URL)
  df_link_metrics_encoders_count_data <- df_link_metrics_encoders_count$data
  
  # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
  df_link_metrics_encoders_count_data <- data.frame(t(sapply(df_link_metrics_encoders_count_data, c)))
  return(df_link_metrics_encoders_count_data)
}

#' @title Returns users who have encoded this link.
#' 
#' @description Returns users who have encoded this link (optionally only those in the requesting user's social graph), 
#' sorted by the number of clicks on each encoding user's link.
#' 
#' @seealso See \url{http://dev.bitly.com/link_metrics.html#v3_link_encoders_by_count}
#' 
#' @note - The response will only contain users whose links have gotten at least one click, and 
#' will not contain any users whose links are private.
#' 
#' @param link - a Bitlink.
#' @param my_network true or false (default) - restrict to my network
#' @param subaccounts (only available to enterprise accounts) false (always default) - restrict to 
#' this enterprise account and its subaccounts
#' @param limit - integer in the range 1:100 that specifies the number of records to return (default:100)
#' @param expand_user false (always default) - include display names of encoders
#'
#' @return entries - a mapping of link, user, and ts (when the Bitlink was created).
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' link_Metrics_EncodersByCount(link = "http://bit.ly/DPetrov", my_network = "false", limit = 100)
#' 
#' @importFrom curl curl_escape
#' @export
link_Metrics_EncodersByCount <- function(link, limit = 100, my_network = "false", expand_user = "false", 
                                           subaccounts = "false") {
  
  link_metrics_encoders_by_count_url <- "https://api-ssl.bitly.com/v3/link/encoders_by_count"
  
  created_URL <- paste(link_metrics_encoders_by_count_url, "?link=", curl_escape(link), "&limit=", limit, 
                      "&my_network=", my_network, "&expand_user=", expand_user, "&subaccounts=", subaccounts, sep = "")
  created_URL <- paste(created_URL, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df_link_metrics_encoders_by_count <- doRequest(created_URL)
  
  df_link_metrics_encoders_by_count_data <- data.frame(df_link_metrics_encoders_by_count$data$encoders_by_count)
  df_link_metrics_encoders_by_count_data$ts <- as.POSIXct(as.integer(df_link_metrics_encoders_by_count_data$ts), 
                                                          origin = "1970-01-01", tz = "UTC")
  
  return(df_link_metrics_encoders_by_count_data)
}


#' @title Returns metrics about the domains referring click traffic to a link.
#' 
#' @description Returns metrics about the domains referring click traffic to a single Bitlink.
#' 
#' @seealso \url{http://dev.bitly.com/link_metrics.html#v3_link_referring_domains}
#' 
#' @param link - a Bitlink.
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return all 
#' units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the 
#' maximum value for units is 60.
#' 
#' @return clicks - the number of clicks referred from this domain.
#' @return domain - the domain referring clicks.
#' @return url - the complete URL of the domain referring clicks.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' link_Metrics_ReferringDomains(link = "http://bit.ly/DPetrov", unit = "day",units = -1, limit = 100)
#' 
#' @importFrom curl curl_escape
#' @export
link_Metrics_ReferringDomains <- function(link, limit = 1000, unit = c("minute", "hour", "day", "week", "month"), 
                                           units = -1) {
  unit_matched <- match.arg(unit)
  
  link_metrics_referring_domains_url <- "https://api-ssl.bitly.com/v3/link/referring_domains"
  
  created_URL <- paste(link_metrics_referring_domains_url, "?link=", curl_escape(link), "&limit=", 
                      limit, "&unit=", unit_matched, "&units=", units, sep = "")
  created_URL <- paste(created_URL, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df_link_metrics_referring_domains <- doRequest(created_URL)
  
  df_link_metrics_referring_domains_data <- df_link_metrics_referring_domains$data$referring_domains
  
  # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
  df_link_metrics_referring_domains_data <- data.frame(t(sapply(df_link_metrics_referring_domains_data,c)))
  return(df_link_metrics_referring_domains_data)
}


#' @title Returns metrics about the pages referring click traffic to a single Bitlink.
#' 
#' @seealso \url{http://dev.bitly.com/link_metrics.html#v3_link_referrers}
#' 
#' @param link - a Bitlink.
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the maximum value for units is 60.
#' value for each period of time.
#' 
#' @return clicks - the number of clicks referred from this domain.
#' @return referrer - the URL referring clicks.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' link_Metrics_Referrers(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#' 
#' @importFrom curl curl_escape
#' @export
link_Metrics_Referrers <- function(link, limit = 1000, unit = c("minute", "hour", "day", "week", "month"), 
                                   units = -1) {
  unit_matched <- match.arg(unit)
  
  link_metrics_referrers_url <- "https://api-ssl.bitly.com/v3/link/referrers"
  
  created_URL <- paste(link_metrics_referrers_url, "?link=", curl_escape(link), "&limit=", limit, 
                      "&unit=", unit_matched, "&units=", units, sep = "")
  created_URL <- paste(created_URL, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df_link_metrics_referrers <- doRequest(created_URL)
  
  df_link_metrics_referrers_data <- df_link_metrics_referrers$data$referrers
  
  # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
  df_link_metrics_referrers_data <- data.frame(t(sapply(df_link_metrics_referrers_data, c)))
  return(df_link_metrics_referrers_data)
}


#' @title Returns metrics about the pages referring click traffic to a single Bitlink.
#' 
#' @description Returns metrics about the pages referring click traffic to a single Bitlink, grouped by referring domain.
#' 
#' @seealso \url{http://dev.bitly.com/link_metrics.html#v3_link_referrers_by_domain}
#' 
#' @param link - a Bitlink.
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return all units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the maximum value for units is 60.
#' value for each period of time.
#' 
#' @return clicks - the number of clicks referred from this domain.
#' @return referrer - the URL referring clicks.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' link_Metrics_ReferrersByDomain(link = "http://bit.ly/DPetrov", unit = "day", 
#' units = -1, limit = 100)
#' 
#' @importFrom curl curl_escape
#' @export
link_Metrics_ReferrersByDomain <- function(link, limit = 1000, unit = c("minute", "hour", "day", "week", "month"), 
                                             units = -1) {
  unit_matched <- match.arg(unit)
  
  link_metrics_referrers_by_domain_url <- "https://api-ssl.bitly.com/v3/link/referrers_by_domain"
  
  created_URL <- paste(link_metrics_referrers_by_domain_url, "?link=", curl_escape(link), "&limit=", 
                      limit, "&unit=", unit_matched, "&units=", units, sep = "")
  created_URL <- paste(created_URL, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df_link_metrics_referrers_by_domain <- doRequest(created_URL)
  
  df_link_metrics_referrers_by_domain_data <- df_link_metrics_referrers_by_domain$data$referrers
  
  # https://stackoverflow_com/questions/4227223/r-list-to-data-frame
  df_link_metrics_referrers_by_domain_data <- data.frame(t(sapply(df_link_metrics_referrers_by_domain_data, c)))
  
  # just guessing at the moment
  df_link_metrics_referrers_by_domain_data$type <- rownames(df_link_metrics_referrers_by_domain_data) 
  return(df_link_metrics_referrers_by_domain_data)
}


