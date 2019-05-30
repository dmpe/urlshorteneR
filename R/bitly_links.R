#' @title Create a short Bitlink 
#' 
#' @description See \url{https://dev.bitly.com/v4/#section/Rate-Limiting} and
#' \url{https://dev.bitly.com/v4/#operation/createFullBitlink}
#' Convert a long url to a Bitlink and set additional parameters. 
#'
#' @param long_url - required, a long URL to be shortened (example: http://betaworks.com/). Must contain http/https
#' @param domain - (optional) the short domain to use; either bit.ly, j.mp, or bitly.com or
#' a custom short domain. The default for this parameter is the short domain selected by each
#' user in their bitly account settings. Passing a specific domain via this parameter will override
#' the default settings.
#' @param title - title of the bitlink in \url{https://bit.ly} UI
#' @param tags - Array of string, use e.g. \code{c("test1", "test2")}
#' @param showRequestURL - show URL which has been build and requested from server. For debug
#' purposes.
#' @param deeplinks_app_uri_path - app_uri_path
#' @param deeplinks_install_type - install_type
#' @param deeplinks_install_url - install_url
#' @param deeplinks_app_id - app_id
#' 
#' @inheritParams bitly_create_campaigns
#' 
#' @note Look in the vignette for bulk shortening of URLs. Each call of this function == 1 API call.
#' Take that into consideration due to limits etc.
#' @note The bitly API does not support shortening more than one long URL with a single API call.
#' Meaning 1 Long URL = 1 Function call.
#' @note Long URLs should be URL-encoded. You can not include a longUrl in the request
#' that has &, ?, #, or other reserved parameters without first encoding it.
#' @note The default value for the domain parameter is selected by each user from within their bitly
#' account settings at \url{https://bitly.com/a/settings/advanced}.
#' @note Long URLs should not contain spaces: any longUrl with spaces will be rejected. All spaces
#' should be either percent encoded %20 or plus encoded +. Note that tabs, newlines and trailing
#' spaces are all indications of errors. Please remember to strip leading and trailing whitespace
#' from any user input before shortening.
#' 
#' @return id - a short bitly identifier for long_url which is unique to the given account.
#' @return long_url - This may not always be equal to the URL requested, as some URL normalization 
#' may occur (e.g., due to encoding differences, or case
#' differences in the domain). This long_url will always be functionally identical the the request
#' parameter.
#' @return link - an bitly id with http(s) prefix
#' @import httr jsonlite lubridate
#' 
#' @examples
#' \dontrun{
#' bitly_LinksShorten(long_url = "http://slovnik.seznam.cz/")
#' }
#' 
#' @export
bitly_create_bitlink <- function(long_url = NULL, domain = "bit.ly", title = NULL, tags = NULL, 
                               group_guid = NULL, 
                               deeplinks_list = list(app_uri_path = NULL, 
                                            install_type = NULL, 
                                            install_url = NULL, 
                                            app_id = NULL), showRequestURL = FALSE) {
   links_shorten_url <- "https://api-ssl.bitly.com/v4/bitlinks"
   
   body_req_query <- list(access_token = bitly_auth_access(), title = title, domain = domain,
                          long_url = long_url)
   
   if (!length(deeplinks_list$app_uri_path) == 0 || !length(deeplinks_list$install_type) == 0 ||
       !length(deeplinks_list$install_url) == 0 || !length(deeplinks_list$app_id) == 0) {
      body_req_query$deeplinks <- deeplinks_list
   }
   
   if (length(tags) >= 1) {
      body_req_query$tags <- tags
   }
   
   if (length(group_guid) >= 1) {
      body_req_query$group_guid <- group_guid
   }
   
   body_req_query_cleaned <- toJSON(body_req_query, auto_unbox = T)
   
   df_link_shorten <- doRequest("POST", links_shorten_url,
                                queryParameters = body_req_query_cleaned, showURL = showRequestURL)
   
   df_link_shorten <- data.frame(t(do.call(rbind, df_link_shorten)), stringsAsFactors = F)
   df_link_shorten$created_at <- now("UTC")
   
   return(df_link_shorten)
}


#' @title Get Metrics for a Bitlink by referring domains
#' 
#' @description This endpoint will rollup the click counts to a referrer about a single Bitlink.
#' 
#' @seealso See \url{https://dev.bitly.com/v4/#operation/getMetricsForBitlinkByReferringDomains}
#' 
#' @inheritParams bitly_retrieve_sorted_links
#'
#' @return facet - string | One of "countries" "referrers" "referrers_by_domain" 
#' "referring_domains" "referring_networks" "shorten_counts" 
#'
#' @param bitlink - required, a Bitlink made of the domain and hash
#'
#' @import httr jsonlite lubridate
#' 
#' @examples
#' \dontrun{
#' bitly_user_metrics_referring_domains(bitlink = "https://cnn.it/2HomWGB", unit = "month", units = -1, size = 100)
#' }
#' 
#' @export
bitly_user_metrics_referring_domains <- function(bitlink = NULL, unit = "day", units = -1, 
                                              size = 50, unit_reference = NULL,
                                              showRequestURL = FALSE) {

  user_metrics_referring_domains_url <- paste0("https://api-ssl.bitly.com/v4/bitlinks/", bitlink, "/referring_domains")
  
  query <- list(access_token = bitly_auth_access(), bitlink = bitlink, unit = unit, units = units, size = size,
      unit_reference = unit_reference)
   
  df_user_metrics_referring_domains <- doRequest("GET", user_metrics_referring_domains_url,
                                                  query, showURL = showRequestURL)
  df_user_metrics_referring_domains$unit_reference <- ymd_hms(df_user_metrics_referring_domains$unit_reference, tz = "UTC")
  
  if (length(df_user_metrics_referring_domains$metrics) == 0) {
     df_user_metrics_referring_domains <- NULL
     message("The domain has no metrics available. Has it been shared ?")
  }
   
  return(df_user_metrics_referring_domains)
}












#______________________________________________________
#' @title Query for a Bitlink based on a long URL.
#' 
#' @description See \url{http://dev.bitly.com/links.html#v3_link_lookup}
#' 
#' @param url - one long URLs to lookup.
#' @param showRequestURL - show URL which has been build and requested from server. For debug
#' purposes.
#' 
#' @return url - an echo back of the url parameter.
#' @return aggregate_link - the corresponding bitly aggregate link (global hash).
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(
#'   key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8",
#'   secret = "b7e4abaf8b26ec4daa92b1e64502736f5cd78899"
#' )
#' bitly_LinksLookup(url = "http://www.seznam.cz/")
#' bitly_LinksLookup(url = "http://www.seznam.cz/", showRequestURL = TRUE)
#' 
#' manyUrls <- list(
#'   "http://www.seznam.cz/", "http://www.seznamasdas.cz/",
#'   "http://www.seznam.cz/asadasd", "http://www.seznam.cz/adqwrewtregt"
#' )
#' for (u in 1:length(manyUrls)) {
#'   print(bitly_LinksLookup(url = manyUrls[[u]], showRequestURL = TRUE))
#' }
#' }
#' 
#' @export
bitly_LinksLookup <- function(url, showRequestURL = FALSE) {
 links_lookup_url <- "https://api-ssl.bitly.com/v3/link/lookup"

 query <- list(access_token = bitly_auth_access(), url = url)

 # call method from ApbiKey.R
 df_link_lookup <- doRequest("GET", links_lookup_url, "bitly", query, showURL = showRequestURL)
 df_link_lookup_data <- df_link_lookup$data$link_lookup

 # sapply(df_link_lookup_data, class)
 return(df_link_lookup_data)
}

#' @title Used to return the page title for a given Bitlink.
#' 
#' @description See \url{http://dev.bitly.com/links.html#v3_info}
#' 
#' @note Either shortUrl or hash must be given as a parameter (or both).
#' @note The maximum number of shortUrl and hash parameters is 15.
#' 
#' @param hashIN - refers to one bitly hashes, (e.g.:  2bYgqR or a-custom-name). Required
#' @param shortUrl - refers to one Bitlinks e.g.: http://bit.ly/1RmnUT or http://j.mp/1RmnUT.
#' Optional.
#' @param expand_user - optional true|false (default) - include extra user info in response.
#' @param showRequestURL - show URL which has been build and requested from server.
#' For debug purposes.
#' 
#' @return short_url - this is an echo back of the shortUrl request parameter.
#' @return hash - this is an echo back of the hash request parameter.
#' @return user_hash - the corresponding bitly user identifier.
#' @return global_hash - the corresponding bitly aggregate identifier.
#' @return error - indicates there was an error retrieving data for a given shortUrl or hash. An
#' example error is "NOT_FOUND".
#' @return title - the HTML page title for the destination page (when available).
#' @return created_by - the bitly username that originally shortened this link, if the
#' link is public. Otherwise, null.
#' @return created_at - the epoch timestamp when this Bitlink was created.
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(
#'   key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8",
#'   secret = "b7e4abaf8b26ec4daa92b1e64502736f5cd78899"
#' )
#' bitly_LinksInfo(shortUrl = "http://bit.ly/DPetrov")
#' bitly_LinksInfo(hash = "DPetrov", showRequestURL = TRUE)
#' bitly_LinksInfo(hash = "DPetrov", expand_user = "true")
#' 
#' ## hash is the one which is only returned. Dont use
#' bitly_LinksInfo(shortUrl = "on.natgeo.com/1bEVhwE", hash = "DPetrov")
#' 
#' ## manyHashes <- list("DPetrov", "1QU8CFm", "1R1LPSE", "1LNqqva")
#' ## for (u in 1:length(manyHashes)) {
#' ##   print(bitly_LinksInfo(hashIN = manyHashes[[u]], showRequestURL = TRUE))
#' ## }
#' }
#' 
#' @export
bitly_LinksInfo <- function(hashIN = NULL, shortUrl = NULL, expand_user = "true", showRequestURL = FALSE) {
 links_info_url <- "https://api-ssl.bitly.com/v3/info"

 if (is.null(hashIN)) {
   query <- list(access_token = bitly_auth_access(), shortUrl = shortUrl, expand_user = expand_user)
 } else {
   query <- list(access_token = bitly_auth_access(), hash = hashIN, expand_user = expand_user)
 }

 # call method from ApbiKey.R
 df_link_info <- doRequest("GET", links_info_url, "bitly", query, showURL = showRequestURL)

 df_user_info_data <- data.frame(t(sapply(unlist(df_link_info$data$info), c)), stringsAsFactors = FALSE)
 df_user_info_data$created_at <- as.POSIXct(as.integer(df_user_info_data$created_at),
   origin = "1970-01-01", tz = "UTC"
 )

 # sapply(df_link_info_data, class)
 return(df_user_info_data)
}


#' @title Given a bitly URL or hash (or multiple), returns the target (long) URL.
#' 
#' @description See \url{http://dev.bitly.com/links.html#v3_expand}
#' 
#' @param hashIN - refers to one bitly hashes, (e.g.:  2bYgqR or a-custom-name). Required
#' @param shortUrl - refers to one Bitlinks e.g.: http://bit.ly/1RmnUT or http://j.mp/1RmnUT.
#' Optional.
#' @param showRequestURL - show URL which has been build and requested from server.
#' For debug purposes.
#' 
#' @section TODO: or more URLs  Up TO 15
#' 
#' @note Either shortUrl or hash must be given as a parameter.
#' @note The maximum number of shortUrl and hash parameters is 15.
#' 
#' @return short_url - this is an echo back of the shortUrl request parameter.
#' @return hash - this is an echo back of the hash request parameter.
#' @return user_hash - the corresponding bitly user identifier.
#' @return global_hash - the corresponding bitly aggregate identifier.
#' @return error - indicates there was an error retrieving data for a given shortUrl or hash. An
#' example error is "NOT_FOUND".
#' @return long_url - the URL that the requested short_url or hash points to.
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(
#'   key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8",
#'   secret = "b7e4abaf8b26ec4daa92b1e64502736f5cd78899"
#' )
#' bitly_LinksExpand(shortUrl = "http://bit.ly/DPetrov")
#' bitly_LinksExpand(hash = "DPetrov", showRequestURL = TRUE)
#' bitly_LinksExpand(hash = "DPetrov")
#' bitly_LinksExpand(shortUrl = "on.natgeo.com/1bEVhwE", hash = "1bEVhwE")
#' 
#' ## manyHashes <- list("DPetrov", "1QU8CFm", "1R1LPSE", "1LNqqva")
#' ## for (u in 1:length(manyHashes)) {
#' ##   print(bitly_LinksExpand(hashIN = manyHashes[[u]], showRequestURL = TRUE))
#' ## }
#' }
#' 
#' @export
bitly_LinksExpand <- function(hashIN = NULL, shortUrl = NULL, showRequestURL = FALSE) {
 links_expand_url <- "https://api-ssl.bitly.com/v3/expand"

 if (is.null(hashIN)) {
   query <- list(access_token = bitly_auth_access(), shortUrl = shortUrl)
 } else {
   query <- list(access_token = bitly_auth_access(), hash = hashIN)
 }

 # call method from ApbiKey.R
 df_link_expand <- doRequest("GET", links_expand_url, "bitly", query, showURL = showRequestURL)

 df_link_expand_data <- data.frame(t(sapply(unlist(df_link_expand$data$expand), c)), stringsAsFactors = FALSE)

 # sapply(df_link_expand_data, class)
 return(df_link_expand_data)
}




#' @title Returns the aggregate number of clicks on all of the authenticated user's Bitlinks.
#' 
#' @description See \url{http://dev.bitly.com/user_metrics.html#v3_user_clicks}
#' 
#' @param limit - 1 to 1000 (default=1000).
#' @param units - an integer representing the time units to query data for. Pass -1 to return all
#' units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the
#' maximum value for units is 60.
#' @param rollup - true or false. Return data for multiple units rolled up to a single result
#' instead of a separate value for each period of time.
#' @param showRequestURL - show URL which has been build and requested from server. For debug
#' purposes.
#' 
#' @return dt - a unix timestamp representing the beginning of this unit.
#' @return day_start - a unix timestamp representing the beginning of the specified day (ONLY
#' returned if unit is not specified).
#' @return clicks - the number of clicks on this user's links in the specified timeframe.
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(key = "", secret = "")
#' bitly_UserMetricsClicks(unit = "day", units = -1, limit = 100, rollup = "true")
#' bitly_UserMetricsClicks(unit = "day", units = -1, limit = 100, rollup = "false")
#' }
#' 
#' @note without the parameter unit this endpoint returns a legacy response format which assumes
#' rollup=false, unit=day and units=7.
#' 
#' @export
bitly_UserMetricsClicks <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"),
                                   units = -1, rollup = c("false", "true"), showRequestURL = FALSE) {
 unit_matched <- match.arg(unit)
 rollup_matched <- match.arg(rollup)

 user_metrics_clicks_url <- "https://api-ssl.bitly.com/v4/user/clicks"

 query <- list(
   access_token = bitly_token$credentials$access_token, limit = limit,
   unit = unit_matched, units = units, rollup = rollup_matched
 )

 # call method from ApiKey.R
 df_user_metrics_clicks <- doRequest("GET", user_metrics_clicks_url, query, showURL = showRequestURL)
 df_user_metrics_clicks_data <- df_user_metrics_clicks$data$user_clicks

 if (rollup == "true") {

   # won't return a data frame, just a number
   return(df_user_metrics_clicks_data)
 } else {
   df_user_metrics_clicks_data$dt <- as.POSIXct(as.integer(df_user_metrics_clicks_data$dt),
     origin = "1970-01-01", tz = "UTC"
   )
   # sapply(df_user_metrics_clicks_data, class)
   return(df_user_metrics_clicks_data)
 }
}


#' @title Returns aggregate metrics about the countries referring click traffic to all of the
#' authenticated user's Bitlinks.
#' 
#' @description See \url{http://dev.bitly.com/user_metrics.html#v3_user_countries}
#' 
#' @inheritParams bitly_UserMetricsClicks
#' 
#' @return clicks - the number of clicks referred from this country.
#' @return country - the two-letter code of the referring country.
#' 
#' @note When a unit is specified (always the case), rollup is always (!) true.
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(key = "", secret = "")
#' bitly_UserMetricsCountries(unit = "day", units = -1, limit = 100, rollup = "true")
#' }
#' 
#' @export
bitly_UserMetricsCountries <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"),
                                      rollup = "true", units = -1, showRequestURL = FALSE) {
 unit_matched <- match.arg(unit)

 user_metrics_countries_url <- "https://api-ssl.bitly.com/v4/user/countries"

 query <- list(
   access_token = bitly_token$credentials$access_token, limit = limit, unit = unit_matched, units = units,
   rollup = rollup
 )

 # call method from ApiKey.R
 df_user_metrics_countries <- doRequest("GET", url = user_metrics_countries_url, query, showURL = showRequestURL)

 df_user_metrics_countries_data <- df_user_metrics_countries$data$user_countries

 # sapply(df_user_metrics_countries_data, class)
 return(df_user_metrics_countries_data)
}

#' @title Returns the authenticated user's most-clicked Bitlinks (ordered by number of clicks) in
#' a given time period.
#' 
#' @description See \url{http://dev.bitly.com/user_metrics.html#v3_user_popular_links}
#' 
#' @inheritParams bitly_UserMetricsClicks
#' 
#' @return link - a Bitlink.
#' @return clicks - the number of clicks on that Bitlink in the specified timeframe.
#' 
#' @note This has replaced the realtime_links endpoint.
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(key = "", secret = "")
#' bitly_UserMetricsPopularLinks(unit = "day", units = -1, limit = 100)
#' }
#' 
#' @export
bitly_UserMetricsPopularLinks <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"),
                                         units = -1, showRequestURL = FALSE) {
 unit_matched <- match.arg(unit)

 user_metrics_popular_links_url <- "https://api-ssl.bitly.com/v3/user/popular_links"

 query <- list(access_token = bitly_token$credentials$access_token, limit = limit, unit = unit_matched, units = units)

 # call method from ApiKey.R
 df_user_metrics_popular_links <- doRequest("GET", user_metrics_popular_links_url, query, showURL = showRequestURL)
 df_user_metrics_popular_links_data <- df_user_metrics_popular_links$data$popular_links

 # sapply(df_user_metrics_popular_links_data, class)
 return(df_user_metrics_popular_links_data)
}


#' @title Returns aggregate metrics about the pages referring click traffic to all of the
#' authenticated user's Bitlinks.
#' 
#' @description See \url{http://dev.bitly.com/user_metrics.html#v3_user_referrers}
#' 
#' @inheritParams bitly_UserMetricsClicks
#' 
#' @return clicks - the number of clicks referred from this URL.
#' @return referrer - the URL referring clicks.
#' 
#' @note When a unit is specified (always the case), rollup is always (!) true.
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(key = "", secret = "")
#' user_Metrics_Referrers(unit = "day", units = -1, limit = 100, rollup = "true")
#' }
#' 
#' @export
bitly_UserMetricsReferrers <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"),
                                      rollup = c("false", "true"), units = -1, showRequestURL = FALSE) {
 unit_matched <- match.arg(unit)

 user_metrics_referrers_url <- "https://api-ssl.bitly.com/v4/user/referrers"

 query <- list(
   access_token = bitly_token$credentials$access_token, limit = limit,
   unit = unit_matched, units = units, rollup = rollup
 )

 # call method from ApiKey.R
 df_user_metrics_referrers <- doRequest("GET", user_metrics_referrers_url, query, showURL = showRequestURL)
 df_user_metrics_referrers_data <- df_user_metrics_referrers$data$user_referrers

 # sapply(df_user_metrics_referrers_data, class)
 return(df_user_metrics_referrers_data)
}




#' @title Returns the number of Bitlinks created in a given time period by the authenticated user.
#' 
#' @description See \url{http://dev.bitly.com/user_metrics.html#v3_user_shorten_counts}
#' 
#' @inheritParams bitly_UserMetricsClicks
#' 
#' @return dt - datetime when shortens had been made.
#' @return shortens - the number of shortens made by the specified user in the specified time.
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(key = "", secret = "")
#' bitly_UserMetricsShortenCounts(unit = "day", units = -1, limit = 100, rollup = "true")
#' bitly_UserMetricsShortenCounts(unit = "day", units = -1, limit = 100, rollup = "false")
#' bitly_UserMetricsShortenCounts(unit = "day", units = -1, limit = 100)
#' }
#' 
#' @export
bitly_UserMetricsShortenCounts <- function(limit = 1000, unit = c("minute", "hour", "day", "week", "month"),
                                          rollup = c("false", "true"), units = -1, showRequestURL = FALSE) {
 unit_matched <- match.arg(unit)
 rollup_matched <- match.arg(rollup)

 user_metrics_shorten_counts_url <- "https://api-ssl.bitly.com/v4/user/shorten_counts"

 query <- list(
   access_token = bitly_token$credentials$access_token, limit = limit,
   unit = unit_matched, units = units, rollup = rollup_matched, showURL = showRequestURL
 )

 # call method from ApiKey.R
 df_user_metrics_shorten_counts <- doRequest("GET", user_metrics_shorten_counts_url, query,
   showURL = showRequestURL
 )
 df_user.metrics_shorten_counts_data <- df_user_metrics_shorten_counts$data$user_shorten_counts

 if (rollup_matched == "false") {
   df_user.metrics_shorten_counts_data$dt <- as.POSIXct(as.integer(df_user.metrics_shorten_counts_data$dt),
     origin = "1970-01-01", tz = "UTC"
   )
 } else {
   df_user.metrics_shorten_counts_data
 }

 # sapply(df_user.metrics_shorten_counts_data, class)
 return(df_user.metrics_shorten_counts_data)
}

#' @title Returns numbers of click on a link.
#' 
#' @description Returns the number of clicks on a single Bitlink of the authenticated user.
#' 
#' @seealso \url{http://dev.bitly.com/link_metrics.html#v3_link_clicks}
#' 
#' @param link - a Bitlink.
#' @param limit - 1 to 1000 (default=1000).
#' @param rollup - true (default) or false.  Return data for multiple units rolled up to a
#' single result instead of a separate value for each period of time.
#' @param units - an integer representing the time units to query data for. Pass -1 to return all
#' units of time.
#' @param unit - minute, hour, day, week or month, default: day; Note: when unit is minute the
#' maximum value for units is 60.
#' @param showRequestURL - show URL which has been build and requested from server. For debug
#' purposes.
#' 
#' @return clicks - the number of clicks on the specified Bitlink.
#' @return dt - time in UTC format (only when rollup = "false")
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(key = "", secret = "")
#' bitly_LinksMetricsClicks(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#' }
#' 
#' @export
bitly_LinksMetricsClicks <- function(link, limit = 1000, unit = c("minute", "hour", "day", "week", "month"),
                                    units = -1, rollup = "true", showRequestURL = FALSE) {
 unit_matched <- match.arg(unit)

 link_metrics_clicks_url <- "https://api-ssl.bitly.com/v3/link/clicks"

 query <- list(
   access_token = bitly_token$credentials$access_token, link = link, limit = limit, unit = unit_matched, units = units,
   rollup = rollup
 )

 # call method from ApiKey.R
 df.link_metrics_clicks <- doRequest("GET", link_metrics_clicks_url, query, showURL = showRequestURL)

 df.link_metrics_clicks_data <- df.link_metrics_clicks$data$link_clicks

 if (rollup == "true") {
   return(df.link_metrics_clicks_data)
 } else {
   df.link_metrics_clicks_data$dt <- as.POSIXct(df.link_metrics_clicks_data$dt, origin = "1970-01-01", tz = "UTC")
 }

 # sapply(df.link_metrics_clicks_data, class)

 return(df.link_metrics_clicks_data)
}


#' @title Returns metrics about the countries from a link.
#' 
#' @description Returns metrics about the countries referring click traffic to a single Bitlink.
#' 
#' @seealso \url{http://dev.bitly.com/link_metrics.html#v3_link_countries}
#' 
#' @inheritParams bitly_LinksMetricsClicks
#' 
#' @return clicks - the number of clicks referred from this country.
#' @return country - the two-letter code of the referring country.
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(key = "", secret = "")
#' bitly_LinksMetricsCountries(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#' bitly_LinksMetricsCountries(
#'   link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100,
#'   showRequestURL = TRUE
#' )
#' }
#' 
#' @export
bitly_LinksMetricsCountries <- function(link, limit = 1000, unit = c("minute", "hour", "day", "week", "month"),
                                       units = -1, showRequestURL = FALSE) {
 unit_matched <- match.arg(unit)

 link_metrics_countries_url <- "https://api-ssl.bitly.com/v3/link/countries"

 query <- list(
   access_token = bitly_token$credentials$access_token, link = link, service = "bitly",
   limit = limit, unit = unit_matched, units = units
 )

 # call method from ApiKey.R
 df_link_metrics_countries <- doRequest("GET", link_metrics_countries_url, query, showURL = showRequestURL)

 df_link_metrics_countries_data <- df_link_metrics_countries$data$countries

 # sapply(df_link_metrics_countries_data, class)
 return(df_link_metrics_countries_data)
}

#' @title Returns users who have encoded this long URL.
#' 
#' @description Returns users who have encoded this long URL (optionally only those in
#' the requesting user's social graph).
#' 
#' @seealso \url{http://dev.bitly.com/link_metrics.html#v3_link_encoders}
#' 
#' @note Some users may not be returned from this call depending on Bitlink privacy settings.
#' 
#' @inheritParams bitly_LinksMetricsClicks
#' 
#' @param my_network - true or false (default) restrict to my network.
#' @param subaccounts - (only available to enterprise accounts) false (always default) restrict to
#' this enterprise account and its subaccounts
#' @param expand_user - true or false (default) include display names of encoders.
#' 
#' @return entries - a mapping of link, user, and ts (when the Bitlink was created) and possible
#' more depending on input parameters.
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(key = "", secret = "")
#' bitly_LinksMetricsEncoders(link = "http://bit.ly/DPetrov")
#' bitly_LinksMetricsEncoders("http://bit.ly/DPetrov", expand_user = "true", my_network = "false")
#' }
#' 
#' @export
bitly_LinksMetricsEncoders <- function(link, my_network = "false", limit = 25, expand_user = "false",
                                      subaccounts = "false", showRequestURL = FALSE) {
 link_metrics_encoders_url <- "https://api-ssl.bitly.com/v3/link/encoders"

 query <- list(
   access_token = bitly_token$credentials$access_token, link = link,
   limit = limit, my_network = my_network,
   expand_user = expand_user, subaccounts = subaccounts
 )

 # call method from ApiKey.R
 df_link_metrics_encoders <- doRequest("GET", link_metrics_encoders_url, query, showURL = showRequestURL)

 df_link_metrics_encoders_data <- df_link_metrics_encoders$data$entries

 df_link_metrics_encoders_data$ts <- as.POSIXct(as.integer(df_link_metrics_encoders_data$ts),
   origin = "1970-01-01", tz = "UTC"
 )
 # sapply(df_link_metrics_encoders_data, class)
 return(df_link_metrics_encoders_data)
}


#' @title Returns the number of users who have shortened a link.
#' 
#' @description Returns the number of users who have shortened (encoded) a single Bitlink.
#' 
#' @seealso \url{http://dev.bitly.com/link_metrics.html#v3_link_encoders_count}
#' 
#' @inheritParams bitly_LinksMetricsClicks
#' 
#' @return aggregate_link - the aggregate (global) Bitlink for the provided Bitlink.
#' @return count - the number of bitly users who have shortened (encoded) this link.
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(key = "", secret = "")
#' bitly_LinksMetricsEncodersCount(link = "http://bit.ly/DPetrov")
#' }
#' 
#' @export
bitly_LinksMetricsEncodersCount <- function(link, showRequestURL = FALSE) {
 link_metrics_encoders_count_url <- "https://api-ssl.bitly.com/v3/link/encoders_count"

 query <- list(access_token = bitly_token$credentials$access_token, link = link)

 # call method from ApiKey.R
 df_link_metrics_encoders_count <- doRequest("GET", link_metrics_encoders_count_url,
   query, showURL = showRequestURL
 )
 df_link_metrics_encoders_count_data <- df_link_metrics_encoders_count$data

 # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
 df_link_metrics_encoders_count_data <- data.frame(t(sapply(df_link_metrics_encoders_count_data, c)),
   stringsAsFactors = FALSE
 )
 df_link_metrics_encoders_count_data$count <- as.numeric(df_link_metrics_encoders_count_data$count)

 # sapply(df_link_metrics_encoders_count_data, class)
 return(df_link_metrics_encoders_count_data)
}

#' @title Returns users who have encoded this link.
#' 
#' @description Returns users who have encoded this link (optionally only those in the requesting
#' user's social graph), sorted by the number of clicks on each encoding user's link.
#' 
#' @seealso See \url{http://dev.bitly.com/link_metrics.html#v3_link_encoders_by_count}
#' 
#' @note The response will only contain users whose links have gotten at least one click, and
#' will not contain any users whose links are private.
#' 
#' @inheritParams bitly_LinksMetricsClicks
#' 
#' @param my_network - true or false (default) restrict to my network
#' @param subaccounts - (only available to enterprise accounts) false (always default) restrict to
#' this enterprise account and its subaccounts
#' @param expand_user - false (always default) include display names of encoders
#' 
#' @return entries - a mapping of link, user, and ts (when the Bitlink was created) and possible
#' more depending on input parameters.
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(key = "", secret = "")
#' bitly_LinksMetricsEncodersByCount("http://bit.ly/DPetrov", my_network = "false", limit = 100)
#' bitly_LinksMetricsEncodersByCount("http://bit.ly/DPetrov",
#'   my_network = "false", limit = 100,
#'   expand_user = "true"
#' )
#' }
#' 
#' @export
bitly_LinksMetricsEncodersByCount <- function(link, limit = 100, my_network = "false", expand_user = "false",
                                             subaccounts = "false", showRequestURL = FALSE) {
 link_metrics_encoders_by_count_url <- "https://api-ssl.bitly.com/v3/link/encoders_by_count"

 query <- list(
   access_token = bitly_token$credentials$access_token, link = link, limit = limit, my_network = my_network,
   expand_user = expand_user, subaccounts = subaccounts
 )

 # call method from ApiKey.R
 df_link_metrics_encoders_by_count <- doRequest("GET", link_metrics_encoders_by_count_url,
   query, showURL = showRequestURL
 )

 df_link_metrics_encoders_by_count_data <- data.frame(df_link_metrics_encoders_by_count$data$encoders_by_count)
 df_link_metrics_encoders_by_count_data$ts <- as.POSIXct(as.integer(df_link_metrics_encoders_by_count_data$ts),
   origin = "1970-01-01", tz = "UTC"
 )

 # sapply(df_link_metrics_encoders_by_count_data, class)
 return(df_link_metrics_encoders_by_count_data)
}


#' @title Returns metrics about the domains referring click traffic to a link.
#' 
#' @description Returns metrics about the domains referring click traffic to a single Bitlink.
#' 
#' @seealso \url{http://dev.bitly.com/link_metrics.html#v3_link_referring_domains}
#' 
#' @inheritParams bitly_LinksMetricsClicks
#' 
#' @return clicks - the number of clicks referred from this domain.
#' @return domain - the domain referring clicks.
#' @return url - the complete URL of the domain referring clicks.
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(key = "", secret = "")
#' bitly_LinksMetricsReferringDomains("http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#' }
#' 
#' @export
bitly_LinksMetricsReferringDomains <- function(link, limit = 1000, unit = c("minute", "hour", "day", "week", "month"),
                                              units = -1, showRequestURL = FALSE) {
 unit_matched <- match.arg(unit)

 link_metrics_referring_domains_url <- "https://api-ssl.bitly.com/v3/link/referring_domains"

 query <- list(
   access_token = bitly_token$credentials$access_token, link = link,
   limit = limit, unit = unit_matched, units = units
 )

 # call method from ApiKey.R
 df_link_metrics_referring_domains <- doRequest("GET", link_metrics_referring_domains_url,
   query, showURL = showRequestURL
 )
 df_link_metrics_referring_domains_data <- df_link_metrics_referring_domains$data$referring_domains

 # sapply(df_link_metrics_referring_domains_data, class)

 return(df_link_metrics_referring_domains_data)
}


#' @title Returns metrics about the pages referring click traffic to a single Bitlink.
#' 
#' @description \url{http://dev.bitly.com/link_metrics.html#v3_link_referrers}
#' 
#' @inheritParams bitly_LinksMetricsClicks
#' 
#' @return clicks - the number of clicks referred from this domain.
#' @return referrer - the URL referring clicks.
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(key = "", secret = "")
#' bitly_LinksMetricsReferrers(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#' }
#' 
#' @export
bitly_LinksMetricsReferrers <- function(link, limit = 1000, unit = c("minute", "hour", "day", "week", "month"),
                                       units = -1, showRequestURL = FALSE) {
 unit_matched <- match.arg(unit)

 link_metrics_referrers_url <- "https://api-ssl.bitly.com/v3/link/referrers"

 query <- list(
   access_token = bitly_token$credentials$access_token, link = link,
   limit = limit, unit = unit_matched, units = units
 )

 # call method from ApiKey.R
 df_link_metrics_referrers <- doRequest("GET", link_metrics_referrers_url, query, showURL = showRequestURL)

 df_link_metrics_referrers_data <- df_link_metrics_referrers$data$referrers

 # sapply(df_link_metrics_referrers_data, class)

 return(df_link_metrics_referrers_data)
}


#' @title Returns metrics about the pages referring click traffic to a single Bitlink, grouped
#' by referring domain.
#' 
#' @description \url{http://dev.bitly.com/link_metrics.html#v3_link_referrers_by_domain}
#' 
#' @inheritParams bitly_LinksMetricsClicks
#' 
#' @return clicks - the number of clicks referred from this domain.
#' @return referrer - the URL referring clicks.
#' 
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(key = "", secret = "")
#' bitly_LinksMetricsReferrersByDomain("http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#' }
#' 
#' @export
bitly_LinksMetricsReferrersByDomain <- function(link, limit = 1000, unit = c("minute", "hour", "day", "week", "month"),
                                               units = -1, showRequestURL = FALSE) {
 unit_matched <- match.arg(unit)

 link_metrics_referrers_by_domain_url <- "https://api-ssl.bitly.com/v3/link/referrers_by_domain"

 query <- list(access_token = bitly_token$credentials$access_token, link = link, limit = limit, unit = unit_matched, units = units)

 # call method from ApiKey.R
 df_link_metrics_referrers_by_domain <- doRequest("GET", link_metrics_referrers_by_domain_url,
 query, showURL = showRequestURL
 )

 df_link_metrics_referrers_by_domain_data <- df_link_metrics_referrers_by_domain$data$referrers

 # https://stackoverflow_com/questions/4227223/r-list-to-data-frame
 df_link_metrics_referrers_by_domain_data <- data.frame(t(sapply(df_link_metrics_referrers_by_domain_data, c)))

 df_link_metrics_referrers_by_domain_data$type <- rownames(df_link_metrics_referrers_by_domain_data)
 df_link_metrics_referrers_by_domain_data$referrer <- as.character(df_link_metrics_referrers_by_domain_data$referrer)
 df_link_metrics_referrers_by_domain_data$clicks <- as.integer(df_link_metrics_referrers_by_domain_data$clicks)

 # sapply(df_link_metrics_referrers_by_domain_data, class)

 return(df_link_metrics_referrers_by_domain_data)
}
