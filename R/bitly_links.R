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
#' bitly_create_bitlink(long_url = "http://slovnik.seznam.cz/")
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
   
   body_req_query <- list(access_token = bitly_auth_access(), domain = domain,
                          long_url = long_url)
   
   if (!length(deeplinks_list$app_uri_path) == 0 || !length(deeplinks_list$install_type) == 0 ||
       !length(deeplinks_list$install_url) == 0 || !length(deeplinks_list$app_id) == 0) {
      body_req_query$deeplinks <- deeplinks_list
   }
   
   if (length(title) >= 1) {
      body_req_query$title <- title
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
#' bitly_user_metrics_referring_domains(bitlink = "cnn.it/2HomWGB", unit = "month", units = -1, size = 100)
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

#' @title Expand a Bitlink 
#' 
#' @description See \url{https://dev.bitly.com/v4/#operation/expandBitlink}
#' This endpoint returns public information for a Bitlink.
#'
#' @inheritParams bitly_user_metrics_referring_domains
#' 
#' @return long_url - a full URL to which bitlink points to
#' 
#' @examples
#' \dontrun{
#' bitly_expand_link(bitlink_id = "bit.ly/DPetrov")
#' bitly_expand_link(bitlink_id = "on.natgeo.com/1bEVhwE")
#' 
#' ## manyHashes <- list("bit.ly/DPetrov", "bit.ly/1QU8CFm", "bit.ly/1R1LPSE", "bit.ly/1LNqqva")
#' ## for (u in 1:length(manyHashes)) {
#' ##   print(bitly_expand_link(bitlink_id = manyHashes[[u]], showRequestURL = TRUE))
#' ## }
#' }
#' @import httr jsonlite lubridate
#' @export
bitly_expand_link <- function(bitlink_id = NULL, showRequestURL = FALSE) {
   links_expand_url <- "https://api-ssl.bitly.com/v4/expand"
   
   if (!is.null(bitlink_id)) {
      body_req_query <- list(access_token = bitly_auth_access(), 
                             bitlink_id = bitlink_id)
   }
   
   df_link_expand <- doRequest("POST", links_expand_url, queryParameters = body_req_query, showURL = showRequestURL)
   df_link_expand <- data.frame(df_link_expand, stringsAsFactors = FALSE)
   df_link_expand$created_at <- ymd_hms(df_link_expand$created_at, tz = "UTC")
   
   return(df_link_expand)
}
   

#' @title Shorten a Link 
#' 
#' @description See \url{https://dev.bitly.com/v4/#operation/createBitlink}
#' Convert a long url to a Bitlink
#' 
#' @inheritParams bitly_create_bitlink
#' 
#' @examples
#' \dontrun{
#' bitly_shorten_link(url = "http://www.seznam.cz/")
#' bitly_shorten_link(url = "http://www.seznam.cz/", showRequestURL = TRUE)
#' 
#' manyUrls <- list(
#'   "http://www.seznam.cz/", "http://www.seznamasdas.cz/",
#'   "http://www.seznam.cz/asadasd", "http://www.seznam.cz/adqwrewtregt"
#' )
#' for (u in 1:length(manyUrls)) {
#'   print(bitly_shorten_link(long_url = manyUrls[[u]], showRequestURL = TRUE))
#' }
#' }
#' @import httr jsonlite lubridate
#' @export
bitly_shorten_link <- function(domain = "bit.ly", group_guid = NULL, long_url = NULL, showRequestURL = FALSE) {
   links_shorten_url <- "https://api-ssl.bitly.com/v4/shorten"
   
   if (!is.null(long_url)) {
      body_req_query <- list(access_token = bitly_auth_access(), group_guid = group_guid,
                             long_url = long_url, domain = domain)
   }
   
   df_link_shorten <- doRequest("POST", links_shorten_url, queryParameters = body_req_query, showURL = showRequestURL)
   df_link_shorten <- data.frame(t(do.call(rbind, df_link_shorten)), stringsAsFactors = F)
   df_link_shorten$created_at <- now("UTC")
   
   return(df_link_shorten)
}

#' @title Update a Bitlink 
#' 
#' @description See \url{https://dev.bitly.com/v4/#operation/updateBitlink}
#' Update fields in the Bitlink
#' 
#' @inheritParams bitly_create_bitlink
#' 
#' @examples
#' \dontrun{
#' bitly_update_bitlink(bitlink = "bit.ly/DPetrov", title = "novy titulek")
#' 
#' ## hash is the one which is only returned. Dont use
#' bitly_update_bitlink(bitlink = "on.natgeo.com/1bEVhwE")
#' 
#' ## manyHashes <- list("bit.ly/DPetrov", "bit.ly/1QU8CFm", "bit.ly/1R1LPSE", "bit.ly/1LNqqva")
#' ## for (u in 1:length(manyHashes)) {
#' ##   print(bitly_update_bitlink(bitlink = manyHashes[[u]], title = stri_rand_strings(1, 8, pattern = "[A-Za-z0-9]")))
#' ## }
#' }
#' @import httr jsonlite lubridate stringi
#' @export
bitly_update_bitlink <- function(bitlink = NULL, archived = NULL, tags = NULL, showRequestURL = FALSE, 
                                 created_at = NULL, title = NULL, created_by = NULL, long_url = NULL, 
                                 client_id = NULL, custom_bitlinks = NULL, link = NULL, id = NULL, 
                                 deeplinks = list(bitlink = NULL, install_url = NULL, created = NULL,
                                                  modified = NULL, app_uri_path = NULL, install_type = NULL,
                                                  app_guid = NULL, guid = NULL, os = NULL)) {
   
   link_update <- paste0("https://api-ssl.bitly.com/v4/bitlinks/", bitlink)
   
   query <- list(access_token = bitly_auth_access())
   body_upd = list()
   
   
   if (!length(deeplinks$bitlink) == 0 || !length(deeplinks$created) == 0 ||
       !length(deeplinks$install_url) == 0 || !length(deeplinks$app_uri_path) == 0 ||
       !length(deeplinks$modified) == 0 || !length(deeplinks$install_type) == 0 ||
       !length(deeplinks$app_guid) == 0 || !length(deeplinks$guid) == 0 || 
       !length(deeplinks$os) == 0) {
      body_upd$deeplinks <- deeplinks
   }
   
   if (length(tags) >= 1) {
      body_upd$tags <- tags
   }
   
   if (length(archived) >= 1) {
      body_upd$archived <- archived
   }
   
   if (length(created_at) >= 1) {
      body_upd$created_at <- created_at
   }
  
   if (length(created_by) >= 1) {
      body_upd$created_by <- created_by
   }
   
   if (length(title) >= 1) {
      body_upd$title <- title
   }
   
   if (length(long_url) >= 1) {
      body_upd$long_url <- long_url
   }
   
   if (length(client_id) >= 1) {
      body_upd$client_id <- client_id
   }
   
   if (length(custom_bitlinks) >= 1) {
      body_upd$custom_bitlinks <- custom_bitlinks
   }
   
   if (length(link) >= 1) {
      body_upd$link <- link
   }
   
   if (length(id) >= 1) {
      body_upd$id <- id
   }
   
   body_req_query_cleaned <- toJSON(body_upd, auto_unbox = T)
   
   df_update_pref <- doRequest("PATCH", url = link_update, queryParameters = query, 
                               patch_body = body_req_query_cleaned, showURL = showRequestURL)
   
   df_update_pref <- data.frame(t(do.call(rbind, df_update_pref)), stringsAsFactors = F)
   df_update_pref$created_at <- ymd_hms(df_update_pref$created_at, tz = "UTC")
   
   return(df_update_pref)
}



#' @title Retrieve a Bitlink 
#' 
#' @description This endpoint returns information for a Bitlink.
#' 
#' @seealso See \url{https://dev.bitly.com/v4/#operation/getBitlink}
#' 
#' @inheritParams bitly_retrieve_sorted_links
#'
#' @import httr jsonlite lubridate
#' 
#' @examples
#' \dontrun{
#' bitly_retrieve_bitlink(bitlink = "cnn.it/2HomWGB")
#' }
#' 
#' @export
bitly_retrieve_bitlink  <- function(bitlink = NULL, showRequestURL = FALSE) {
   
   get_bitlink_url <- paste0("https://api-ssl.bitly.com/v4/bitlinks/", bitlink)
   
   query <- list(access_token = bitly_auth_access(), bitlink = bitlink)
   
   df_bitlink <- doRequest("GET", get_bitlink_url, queryParameters = query, showURL = showRequestURL)
   df_bitlink <- data.frame(t(do.call(rbind, df_bitlink)), stringsAsFactors = F)
   df_bitlink$created_at <- ymd_hms(df_bitlink$created_at, tz = "UTC")
   
   return(df_bitlink)
}


#' @title Get Metrics for a Bitlink by referrers by domain
#' 
#' @description This endpoint will group referrers metrics about a single Bitlink.
#'
#' @seealso \url{https://dev.bitly.com/v4/#operation/getMetricsForBitlinkByReferrersByDomains}
#' 
#' @inheritParams bitly_user_metrics_referring_domains
#' @import httr jsonlite lubridate
#' @examples
#' \dontrun{
#' bitly_retrieve_metrics_by_referrers_by_domain(bitlink = "bit.ly/DPetrov", unit = "day", units = -1, size = 100)
#' }
#' 
#' @export
bitly_retrieve_metrics_by_referrers_by_domain <- function(bitlink = NULL, size = 50, unit_reference = NULL, unit = NULL, 
                                                units = -1, showRequestURL = FALSE) {

   metrics_referrers_by_domain_url <- paste0("https://api-ssl.bitly.com/v4/bitlinks/", bitlink, "/referrers_by_domains")
   
   query <- list(access_token = bitly_auth_access(), bitlink = bitlink, size = size, 
                 units = units, unit = unit, unit_reference = unit_reference)
   
   df_metrics_referrers_by_domain <- doRequest("GET", metrics_referrers_by_domain_url,
                                                    query, showURL = showRequestURL)
   
   return(df_metrics_referrers_by_domain)
}


#' @title Get Clicks for a Bitlink
#' 
#' @description See \url{https://dev.bitly.com/v4/#operation/getClicksForBitlink}
#' This will return the click counts for a specified Bitlink. This returns an array with clicks based on a date.
#' 
#' @param size - The quantity of items to be be returned
#' @param units - An integer representing the time units to query data for. pass -1 to return all units of time.
#' @param unit - A unit of time
#' @param showRequestURL - show URL which has been build and requested from server. For debug
#' purposes.
#' 
#' @examples
#' \dontrun{
#' bitly_retrieve_clicks(bitlink = "cnn.it/2HomWGB", unit = "day", units = -1, size = 100)
#' }
#' @import httr jsonlite lubridate
#' @export
bitly_retrieve_clicks <- function(bitlink = NULL, size = 50, unit_reference = NULL, unit = NULL, 
                                    units = -1, showRequestURL = FALSE) {

   user_metrics_clicks_url <- paste0("https://api-ssl.bitly.com/v4/bitlinks/", bitlink, "/clicks")
   
   query <- list(access_token = bitly_auth_access(), bitlink = bitlink, unit_reference = unit_reference,
      unit = unit, units = units)
   
   df_user_metrics_clicks <- doRequest("GET", user_metrics_clicks_url, query, showURL = showRequestURL)
   df_user_metrics_clicks$link_clicks$date <- ymd_hms(df_user_metrics_clicks$link_clicks$date, tz = "UTC")
   df_user_metrics_clicks$unit_reference <- ymd_hms(df_user_metrics_clicks$unit_reference, tz = "UTC")
   
   return(df_user_metrics_clicks)
}


#' @title Get Clicks Summary for a Bitlink
#' 
#' @description See \url{https://dev.bitly.com/v4/#operation/getClicksSummaryForBitlink}
#' This will return the click counts for a specified Bitlink. This rolls up all the data into a single field of clicks.
#' 
#' @param size - The quantity of items to be be returned
#' @param units - An integer representing the time units to query data for. pass -1 to return all units of time.
#' @param unit - A unit of time
#' @param showRequestURL - show URL which has been build and requested from server. For debug
#' purposes.
#' 
#' @examples
#' \dontrun{
#' bitly_retrieve_clicks_summary(bitlink = "cnn.it/2HomWGB", unit = "day", units = -1, size = 100)
#' }
#' @import httr jsonlite lubridate
#' @export
bitly_retrieve_clicks_summary <- function(bitlink = NULL, size = 50, unit_reference = NULL, unit = NULL, 
                                  units = -1, showRequestURL = FALSE) {
   
   user_metrics_clicks_url_sum <- paste0("https://api-ssl.bitly.com/v4/bitlinks/", bitlink, "/clicks/summary")
   
   query <- list(access_token = bitly_auth_access(), bitlink = bitlink, unit_reference = unit_reference,
                 unit = unit, units = units, size = size)
   
   df_user_metrics_clicks_sum <- doRequest("GET", user_metrics_clicks_url_sum, query, showURL = showRequestURL)
   df_user_metrics_clicks_sum$unit_reference <- ymd_hms(df_user_metrics_clicks_sum$unit_reference, tz = "UTC")
   
   return(df_user_metrics_clicks_sum)
}


#' @title Get Metrics for a Bitlink by countries 
#' 
#' @description This endpoint will return metrics about the countries referring click traffic to a single Bitlink.
#' 
#' @seealso \url{https://dev.bitly.com/v4/#operation/getMetricsForBitlinkByCountries}
#' 
#' @inheritParams bitly_retrieve_clicks_summary
#' 
#' @import httr jsonlite lubridate
#' 
#' @examples
#' \dontrun{
#' bitly_retrieve_metrics_by_countries(bitlink = "bit.ly/DPetrov", unit = "day", units = -1, size = 100)
#' }
#' 
#' @export
bitly_retrieve_metrics_by_countries <- function(bitlink = NULL, size = 100, unit = NULL, unit_reference = NULL,
                                        units = -1, showRequestURL = FALSE) {
   
   link_metrics_countries_url <- paste0("https://api-ssl.bitly.com/v4/bitlinks/", bitlink, "/countries")
   
   query <- list(access_token = bitly_auth_access(), bitlink = bitlink, unit_reference = unit_reference,
                 unit = unit, units = units, size = size)
   
   df_link_metrics_countries <- doRequest("GET", link_metrics_countries_url, query, showURL = showRequestURL)
   df_link_metrics_countries$unit_reference <- ymd_hms(df_link_metrics_countries$unit_reference, tz = "UTC")
   
   return(df_link_metrics_countries)
}

#' @title Get Metrics for a Bitlink by referrers 
#' 
#' @description This endpoint will return metrics about the referrers referring click traffic to a single Bitlink.
#' 
#' @seealso \url{https://dev.bitly.com/v4/#operation/getMetricsForBitlinkByReferrers}
#' 
#' @inheritParams bitly_retrieve_metrics_by_countries
#' 
#' @import httr jsonlite lubridate
#' 
#' @examples
#' \dontrun{
#' bitly_retrieve_metrics_by_referrers(bitlink = "bit.ly/DPetrov", unit = "day", units = -1, size = 100)
#' }
#' 
#' @export
bitly_retrieve_metrics_by_referrers <- function(bitlink = NULL, size = 100, unit = NULL, unit_reference = NULL,
                                                units = -1, showRequestURL = FALSE) {
   
   link_metrics_countries_url <- paste0("https://api-ssl.bitly.com/v4/bitlinks/", bitlink, "/referrers")
   
   query <- list(access_token = bitly_auth_access(), bitlink = bitlink, unit_reference = unit_reference,
                 unit = unit, units = units, size = size)
   
   df_link_metrics_countries <- doRequest("GET", link_metrics_countries_url, query, showURL = showRequestURL)
   df_link_metrics_countries$unit_reference <- ymd_hms(df_link_metrics_countries$unit_reference, tz = "UTC")
   
   return(df_link_metrics_countries)
}

#' @title Retrieve Bitlinks by Group
#' 
#' @description See \url{https://dev.bitly.com/v4/#operation/getBitlinksByGroup}
#' Retrieve a paginated collection of Bitlinks for a Group
#'
#' @inheritParams bitly_retrieve_metrics_by_referrers
#' @inheritParams bitly_update_bitlink
#' @inheritParams bitly_retrieve_links_grouped
#' 
#' @examples
#' \dontrun{
#' bitly_retrieve_bitlinks_by_groups(group_guid = "bit.ly/DPetrov", keyword = "novy titulek")
#' }
#' @import httr jsonlite lubridate stringi
#' @export
bitly_retrieve_bitlinks_by_groups <- function(group_guid = NULL, size = 50, page = 1, showRequestURL = FALSE, 
                                 keyword = NULL, query_q = NULL, created_before = NULL, created_after = NULL, 
                                 modified_after = NULL, archived = "both", deeplinks = "both", campaign_guid = NULL, 
                                 channel_guid = NULL, custom_bitlink = "both", tags = NULL, encoding_login = NULL,
                                 domain_deeplinks = "both") {
   
    link_by_groups <- paste0("https://api-ssl.bitly.com/v4/groups/", group_guid, "/bitlinks")

    query <- list(access_token = bitly_auth_access(), size = size, page = page,
                  archived = archived, domain_deeplinks = domain_deeplinks,
      deeplinks = deeplinks, custom_bitlink = custom_bitlink)

    if (!length(tags) >= 1) {
      query$tags <- tags
    }

    if (!length(keyword) >= 1) {
      query$keyword <- keyword
    }

    if (!length(query_q) >= 1) {
      query$query_q <- query_q
    }

    if (!length(created_before) >= 1) {
      query$created_before <- created_before
    }

    if (!length(created_after) >= 1) {
      query$created_after <- created_after
    }

    if (!length(modified_after) >= 1) {
      query$modified_after <- modified_after
    }

    if (!length(campaign_guid) >= 1) {
      query$campaign_guid <- campaign_guid
    }

    if (!length(channel_guid) >= 1) {
      query$channel_guid <- channel_guid
    }

    if (!length(encoding_login) >= 1) {
      query$encoding_login <- encoding_login
    }

    df_bitlinks_byGroup <- doRequest("GET", url = link_by_groups, queryParameters = query, showURL = showRequestURL)
    return(df_bitlinks_byGroup)
}


#' @title Retrieve Sorted Bitlinks for Group
#' 
#' @description See \url{https://dev.bitly.com/v4/#operation/getSortedBitlinks}
#' This will retrieve a paginated response for Bitlinks that are sorted for the Group
#'
#' @inheritParams bitly_retrieve_metrics_by_referrers
#' @inheritParams bitly_update_bitlink
#' @inheritParams bitly_retrieve_bitlinks_by_groups
#'
#' @param sort - required, Enum:"clicks" - The type of sorting that you would like to do
#'
#' @examples
#' \dontrun{
#' bitly_retrieve_sorted_bitlinks_by_groups(group_guid = "", sort = "clicks")
#' }
#' @import httr jsonlite lubridate stringi
#' @export
bitly_retrieve_sorted_bitlinks_by_groups <- function(group_guid = NULL, unit = "day", units = -1, sort = "clicks",
                                              size = 50, unit_reference = NULL, showRequestURL = FALSE) {
   
    link_by_sorted_groups <- paste0("https://api-ssl.bitly.com/v4/groups/", group_guid, "/bitlinks/", sort)

    query <- list(access_token = bitly_auth_access(), unit_reference = unit_reference,
                 unit = unit, units = units, size = size)

    df_bitlinks_byGroup <- doRequest("GET", url = link_by_sorted_groups, queryParameters = query, showURL = showRequestURL)
    return(df_bitlinks_byGroup)
}



