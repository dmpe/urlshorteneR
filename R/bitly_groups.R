#' Retrieve Group Preferences
#'
#' Retrieve preferences for a specific group
#'
#' @seealso \url{https://dev.bitly.com/api-reference#getGroupPreferences}
#'
#' @inheritSection bitly_retrieve_group Group
#' @inheritParams bitly_retrieve_sorted_links
#' @param group_id - the group id the user belongs to
#'
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' group_pref <- bitly_retrieve_group_pref(group_id = ui$default_group_guid[1])
#' }
#' @import httr2 jsonlite assertthat
#'
#' @export
bitly_retrieve_group_pref <- function(access_token, group_id = NA, showRequestURL = F) {

  if (is.string(group_id)) {
    gr_pref_url <- paste0("https://api-ssl.bitly.com/v4/groups/", group_id, "/preferences")
  } else {
    stop("group_id must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access())

  df_group_prefs <- doBearerTokenRequest("GET", url = gr_pref_url, queryParameters = query, showURL = showRequestURL)
  df_group_prefs <- data.frame(df_group_prefs, stringsAsFactors = FALSE)

  return(df_group_prefs)
}

#' Update Group Preferences
#'
#' Update preferences for a specific group
#'
#' @seealso \url{https://dev.bitly.com/api-reference#updateGroupPreferences}
#'
#' @inheritSection bitly_retrieve_group Group
#'
#' @inheritParams bitly_retrieve_sorted_links
#' @inheritParams bitly_retrieve_group_pref
#'
#' @param domain_pref - string
#'
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' group_pref <- bitly_update_group_pref(group_id = ui$default_group_guid[1])
#' }
#' @import httr2 jsonlite assertthat
#'
#' @export
bitly_update_group_pref <- function(access_token, group_id = NA, domain_pref = NA, showRequestURL = F) {

  if (is.string(group_id)) {
    gr_pref_url_up <- paste0("https://api-ssl.bitly.com/v4/groups/", group_id, "/preferences")
  } else {
    stop("group_id must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access())
  body_upd <- list(group_guid = group_id, domain_preference = domain_pref)

  df_update_pref <- doBearerTokenRequest("PATCH", url = gr_pref_url_up, queryParameters = query,
                              patch_body = body_upd, showURL = showRequestURL)

  df_update_pref <- data.frame(df_update_pref, stringsAsFactors = FALSE)

  return(df_update_pref)
}

#' Retrieve Bitlinks by Group
#'
#' @description
#' Retrieve a paginated collection of Bitlinks for a Group
#'
#' @param page - Default: 1 | Integer specifying the numbered result at which to start
#' @param keyword - Custom keyword to filter on history entries
#' @param modified_after - Timestamp as an integer unix epoch,
#' see \code{\link[lubridate]{as_datetime}} or \code{anytime}'s \code{as_datetime}
#' @param created_after - Timestamp as an integer unix epoch
#' @param created_before - Timestamp as an integer unix epoch
#' @param search_query - string | the value that you would like to search
#' @param archived - string | Default: "off" | Enum:"on" "off" "both" | Whether or not to include archived bitlinks
#' @param deeplinks - string | Default: "both" | Enum:"on" "off" "both" | Filter to only Bitlinks that contain deeplinks
#' @param domain_deeplinks - string | Default: "both" | Enum:"on" "off" "both" | Filter to only Bitlinks
#' that contain deeplinks configured with a custom domain
#' @param campaign_guid - Filter to return only links for the given campaign GUID, can be provided
#' @param channel_guid - Filter to return only links for the given channel GUID,
#' can be provided, overrides all other parameters
#' @param custom_bitlink - string | Default: "both" | Enum:"on" "off" "both"
#' @param tags - Array of string | filter by given tags
#' @param encoding_login - Array of string | Filter by the login of the authenticated user that created the Bitlink
#'
#' @inheritSection bitly_retrieve_group Group
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' @inheritParams bitly_retrieve_sorted_links
#' @inheritParams bitly_update_group_pref
#' @seealso \url{https://dev.bitly.com/api-reference#getBitlinksByGroup}
#'
#' @import httr2 jsonlite assertthat lubridate
#'
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' rg <- bitly_retrieve_links_grouped(group_id = ui$default_group_guid[1])
#' }
#'
#' @export
bitly_retrieve_links_grouped <- function(access_token, group_id = NA, keyword = NULL, search_query = NULL, created_before = NULL,
                                         created_after = NULL, modified_after = NULL, archived = "off",
                                         deeplinks = "both", domain_deeplinks = "both", campaign_guid = NULL,
                                         channel_guid = NULL, custom_bitlink = "both", tags = NULL,
                                         encoding_login = NULL, page = 1, size = 50, showRequestURL = F) {

  if (is.string(group_id)) {
    grouped_links_url <- paste0("https://api-ssl.bitly.com/v4/groups/", group_id, "/bitlinks")
  } else {
    stop("group_id must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access(), size = size, page = page, keyword = keyword,
                query = search_query, created_before = created_before, created_after = created_after,
                modified_after = modified_after, archived = archived)

  df_grouped_links <- doBearerTokenRequest("GET", url = grouped_links_url, queryParameters = query,
                               showURL = showRequestURL)
  return(df_grouped_links)
}


#' @title Retrieve Tags by Group
#'
#' @description
#' Retrieve the currently used tags for a group
#'
#' @seealso \url{https://dev.bitly.com/api-reference#getGroupTags}
#'
#' @import httr2 jsonlite assertthat
#'
#' @inheritSection bitly_retrieve_group Group
#' @inheritParams bitly_retrieve_links_grouped
#' @inheritParams bitly_user_info
#'
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' rg <- bitly_retrieve_tags(group_id = ui$default_group_guid[1])
#' }
#' @export
bitly_retrieve_tags <- function(access_token, group_id = NA, showRequestURL = F) {

  if (is.string(group_id)) {
    tags_url <- paste0("https://api-ssl.bitly.com/v4/groups/", group_id, "/tags")
  } else {
    stop("group_id must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access())

  df_groups_details <- doBearerTokenRequest("GET", tags_url, query, showURL = showRequestURL)
  df_groups_details <- data.frame(df_groups_details, stringsAsFactors = FALSE)

  return(df_groups_details)
}

#' Get Click Metrics for a Group by countries
#'
#' This endpoint will return metrics about the countries referring click traffic rolled up to a Group
#'
#' @seealso \url{https://dev.bitly.com/api-reference#getGroupMetricsByCountries}
#'
#' @inheritSection bitly_retrieve_group Group
#'
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_links_grouped
#' @import httr2 jsonlite assertthat lubridate
#'
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' rg <- bitly_retrieve_group_click_metrics_by_countries(group_id = ui$default_group_guid[1])
#' }
#' @export
bitly_retrieve_group_click_metrics_by_countries <- function(access_token, group_id = NA, showRequestURL = F) {

  if (is.string(group_id)) {
    metrics_url <- paste0("https://api-ssl.bitly.com/v4/groups/", group_id, "/countries")
  } else {
    stop("group_id must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access())

  df_click_metrics <- doBearerTokenRequest("GET", metrics_url, query, showURL = showRequestURL)
  df_click_metrics <- data.frame(df_click_metrics, stringsAsFactors = FALSE)
  df_click_metrics$unit_reference <- ymd_hms(df_click_metrics$unit_reference, tz = "UTC")

  return(df_click_metrics)
}

#' Get Click Metrics for a specified group by devices (Premium)
#'
#' Returns the device types generating click traffic to the specified group's links.
#' Requires a premium account.
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getGroupMetricsByDevices}
#'
#' @inheritSection bitly_retrieve_group Group
#'
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_links_grouped
#' @import httr2 jsonlite assertthat lubridate
#'
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' rg <- bitly_retrieve_group_click_metrics_by_devices(group_id = ui$default_group_guid[1])
#' }
#' @export
bitly_retrieve_group_click_metrics_by_devices <- function(access_token, group_id = NA, showRequestURL = F) {

  if (is.string(group_id)) {
    metrics_devices_url <- paste0("https://api-ssl.bitly.com/v4/groups/", group_id, "/cities")
  } else {
    stop("group_id must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access())

  df_click_metrics_devices <- doBearerTokenRequest("GET", metrics_devices_url, query, showURL = showRequestURL)
  df_click_metrics_devices <- data.frame(df_click_metrics_devices, stringsAsFactors = FALSE)
  df_click_metrics_devices$unit_reference <- ymd_hms(df_click_metrics_devices$unit_reference, tz = "UTC")

  return(df_click_metrics_devices)
}


#' Get Click Metrics for a specified group by city (Premium)
#'
#' Returns the geographic origins of click traffic by city for the specified group.
#' Requires a premium account.
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getGroupMetricsByCities}
#'
#' @inheritSection bitly_retrieve_group Group
#'
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_links_grouped
#' @import httr2 jsonlite assertthat lubridate
#'
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' rg <- bitly_retrieve_group_click_metrics_by_cities(group_id = ui$default_group_guid[1])
#' }
#' @export
bitly_retrieve_group_click_metrics_by_cities <- function(access_token, group_id = NA, showRequestURL = F) {

  if (is.string(group_id)) {
    metrics_cities_url <- paste0("https://api-ssl.bitly.com/v4/groups/", group_id, "/cities")
  } else {
    stop("group_id must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access())

  df_click_metrics_cities <- doBearerTokenRequest("GET", metrics_cities_url, query, showURL = showRequestURL)
  df_click_metrics_cities <- data.frame(df_click_metrics_cities, stringsAsFactors = FALSE)
  df_click_metrics_cities$unit_reference <- ymd_hms(df_click_metrics_cities$unit_reference, tz = "UTC")

  return(df_click_metrics_cities)
}


#' Get Click Metrics for a Group by referring networks
#'
#' This endpoint will return metrics about the referring network click traffic rolled up to a Group
#'
#' @seealso \url{https://dev.bitly.com/api-reference#GetGroupMetricsByReferringNetworks}
#'
#' @inheritSection bitly_retrieve_group Group
#'
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_links_grouped
#' @import httr2 jsonlite assertthat lubridate
#'
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' rg <- bitly_retrieve_group_click_metrics_by_ref_networks(group_id = ui$default_group_guid[1])
#' }
#' @export
bitly_retrieve_group_click_metrics_by_ref_networks <- function(access_token, group_id = NA, showRequestURL = F) {

  if (is.string(group_id)) {
    metrics_ref_net_url <- paste0("https://api-ssl.bitly.com/v4/groups/", group_id, "/referring_networks")
  } else {
    stop("group_id must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access())

  df_click_metrics_net <- doBearerTokenRequest("GET", metrics_ref_net_url, query, showURL = showRequestURL)

  if(length(df_click_metrics_net$metrics) == 0) {
    stop("Metrics are empty.")
  }

  df_click_metrics_net <- data.frame(df_click_metrics_net, stringsAsFactors = FALSE)
  df_click_metrics_net$unit_reference <- ymd_hms(df_click_metrics_net$unit_reference, tz = "UTC")

  return(df_click_metrics_net)
}

#' Retrieve Group Shorten Counts
#'
#' Get all the shorten counts for a specific group
#'
#' @seealso \url{https://dev.bitly.com/api-reference#getGroupShortenCounts}
#'
#' @inheritSection bitly_retrieve_group Group
#'
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_links_grouped
#' @import httr2 jsonlite assertthat lubridate
#'
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' rg <- bitly_retrieve_group_shorten_counts(group_id = ui$default_group_guid[1])
#' }
#' @export
bitly_retrieve_group_shorten_counts <- function(access_token, group_id = NA, showRequestURL = F) {

  if (is.string(group_id)) {
    gr_short_counts_url <- paste0("https://api-ssl.bitly.com/v4/groups/", group_id, "/shorten_counts")
  } else {
    stop("group_id must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access())

  df_short_co <- doBearerTokenRequest("GET", gr_short_counts_url, query, showURL = showRequestURL)
  df_short_co <- data.frame(df_short_co, stringsAsFactors = FALSE)
  df_short_co$unit_reference <- ymd_hms(df_short_co$unit_reference, tz = "UTC")

  return(df_short_co)
}


#' @title Retrieve a list of all groups
#'
#' @description
#' Retrive details for all groups that a user belongs to.
#'
#' @param organization_id - an optional string parameter | A GUID for a Bitly organization
#'
#' @seealso \url{https://dev.bitly.com/api-reference#getGroups}
#'
#' @import httr2 jsonlite assertthat
#'
#' @inheritSection bitly_retrieve_group Group
#'
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_links_grouped
#' @examples
#' \dontrun{
#' rg <- bitly_retrieve_groups("") # will still work ok
#' }
#' @export
bitly_retrieve_groups <- function(access_token, organization_id = NULL, showRequestURL = F) {
  groups_url <- "https://api-ssl.bitly.com/v4/groups/"

  query <- list(access_token = bitly_auth_access(), organization_guid = organization_id)

  df_groups_details <- doBearerTokenRequest("GET", groups_url, query, showURL = showRequestURL)
  df_groups_details <- data.frame(df_groups_details, stringsAsFactors = FALSE)

  return(df_groups_details)
}


#' Retrieve Sorted Bitlinks for Group
#'
#' @description
#' This will retrieve a paginated response for Bitlinks that are sorted for the Group.
#' This method returns a combined object which end-user (you) have to further process for your needs.
#'
#' @param to_sort_by - a required string | Enum: "clicks" | The type of sorting that you would like to do
#' @param unit - string | Default: "day", Enum: "minute" "hour" "day" "week" "month" | A unit of time
#' @param units - integer | Default: -1 | An integer representing the time units to
#' query data for. pass -1 to return all units of time.
#' @param unit_reference - string | An ISO-8601 timestamp, indicating the most recent time for
#' which to pull metrics. Will default to current time.
#' @param size - string |  Default: 50 | The quantity of items to be be returned
#'
#' @inheritSection bitly_retrieve_group Group
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' @inheritParams bitly_retrieve_links_grouped
#' @seealso \url{https://dev.bitly.com/api-reference#getSortedBitlinks}
#'
#' @import httr2 jsonlite lubridate
#'
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' rg <- bitly_retrieve_sorted_links(group_id = ui$default_group_guid[1])
#' }
#'
#' @export
bitly_retrieve_sorted_links <- function(access_token, group_id = NA, to_sort_by = "clicks", unit = "day",
                                        units = -1, unit_reference = NULL, size = 50, showRequestURL = F) {

  if (is.string(group_id) && is.string(to_sort_by)) {
    sorted_links_group_url <- paste0("https://api-ssl.bitly.com/v4/groups/", group_id, "/bitlinks/", to_sort_by)
  } else {
    stop("group_id and to_sort_by must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access(), unit = unit, units = units,
                unit_reference = unit_reference, size = size)

  df_sorted_links <- doBearerTokenRequest("GET", url = sorted_links_group_url, queryParameters = query,
                               showURL = showRequestURL)
  df_sorted_links2 <- cbind(
    data.frame(df_sorted_links$links, stringsAsFactors = FALSE),
    data.frame(df_sorted_links$sorted_links, stringsAsFactors = FALSE))

  return(df_sorted_links2)
}


#' Update a Group
#'
#' Update the details of a group
#'
#' @seealso \url{https://dev.bitly.com/api-reference#updateGroup}
#'
#' @inheritSection bitly_retrieve_group Group
#'
#' @seealso [bitly_update_user()]
#'
#' @inheritParams bitly_retrieve_sorted_links
#' @inheritParams bitly_update_user
#' @inheritParams bitly_retrieve_groups
#' @inheritParams bitly_retrieve_links_grouped
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' up_group <- bitly_update_group(group_id = ui$default_group_guid[1], name = "New Group Name",
#' organization_id = "asd")
#' }
#' @import httr2 jsonlite assertthat lubridate
#'
#' @export
bitly_update_group <- function(access_token, group_id = NA, name = NA, organization_id = NA, showRequestURL = F) {

  if (is.string(group_id)) {
    upd_group_url <- paste0("https://api-ssl.bitly.com/v4/groups/", group_id)
  } else {
    stop("group_id must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access())
  update_body <- list(name = name, organization_guid = organization_id)

  df_update_pref <- doBearerTokenRequest("PATCH", url = upd_group_url, queryParameters = query,
                              patch_body = update_body, showURL = showRequestURL)
  df_update_pref <- data.frame(t(unlist(df_update_pref)), stringsAsFactors = FALSE)
  df_update_pref$created <- ymd_hms(df_update_pref$created, tz = "UTC")
  df_update_pref$modified <- ymd_hms(df_update_pref$modified, tz = "UTC")

  return(df_update_pref)
}

#' @title Retrieve a single group
#'
#' @description
#' Retrive details for a specific group that a user belongs to.
#'
#' @section Group:
#' Groups are a subdivision within an organization. A user will belong to a group within an organization.
#' Most actions on our API will be on behalf of a group. For example, when you
#' shorten a link, it will be on behalf of a user and a group.
#'
#' @seealso \url{https://dev.bitly.com/api-reference#getGroup}
#'
#' @param group_id - a required string | A GUID for a Bitly group
#'
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_links_grouped
#' @import httr2 jsonlite assertthat
#'
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' rg <- bitly_retrieve_group(group_guid = ui$default_group_guid)
#' }
#'
#' @export
bitly_retrieve_group <- function(access_token, group_id = NA, showRequestURL = F) {

  if (is.string(group_id)) {
    group_url <- paste0("https://api-ssl.bitly.com/v4/groups/", group_id)
  } else {
    stop("group_id must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access(), group_guid = group_id)

  df_group_details <- doBearerTokenRequest("GET", url = group_url, queryParameters = query, showURL = showRequestURL)
  df_group_details <- data.frame(t(unlist(df_group_details)), stringsAsFactors = FALSE)

  return(df_group_details)
}
