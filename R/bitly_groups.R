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
#' @seealso \url{https://dev.bitly.com/v4/#operation/getGroup}
#'
#' @param group_id - a required string | A GUID for a Bitly group
#' 
#' @inheritParams bitly_user_info
#' 
#' @import httr jsonlite assertthat
#'
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' rg <- bitly_retrieve_group(group_guid = ui$default_group_guid)
#' }
#' 
#' @export
bitly_retrieve_group <- function(group_id = NA, showRequestURL = F) {
  
  if (is.string(group_id)) {
    group_url <- paste0("https://api-ssl.bitly.com/v4/groups/", group_id)
  } else {
    stop("group_id must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access(), group_guid = group_id)

  df_group_details <- doRequest("GET", url = group_url, queryParameters = query, showURL = showRequestURL)
  df_group_details <- data.frame(t(unlist(df_group_details)), stringsAsFactors = FALSE)
  
  return(df_group_details)
}


#' @title Retrieve a list of all groups
#'
#' @description
#' Retrive details for all groups that a user belongs to.
#'
#' @param organization_id - an optional string parameter | A GUID for a Bitly organization
#'
#' @seealso \url{https://dev.bitly.com/v4/#operation/getGroups}
#'
#' @import httr jsonlite assertthat
#'
#' @inheritSection bitly_retrieve_group Group
#' 
#' @inheritParams bitly_user_info
#' 
#' @examples
#' \dontrun{
#' rg <- bitly_retrieve_groups("") # will still work ok
#' }
#' @export
bitly_retrieve_groups <- function(organization_id = NA, showRequestURL = F) {
  groups_url <- "https://api-ssl.bitly.com/v4/groups/"
  
  if (!is.string(organization_id)) {
    stop("organization_id must not be empty string, NA or NULL")
  }
  
  query <- list(access_token = bitly_auth_access(), organization_guid = organization_id)

  df_groups_details <- doRequest("GET", groups_url, query, showURL = showRequestURL)
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
#' 
#' @seealso \url{https://dev.bitly.com/v4/#operation/getSortedBitlinks}
#'
#' @import httr jsonlite lubridate 
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' rg <- bitly_retrieve_sorted_links(group_id = ui$default_group_guid[1])
#' }
#' 
#' @export 
bitly_retrieve_sorted_links <- function(group_id = NA, to_sort_by = "clicks", unit = "day", 
                                        units = -1, unit_reference = NULL, size = 50, showRequestURL = F) {
  
  if (is.string(group_id) && is.string(to_sort_by)) {
    sorted_links_group_url <- paste0("https://api-ssl.bitly.com/v4/groups/", group_id, "/bitlinks/", to_sort_by)
  } else {
    stop("group_id and to_sort_by must not be emptry string, NA or NULL")
  }
  
  query <- list(access_token = bitly_auth_access(), unit = unit, units = units, unit_reference = unit_reference, size = size)
  
  df_sorted_links <- doRequest("GET", url = sorted_links_group_url, queryParameters = query, showURL = showRequestURL)
  df_sorted_links2 <- cbind(
    data.frame(df_sorted_links$links, stringsAsFactors = FALSE), 
    data.frame(df_sorted_links$sorted_links, stringsAsFactors = FALSE))
  
  return(df_sorted_links2)
}

#' Update a Group
#'
#' Update the details of a group
#'
#' @seealso \url{https://dev.bitly.com/v4/#operation/updateGroup}
#' 
#' @inheritParams bitly_retrieve_sorted_links
#' @inheritParams bitly_update_user 
#' @inheritParams bitly_retrieve_groups
#'
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' up_group <- bitly_update_group(group_id = ui$default_group_guid[1], name = "New Group Name", organization_id = "asd")
#' }
#' @import httr jsonlite assetthat
#' @export
bitly_update_group <- function(group_id = NA, name = NA, organization_id = NA, showRequestURL = F) {
  #TODO, maybe as with https://github.com/dmpe/urlshorteneR/blob/master/R/bitly_user_app_info.R#L68
  if (is.string(group_id) { 
    upd_group_url <- paste0("https://api-ssl.bitly.com/v4/groups/", group_id)  
  } else {
    stop("group_id must not be emptry string, NA or NULL")
  }
  
  query <- list(access_token = bitly_auth_access())
  update_body <- list(name = name, organization_guid = organization_id)
  
  df_update_pref <- doRequest("PATCH", url = upd_group_url, queryParameters = query, 
                              patch_body = update_body, showURL = showRequestURL)
  df_update_pref <- data.frame(t(unlist(df_update_pref)), stringsAsFactors = FALSE)
      
  return(df_update_pref)
}

#' Retrieve Group Preferences
#'
#' Retrieve preferences for a specific group
#'
#' @seealso \url{https://dev.bitly.com/v4/#operation/getGroupPreferences}
#' 
#' @inheritParams bitly_retrieve_sorted_links
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' group_pref <- bitly_retrieve_group_pref(group_id = ui$default_group_guid[1])
#' }
#' @import httr jsonlite assetthat
#' @export
bitly_retrieve_group_pref <- function(group_id = NA, showRequestURL = F) {
  if (is.string(group_id) {
    gr_pref_url <- paste0("bitly_https://api-ssl.bitly.com/v4/groups/", group_id, "/preferences")
  } else {
    stop("group_id must not be emptry string, NA or NULL")
  }
  
  query <- list(access_token = bitly_auth_access())
  
  df_group_prefs <- doRequest("GET", url = gr_pref_url, queryParameters = query, showURL = showRequestURL)
  df_group_prefs <- data.frame(df_group_prefs, stringsAsFactors = FALSE)

  retunr(df_group_prefs)
}






