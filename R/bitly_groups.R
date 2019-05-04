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
#' @import httr jsonlite assertthat
#'
#' @examples
#' \dontrun{
#' ui <- bitly_UserInfo(showRequestURL = TRUE)
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
#' @param organization_id - an optional string parameter | A GUID for a Bitly rganization
#'
#' @seealso \url{https://dev.bitly.com/v4/#operation/getGroups}
#'
#' @import httr jsonlite assertthat
#'
#' @inheritSection bitly_retrieve_group Group
#'
#' @examples
#' \dontrun{
#' rg <- bitly_retrieve_groups()
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
  
  return(df_group_details)
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
#' 
#' @seealso \url{https://dev.bitly.com/v4/#operation/getSortedBitlinks}
#'
#' @import httr jsonlite lubridate 
#' @examples
#' \dontrun{
#' ui <- bitly_UserInfo(showRequestURL = TRUE)
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
  
  df_groups_details <- doRequest("GET", url = sorted_links_group_url, queryParameters = query, showURL = showRequestURL)
  df_groups_details2 <- cbind(
    data.frame(df_groups_details$links, stringsAsFactors = FALSE), 
    data.frame(df_groups_details$sorted_links, stringsAsFactors = FALSE))
  
  return(df_groups_details2)
  
}


