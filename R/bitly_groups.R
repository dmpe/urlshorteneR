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
#' @param group_guid - string | A GUID for a Bitly group
#' 
#' @import httr jsonlite
#' 
#' @examples 
#' \dontrun{
#' ui <- bitly_UserInfo(showRequestURL = TRUE) 
#' rg <- bitly_retrieve_group(group_guid = ui$default_group_guid)
#' }
#' 
#' @export
bitly_retrieve_group <- function(group_guid = NA, showRequestURL = T) {
  if(is.character(group_guid)) {
    group_ulr <- paste0("https://api-ssl.bitly.com/v4/groups/", group_guid)
  } else {
    if(is.na(group_guid) || is.null(group_guid) || group_guid == "" || is.numeric(group_guid)) {
      stop("group_guid must not be empty string, NA or NULL")
    }
  }
  
  query <- list(access_token = bitly_token$credentials$access_token, group_guid = group_guid)
  
  df_group_details <- doRequest("GET", group_ulr, query, showURL = showRequestURL)
  
  df_group_details <- data.frame(df_group_details, stringsAsFactors = FALSE)
  return(df_group_details)
}


#' @title Retrieve a list of Groups
#' 
#' @description 
#' Retrive details for all groups that a user belongs to.
#' 
#' @param organization_guid
#' 
#' @seealso \url{https://dev.bitly.com/v4/#operation/getGroups}
#' 
#' @import httr jsonlite
#' 
#' @inheritSection bitly_retrieve_group Group
#'
#' @examples 
#' \dontrun{
#' rg <- bitly_retrieve_groups()
#' }
#' @export
bitly_retrieve_groups <- function(organization_guid = NA) {
  groups_url <- "https://api-ssl.bitly.com/v4/groups/"
  
  query <- list(access_token = bitly_token$credentials$access_token, organization_guid = organization_guid)
  
  df_groups_details <- doRequest("GET", groups_url, query, showURL = showRequestURL)
  
  df_groups_details <- data.frame(df_groups_details, stringsAsFactors = FALSE)
  return(df_group_details)
}