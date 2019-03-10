#' @title Retrieve a Group 
#' 
#' @description Retrive details for a group
#' 
#' @seealso \url{https://dev.bitly.com/v4/#operation/getGroup}
#' 
#' @param group_guid - string | A GUID for a Bitly group
#' 
#' @import httr jsonlite
#' 
#' @examples 
#' \dontrun {
#' ui <- bitly_UserInfo(showRequestURL = TRUE) 
#' rg <- retrieve_group(group_guid = ui$default_group_guid)
#' 
#' }
#' 
#' @export
retrieve_group <- function(group_guid = NA) {
  
  if(is.na(group_guid)) {
    stop("group_guid must not be empty string/NA/NULL")
  } else {
    group_ulr <- paste0("https://api-ssl.bitly.com/v4/groups/", group_guid)
  }
  
  
  
  
  
}