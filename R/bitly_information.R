#' @title Retrieve information for the current authenticated user
#' 
#' @param showRequestURL - show URL which has been build and requested from server. 
#' For debug purposes.
#'
#' @description See \url{https://dev.bitly.com/v4/#operation/getUser}
#'
#' @return login - the specified bitly login or the login of the authenticated user.
#' @return name - the user's full/display name
#' @return default_group_guid	
#' @return created - Timestamp for the moment the user signed up.
#' @return is_created
#' @return modified
#' @return is_sso_user
#' @return is_2fa_enabled
#' @return email 
#' 
#' @note Both returned columns (!) are character type.
#'
#' @examples 
#' \dontrun{ 
#'      bitly_token <- bitly_auth()
#'      ui <- bitly_UserInfo(showRequestURL = TRUE) 
#' }
#' 
#' @import stringr
#' 
#' @export
bitly_UserInfo <- function(showRequestURL = FALSE) {
  
  user_info_url <- "https://api-ssl.bitly.com/v4/user"
  
  query <- list(access_token = bitly_token$credentials$access_token)
  
  df_user_info <- doRequest("GET", user_info_url, query, showURL = showRequestURL)
  
  df_user_info_data <- data.frame(df_user_info, stringsAsFactors = FALSE)

  # convert to readable format - use lubridate parse_date_time
  df_user_info_data[5, 1] <- as.character(as.POSIXct(as.integer(df_user_info_data[5, 1]), 
                                                     origin = "1970-01-01", tz = "UTC"))
  
  return(df_user_info_data)
}

#' @title Changing your name or fetching basic user information
#' 
#' @note Applies only to the authenticated user.
#' 
#' @param default_group_guid
#' @param name
#'
#' @export 
update_user <- function(default_group_guid = "" , name = "", showRequestURL = FALSE) {
  user_info_url <- "https://api-ssl.bitly.com/v4/user"
  
  query <- list(access_token = bitly_token$credentials$access_token)
  
  df_user_info <- doRequest("PATCH", user_info_url, query, showURL = showRequestURL)

  return(df_user_info)
}

