#' @title Retrieve information for the current authenticated user
#'
#' @param showRequestURL - an optional T/F value to whether show URL which has been 
#' build and requested from server. For debug purposes, default FALSE.
#'
#' @seealso \url{https://dev.bitly.com/v4/#operation/getUser}
#'
#' @return login - the specified bitly login or the login of the authenticated user
#' @return name - the user's full/display name
#' @return default_group_guid	- a group to which user belongs
#' @return created - Timestamp for the moment the user signed up (uses \code{\link[lubridate]{ymd_hms}})
#' @return is_active - whether a user profile is active
#' @return modified - Timestamp of the last modification to the user profile (uses \code{\link[lubridate]{ymd_hms}})
#' @return is_sso_user - is Single-Sign-On enabled for the user (PREMIUM FEATURE) ? (\href{https://support.bitly.com/hc/en-us/articles/360001482672-What-is-Single-Sign-On-SSO-}{Bit.ly SSO})
#' @return is_2fa_enabled - is 2 Step verification enabled ? (\href{https://support.bitly.com/hc/en-us/articles/230650187-What-is-2-step-verification-}{Bit.ly 2FA})
#' @return email - user's emails
#'
#' @examples
#' \dontrun{
#'    ui <- bitly_UserInfo(showRequestURL = TRUE)
#' }
#' 
#' @import httr stringr lubridate
#'
#' @export
bitly_UserInfo <- function(showRequestURL = FALSE) {
  user_info_url <- "https://api-ssl.bitly.com/v4/user"

  create_query <- list(access_token = bitly_auth_access())

  df_user_info <- doRequest(verb = "GET", url = user_info_url, queryParameters = create_query, showURL = showRequestURL)

  df_user_info_data <- data.frame(df_user_info, stringsAsFactors = FALSE)

  # convert to readable format - use lubridate parse_date_time
  df_user_info_data$created <- ymd_hms(df_user_info_data$created, tz = "UTC")
  df_user_info_data$modified <- ymd_hms(df_user_info_data$modified, tz = "UTC")

  return(df_user_info_data)
}

#' @title Update your name and/or default group ID
#'
#' @description 
#' This will overwrite your (display) username and/or group ID you belong to.
#'
#' @note Applies only to the authenticated user:
#' Changing group ID is only permitted to premium users. Thus, if you are a "free" user and will try to change your
#' default group id to something else, you will get an error. In that case, only changing display name is permitted.
#'
#' @param default_group_guid - group id to change, see NOTE
#' @param name - username to change
#' 
#' @inheritParams bitly_UserInfo
#' 
#' @seealso \url{https://dev.bitly.com/v4/#operation/updateUser}
#'
#' @examples
#' \dontrun{
#' # this applies only for "free" users
#' uu <- bitly_update_user(name = "Malc")
#' 
#' # if you are premium user, you can additionally adjust your group id
#' uug <- bitly_update_user(name = "Malc", default_group_guid = "TestGroupID")
#' }
#' 
#' @export
bitly_update_user <- function(default_group_guid = NULL, name = "", showRequestURL = FALSE) {
  user_info_url <- "https://api-ssl.bitly.com/v4/user"

  if (!is_bitly_user_premium_holder()) {
    default_group_guid <- NULL
    warning("Your account is not premium. Please report bugs in GitHub if this is not true.")
  }

  query <- list(access_token = bitly_auth_access())
  body <- list(name = name, default_group_guid = default_group_guid)

  df_user_info <- doRequest("PATCH",
    url = user_info_url, queryParameters = query,
    patch_body = body, showURL = showRequestURL
  )

  return(df_user_info)
}

#' Check if authenticated user holds premium account
#'
#' @seealso [bitly_UserInfo()]
#'
#' @export
is_bitly_user_premium_holder <- function() {
  user_profile <- bitly_UserInfo()

  return(user_profile$is_sso_user[[1]])
}

#' Retrieve details for the provided OAuth App client ID
#'
#' @param client_id - The client ID of an OAuth app
#'
#' @export
bitly_app_details <- function(client_id = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8", showRequestURL = F) {
  oauth_app_details <- paste0("https://api-ssl.bitly.com/v4/apps/", client_id)

  query <- list(access_token = bitly_auth_access(), client_id = client_id)

  df_app_details <- doRequest("GET", url = oauth_app_details, queryParameters = query, showURL = showRequestURL)
  df_app_details <- data.frame(df_app_details, stringsAsFactors = FALSE)
  
  return(df_app_details)
}
