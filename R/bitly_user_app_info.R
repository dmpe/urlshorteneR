#' @title Retrieve information for the current authenticated user
#'
#' @param showRequestURL - an optional T/F value to whether show URL which has been
#' build and requested from server. For debug purposes, default FALSE.
#'
#' @section User:
#' User operations such as changing your name or fetching basic user information apply only to the authenticated user.
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getUser}
#'
#' @return login - the specified bitly login or the login of the authenticated user
#' @return name - the user's full/display name
#' @return default_group_guid	- a group to which user belongs
#' @return created - Timestamp for the moment the user signed up (uses \code{\link[lubridate]{ymd_hms}})
#' @return is_active - whether a user profile is active
#' @return modified - Timestamp of the last modification to the user profile (uses \code{\link[lubridate]{ymd_hms}})
#' @return is_sso_user - is Single-Sign-On enabled for the user (PREMIUM FEATURE)
#' @return is_2fa_enabled - is 2 Step verification enabled ?
#' @return email - user's emails
#'
#' @examples
#' \dontrun{
#'    ui <- bitly_user_info(showRequestURL = TRUE)
#' }
#'
#' @import httr2 stringr lubridate
#' @export
bitly_user_info <- function(access_token, showRequestURL = FALSE) {
  user_info_url <- "https://api-ssl.bitly.com/v4/user"

  df_user_info <- doBitlyRequest(verb = "GET", url = user_info_url, access_token = access_token, queryParameters = create_query, showURL = showRequestURL)

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
#' Changing group/org ID is only permitted to premium users. Thus, if you are a "free" user and will try to change your
#' default group id to something else, you will get an error. In that case, only changing display name is permitted.
#'
#' @param default_group_guid - group id to change, see NOTE
#' @param name - username to change
#'
#' @inheritParams bitly_user_info
#' @inheritSection bitly_user_info User
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#updateUser}
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
#' @import httr2 jsonlite lubridate
#'
#' @export
bitly_update_user <- function(access_token, default_group_guid = NULL, name = "", showRequestURL = FALSE) {
  user_info_url <- "https://api-ssl.bitly.com/v4/user"

  if (!is_bitly_user_premium_holder()) {
    default_group_guid <- NULL
    warning("Your account is not premium. Please report bugs in GitHub if this is not true.",
            "We will now skip changing group guid.")
  }

  body <- list(name = name, default_group_guid = default_group_guid)

  df_user_info <- doBitlyRequest("PATCH",
    url = user_info_url, queryParameters = query,
    access_token = access_token,
    patch_body = body, showURL = showRequestURL
  )
  df_user_info$created <- ymd_hms(df_user_info$created, tz = "UTC")
  df_user_info$modified <- ymd_hms(df_user_info$modified, tz = "UTC")

  return(df_user_info)
}

#' Check if authenticated user holds premium account
#'
#' @seealso [bitly_user_info()]
#'
#' @export
is_bitly_user_premium_holder <- function() {
  user_profile <- bitly_user_info()

  return(user_profile$is_sso_user[[1]])
}

#' Retrieve OAuth App
#'
#' Retrieve details for the provided OAuth App client ID
#'
#' @param client_id - The client ID of an OAuth app
#'
#' @inheritParams bitly_user_info
#'
#' @export
bitly_app_details <- function(access_token, client_id = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8", showRequestURL = F) {
  oauth_app_details <- paste0("https://api-ssl.bitly.com/v4/apps/", client_id)

  query <- list(client_id = client_id)

  df_app_details <- doBitlyRequest("GET", url = oauth_app_details, access_token = access_token, queryParameters = query, showURL = showRequestURL)
  df_app_details <- data.frame(df_app_details, stringsAsFactors = FALSE)

  return(df_app_details)
}

#' @title bitly_rate_limits
#' @description Provides bit.ly rate limits by endpoint. See \url{https://dev.bitly.com/api-reference/#getPlatformLimitss}
#' @inheritParams bitly_user_info
#' @return \code{data.frame} of end points and their rate limits by action
#' @export

bitly_rate_limits <- function(access_token, showRequestURL = F) {
  platform_lmt <- "https://api-ssl.bitly.com/v4/user/platform_limits"

  limits <- doBitlyRequest("GET",
                      url = platform_lmt,
                      access_token = access_token, 
                      showURL = showRequestURL)

  return(limits[[1]])
}
