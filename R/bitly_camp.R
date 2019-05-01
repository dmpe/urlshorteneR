#' Create Campaign (Premium)
#'
#' Create a new campaign
#'
#' @section Campaigns:
#' Bitly Campaigns allows you to build and manage omnichannel campaigns.
#'
#' @param group_guid - a GUID for a Bitly group
#' @param channel_guids - a list of strings
#' @param description - description of campaign
#' @param name - its name
#'
#' @seealso \url{https://dev.bitly.com/v4/#operation/createCampaign}
#'
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(key = "206868fc5803d50c8d3aae9e7efb9e8a9c56067f", secret = "54987201d3599fd9a09cc80922394b0f250c7886")
#' cc <- create_campaigns(
#'   group_guid = "testing", showRequestURL = TRUE, channel_guids = list("1", "2", "3"),
#'   description = "description", name = "name"
#' )
#' }
#' 
#' @import httr jsonlite
#'
#' @export
create_campaigns <- function(group_guid = NULL, channel_guids = list(), description = NULL,
                             name = NULL, showRequestURL = T, verbose = T) {
  create_camp <- "https://api-ssl.bitly.com/v4/campaigns"

  query <- list(
    access_token = bitly_auth_access(), group_guid = group_guid,
    channel_guids = channel_guids, description = description, name = name
  )

  df_create_camps <- doRequest("POST", create_camp, query, showURL = showRequestURL, verbose)

  df_create_camps <- data.frame(df_create_camps, stringsAsFactors = FALSE)
  df_create_camps$created <- ymd_hms(df_create_camps$created, tz = "UTC")
  df_create_camps$modified <- ymd_hms(df_create_camps$modified, tz = "UTC")

  return(df_create_camps)
}



#' Retrieve campaigns (Premium)
#'
#' Retrieve the campaigns for the current user
#'
#' @inheritParams create_campaigns
#'
#' @seealso \url{https://dev.bitly.com/v4/#operation/getCampaigns}
#'
#' @examples
#' \dontrun{
#' bitly_token <- bitly_auth(key = "206868fc5803d50c8d3aae9e7efb9e8a9c56067f", secret = "54987201d3599fd9a09cc80922394b0f250c7886")
#' gc <- retrieve_campaigns(group_guid = "testing")
#' }
#' 
#' @import httr jsonlite
#' @export
retrieve_campaigns <- function(group_guid = NULL, showRequestURL = T, verbose = T) {
  get_camp <- "https://api-ssl.bitly.com/v4/campaigns"

  query <- list(access_token = bitly_auth_access(), group_guid = group_guid)

  df_get_camps <- doRequest("GET", get_camp, query, showURL = showRequestURL, verbose)

  df_get_camps <- data.frame(df_get_camps, stringsAsFactors = FALSE)
  df_get_camps$campaigns$created <- ymd_hms(df_get_camps$campaigns$created, tz = "UTC")
  df_get_camps$campaigns$modified <- ymd_hms(df_get_camps$campaigns$modified, tz = "UTC")

  return(df_get_camps$campaigns)
}
