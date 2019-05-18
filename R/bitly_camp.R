#' Create Campaign (Premium)
#'
#' Create a new campaign
#'
#' @note It has not been tested, see readme file on GitHub \url{https://github.com/dmpe/urlshorteneR/}
#'
#' @section Campaigns:
#' Bitly Campaigns allows you to build and manage omnichannel campaigns.
#'
#' @param group_guid - a GUID for a Bitly group
#' @param channel_guids - a list of strings
#' @param description - description of campaign
#' @param name - its name
#' 
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' 
#' @seealso \url{https://dev.bitly.com/v4/#operation/createCampaign}
#'
#' @examples
#' \dontrun{
#' cc <- bitly_create_campaigns(
#'   group_guid = "testing", showRequestURL = TRUE, channel_guids = list("1", "2", "3"),
#'   description = "description", name = "name"
#' )
#' }
#' 
#' @import httr jsonlite lubridate
#'
#' @export
bitly_create_campaigns <- function(group_guid = NULL, channel_guids = NULL, description = NULL,
                             name = NULL, showRequestURL = T) {
  create_camp <- "https://api-ssl.bitly.com/v4/campaigns"
 
  body_req_query <- list(access_token = bitly_auth_access(), group_guid = group_guid,
    channel_guids = channel_guids, description = description, name = name
  )

  df_create_camps <- doRequest("POST", create_camp, queryParameters = body_req_query, 
                               showURL = showRequestURL)

  df_create_camps <- data.frame(df_create_camps, stringsAsFactors = FALSE)
  df_create_camps$created <- ymd_hms(df_create_camps$created, tz = "UTC")
  df_create_camps$modified <- ymd_hms(df_create_camps$modified, tz = "UTC")

  return(df_create_camps)
}

#' Retrieve campaigns (Premium)
#' @note It has not been tested, see readme file on GitHub \url{https://github.com/dmpe/urlshorteneR/}
#' Retrieve the campaigns for the current user
#'
#' @inheritParams bitly_create_campaigns
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' @inheritSection bitly_create_campaigns Campaigns
#' 
#' @seealso \url{https://dev.bitly.com/v4/#operation/getCampaigns}
#'
#' @examples
#' \dontrun{
#'   gc <- bitly_retrieve_campaigns(group_guid = "testing")
#' }
#' 
#' @import httr jsonlite lubridate
#' @export
bitly_retrieve_campaigns <- function(group_guid = NULL, showRequestURL = T) {
  get_camp <- "https://api-ssl.bitly.com/v4/campaigns"

  query <- list(access_token = bitly_auth_access(), group_guid = group_guid)

  df_get_camps <- doRequest("GET", get_camp, queryParameters = query, showURL = showRequestURL)

  df_get_camps <- data.frame(df_get_camps$campaigns, stringsAsFactors = FALSE)
  df_get_camps$created <- ymd_hms(df_get_camps$created, tz = "UTC")
  df_get_camps$modified <- ymd_hms(df_get_camps$modified, tz = "UTC")

  return(df_get_camps)
}


#' Create channel (Premium)
#'
#' Create a new channel
#' @note It has not been tested, see readme file on GitHub \url{https://github.com/dmpe/urlshorteneR/}
#' @inheritParams bitly_retrieve_campaigns
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' @inheritSection bitly_create_campaigns Campaigns
#' 
#' @param modified - string | ISO_TIMESTAMP
#' @param created	- string | ISO TIMESTAMP
#' @param campaign_guid - string | A GUID for a Bitly campaign
#' 
#' @seealso \url{https://dev.bitly.com/v4/#operation/createChannel}
#'
#' @examples
#' \dontrun{
#'   gc <- bitly_create_channel(group_guid = "testing", ...)
#' }
#' 
#' @import httr jsonlite lubridate
#' @export
bitly_create_channel <- function(group_guid = NULL, guid = NULL, name = NULL, modified = NULL,
                                 created = NULL, campaign_guid = NULL, bitlink_id = NULL,
                                 showRequestURL = T) {
  
  create_channel <- "https://api-ssl.bitly.com/v4/channels"
  
  bitlinks <- list(bitlink_id = bitlink_id, campaign_guid = campaign_guid)
  body_req_query <- list(access_token = bitly_auth_access(), group_guid = group_guid, 
                         created = created, bitlinks = bitlinks,
                         guid = guid, modified = modified, name = name
  )
  
  df_create_channel <- doRequest("POST", create_channel, queryParameters = body_req_query, 
                               showURL = showRequestURL)
  
  df_create_channel <- data.frame(df_create_channel, stringsAsFactors = FALSE)
  df_create_channel$created <- ymd_hms(df_create_channel$created, tz = "UTC")
  df_create_channel$modified <- ymd_hms(df_create_channel$modified, tz = "UTC")
  
  return(df_create_channel)
}


#' Retrieve channels (Premium)
#' 
#' @note It has not been tested, see readme file on GitHub \url{https://github.com/dmpe/urlshorteneR/}
#' Retrieve the channels available to a user
#' 
#' @inheritParams bitly_create_channel
#' @inheritParams bitly_create_campaigns
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' @inheritSection bitly_create_campaigns Campaigns
#' 
#' @seealso \url{https://dev.bitly.com/v4/#operation/getChannels}
#'
#' @examples
#' \dontrun{
#'   gc <- bitly_retrieve_channels(group_guid = "testing", campaign_guid = "test")
#' }
#' 
#' @import httr jsonlite lubridate
#' @export
bitly_retrieve_channels <- function(group_guid = NULL, campaign_guid = NULL, showRequestURL = T) {
  get_channels <- "https://api-ssl.bitly.com/v4/channels"
  
  query <- list(access_token = bitly_auth_access(), group_guid = group_guid, campaign_guid = campaign_guid)
  
  df_get_channels <- doRequest("GET", get_channels, queryParameters = query, showURL = showRequestURL)
  
  df_get_channels <- data.frame(df_get_channels$channels, stringsAsFactors = FALSE)
  df_get_channels$created <- ymd_hms(df_get_channels$created, tz = "UTC")
  df_get_channels$modified <- ymd_hms(df_get_channels$modified, tz = "UTC")
  
  return(df_get_channels)
}

#' Retrieve a Campaign 
#' 
#' @note It has not been tested, see readme file on GitHub \url{https://github.com/dmpe/urlshorteneR/}
#' Retrive details for a campaign
#'
#' @inheritParams bitly_create_channel
#' @inheritParams bitly_create_campaigns
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' @inheritSection bitly_create_campaigns Campaigns
#' 
#' @seealso \url{https://dev.bitly.com/v4/#operation/getCampaign}
#'
#' @examples
#' \dontrun{
#'   gc <- bitly_retrieve_campaign(campaign_guid = "testing")
#' }
#' 
#' @import httr jsonlite lubridate
#' @export
bitly_retrieve_campaign <- function(campaign_guid = NULL, showRequestURL = T) {
  get_camp <- paste0("https://api-ssl.bitly.com/v4/campaigns/", campaign_guid)
  
  query <- list(access_token = bitly_auth_access(), campaign_guid = campaign_guid)
  
  df_get_camp <- doRequest("GET", get_camp, queryParameters = query, showURL = showRequestURL)
  
  df_get_camp <- data.frame(df_get_camp, stringsAsFactors = FALSE)
  df_get_camp$created <- ymd_hms(df_get_camp$created, tz = "UTC")
  df_get_camp$modified <- ymd_hms(df_get_camp$modified, tz = "UTC")
  
  return(df_get_camp)
}

#' Get a Channel (Premium)
#' 
#' @note It has not been tested, see readme file on GitHub \url{https://github.com/dmpe/urlshorteneR/}
#' Get a channel's details
#' 
#' @inheritParams bitly_create_channel
#' @inheritParams bitly_create_campaigns
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' @inheritSection bitly_create_campaigns Campaigns
#' 
#' @seealso \url{https://dev.bitly.com/v4/#operation/getChannel}
#'
#' @examples
#' \dontrun{
#'   gc <- bitly_retrieve_channel(channel_guid = "testing")
#' }
#' 
#' @import httr jsonlite lubridate
#' @export
bitly_retrieve_channel <- function(channel_guid = NULL, showRequestURL = T) {
  get_channel <- paste0("https://api-ssl.bitly.com/v4/channels/", channel_guid)
  
  query <- list(access_token = bitly_auth_access(), channel_guid = channel_guid)
  
  df_get_channel <- doRequest("GET", get_channel, queryParameters = query, showURL = showRequestURL)
  
  df_get_channel <- data.frame(df_get_channel, stringsAsFactors = FALSE)
  df_get_channel$created <- ymd_hms(df_get_channel$created, tz = "UTC")
  df_get_channel$modified <- ymd_hms(df_get_channel$modified, tz = "UTC")
  
  return(df_get_channel)
}


#' Update A Channel (Premium)
#'
#' Update an existing Channel
#'
#' @note It has not been tested, see readme file on GitHub \url{https://github.com/dmpe/urlshorteneR/}
#' @inheritSection bitly_create_campaigns Campaigns
#' 
#' @inheritParams bitly_create_channel
#' @inheritParams bitly_create_campaigns
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' 
#' @seealso \url{https://dev.bitly.com/v4/#operation/updateCampaign}
#'
#' @examples
#' \dontrun{
#' cc <- bitly_update_campaign(
#'   channel_guid = "testing", group_guid = "", channel_guids = list("1", "2", "3"),
#'   description = "description", name = "name"
#' )
#' }
#' 
#' @import httr jsonlite lubridate
#'
#' @export
bitly_update_campaign <- function(campaign_guid = NULL, group_guid = NULL, channel_guids = NULL, description = NULL,
                                   name = NULL, showRequestURL = T) {
  update_campaign <- paste0("https://api-ssl.bitly.com/v4/campaigns", campaign_guid)

  query <- list(access_token = bitly_auth_access())
  body_req_query <- list(group_guid = group_guid,
                         channel_guids = channel_guids, description = description, name = name
  )
  
  df_update_camps <- doRequest("PATCH", update_campaign, queryParameters = query, patch_body = body_req_query, 
                               showURL = showRequestURL)
  
  df_update_camps <- data.frame(df_update_camps, stringsAsFactors = FALSE)
  df_update_camps$created <- ymd_hms(df_update_camps$created, tz = "UTC")
  df_update_camps$modified <- ymd_hms(df_update_camps$modified, tz = "UTC")
  
  return(df_update_camps)
}

#' Update A Channel (Premium)
#'
#' Update an existing Channel
#'
#' @note It has not been tested, see readme file on GitHub \url{https://github.com/dmpe/urlshorteneR/}
#' @inheritSection bitly_create_campaigns Campaigns
#' 
#' @inheritParams bitly_create_channel
#' @inheritParams bitly_create_campaigns
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' 
#' @seealso \url{https://dev.bitly.com/v4/#operation/updateChannel}
#'
#' @examples
#' \dontrun{
#' cc <- bitly_update_channel(
#'   channel_guid = "testing", group_guid = "", channel_guids = list("1", "2", "3"),
#'   description = "description", name = "name"
#' )
#' }
#' 
#' @import httr jsonlite lubridate
#'
#' @export
bitly_update_channel <- function(channel_guid = NULL, group_guid = NULL, guid = NULL, name = NULL, modified = NULL,
                                  created = NULL, campaign_guid = NULL, bitlink_id = NULL,
                                  showRequestURL = T) {
  update_channels <- paste0("https://api-ssl.bitly.com/v4/channels", channel_guid)
  
  query <- list(access_token = bitly_auth_access())
  bitlinks <- list(bitlink_id = bitlink_id, campaign_guid = campaign_guid)
  
  body_req_query <- list(group_guid = group_guid, guid = guid, modified = modified,
                         name = name, created = created, bitlinks = bitlinks, 
  )
  
  df_update_camp <- doRequest("PATCH", update_channels, queryParameters = query, patch_body = body_req_query, 
                               showURL = showRequestURL)
  
  df_update_camp <- data.frame(df_update_camp, stringsAsFactors = FALSE)
  df_update_camp$created <- ymd_hms(df_update_camp$created, tz = "UTC")
  df_update_camp$modified <- ymd_hms(df_update_camp$modified, tz = "UTC")
  
  return(df_update_camp)
}





