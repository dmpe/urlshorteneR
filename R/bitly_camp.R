#' Create Campaign (Premium)
#'
#' @description
#' Create a new campaign
#'
#' @section Campaigns:
#' Bit.ly Campaigns allows you to build and manage omnichannel campaigns.
#'
#' @param group_guid - a GUID for a Bitly group
#' @param channel_guids - a list of strings
#' @param description - description of campaign
#' @param name - its name
#'
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' @inheritParams bitly_retrieve_bitlinks_by_groups
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#createCampaign}
#'
#' @examples
#' \dontrun{
#' cc <- bitly_create_campaigns(
#'   group_guid = "testing", showRequestURL = TRUE, channel_guids = list("1", "2", "3"),
#'   description = "description", name = "name"
#' )
#' }
#'
#' @import httr2 jsonlite lubridate
#'
#' @export
bitly_create_campaigns <- function(group_guid = NULL, channel_guids = NULL, description = NULL,
                                   name = NULL, showRequestURL = T) {
  create_camp <- "https://api-ssl.bitly.com/v4/campaigns"

  body_req_query <- list(
    group_guid = group_guid,
    channel_guids = channel_guids, description = description, name = name
  )

  df_create_camps <- doBearerTokenRequest("POST", create_camp,
    queryParameters = body_req_query, access_token = Sys.getenv("bitly_access_token"),
    showURL = showRequestURL
  )

  df_create_camps <- data.frame(df_create_camps)
  df_create_camps$created <- ymd_hms(df_create_camps$created, tz = "UTC")
  df_create_camps$modified <- ymd_hms(df_create_camps$modified, tz = "UTC")

  return(df_create_camps)
}

#' Retrieve campaigns (Premium)
#'
#' @description
#' Retrieve the campaigns for the current user
#'
#' @inheritParams bitly_create_campaigns
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' @inheritSection bitly_create_campaigns Campaigns
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getCampaigns}
#'
#' @examples
#' \dontrun{
#' gc <- bitly_retrieve_campaigns(group_guid = "testing")
#' }
#'
#' @import httr2 jsonlite lubridate
#' @export
bitly_retrieve_campaigns <- function(group_guid = NULL, showRequestURL = T) {
  get_camp <- "https://api-ssl.bitly.com/v4/campaigns"

  query <- list(group_guid = group_guid)

  df_get_camps <- doBearerTokenRequest("GET", get_camp, access_token = Sys.getenv("bitly_access_token"), queryParameters = query, showURL = showRequestURL)

  df_get_camps <- data.frame(df_get_camps$campaigns)
  df_get_camps$created <- ymd_hms(df_get_camps$created, tz = "UTC")
  df_get_camps$modified <- ymd_hms(df_get_camps$modified, tz = "UTC")

  return(df_get_camps)
}


#' Create channel (Premium)
#'
#' @description
#' Create a new channel
#'
#' @inheritParams bitly_retrieve_campaigns
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' @inheritParams bitly_create_campaigns
#' @inheritParams bitly_add_cust_bitlink
#' @inheritSection bitly_create_campaigns Campaigns
#'
#' @param modified - string | ISO_TIMESTAMP
#' @param created	- string | ISO TIMESTAMP
#' @param campaign_guid - string | A GUID for a Bitly campaign
#' @param guid - ID for a channel
#' @seealso \url{https://dev.bitly.com/api-reference/#createChannel}
#'
#' @examples
#' \dontrun{
#' gc <- bitly_create_channel(group_guid = "testing", ...)
#' }
#'
#' @import httr2 jsonlite lubridate
#' @export
bitly_create_channel <- function(group_guid = NULL, guid = NULL, name = NULL, modified = NULL,
                                 created = NULL, campaign_guid = NULL, bitlink_id = NULL,
                                 showRequestURL = T) {
  create_channel <- "https://api-ssl.bitly.com/v4/channels"

  bitlinks <- list(bitlink_id = bitlink_id, campaign_guid = campaign_guid)
  body_req_query <- list(
    group_guid = group_guid,
    created = created, bitlinks = bitlinks,
    guid = guid, modified = modified, name = name
  )

  df_create_channel <- doBearerTokenRequest("POST", create_channel,
    access_token = Sys.getenv("bitly_access_token"), queryParameters = body_req_query,
    showURL = showRequestURL
  )

  df_create_channel <- data.frame(df_create_channel)
  df_create_channel$created <- ymd_hms(df_create_channel$created, tz = "UTC")
  df_create_channel$modified <- ymd_hms(df_create_channel$modified, tz = "UTC")

  return(df_create_channel)
}


#' Retrieve channels (Premium)
#'
#' @description
#' Retrieve the channels available to a user
#'
#' @inheritParams bitly_create_channel
#' @inheritParams bitly_create_campaigns
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' @inheritSection bitly_create_campaigns Campaigns
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getChannels}
#'
#' @examples
#' \dontrun{
#' gc <- bitly_retrieve_channels(group_guid = "testing", campaign_guid = "test")
#' }
#'
#' @import httr2 jsonlite lubridate
#' @export
bitly_retrieve_channels <- function(group_guid = NULL, campaign_guid = NULL, showRequestURL = T) {
  get_channels <- "https://api-ssl.bitly.com/v4/channels"

  query <- list(group_guid = group_guid, campaign_guid = campaign_guid)

  df_get_channels <- doBearerTokenRequest("GET", get_channels, access_token = Sys.getenv("bitly_access_token"), queryParameters = query, showURL = showRequestURL)

  df_get_channels <- data.frame(df_get_channels$channels)
  df_get_channels$created <- ymd_hms(df_get_channels$created, tz = "UTC")
  df_get_channels$modified <- ymd_hms(df_get_channels$modified, tz = "UTC")

  return(df_get_channels)
}

#' Retrieve a Campaign
#'
#' @description
#' Retrive details for a campaign
#'
#' @inheritParams bitly_create_channel
#' @inheritParams bitly_create_campaigns
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' @inheritSection bitly_create_campaigns Campaigns
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getCampaign}
#'
#' @examples
#' \dontrun{
#' gc <- bitly_retrieve_campaign(campaign_guid = "testing")
#' }
#'
#' @import httr2 jsonlite lubridate
#' @export
bitly_retrieve_campaign <- function(campaign_guid = NULL, showRequestURL = T) {
  get_camp <- paste0("https://api-ssl.bitly.com/v4/campaigns/", campaign_guid)

  query <- list(campaign_guid = campaign_guid)

  df_get_camp <- doBearerTokenRequest("GET", get_camp, access_token = Sys.getenv("bitly_access_token"), queryParameters = query, showURL = showRequestURL)

  df_get_camp <- data.frame(df_get_camp)
  df_get_camp$created <- ymd_hms(df_get_camp$created, tz = "UTC")
  df_get_camp$modified <- ymd_hms(df_get_camp$modified, tz = "UTC")

  return(df_get_camp)
}

#' Get a Channel (Premium)
#'
#' @description
#' Get a channel's details
#'
#' @inheritParams bitly_create_channel
#' @inheritParams bitly_create_campaigns
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' @inheritSection bitly_create_campaigns Campaigns
#' @inheritParams bitly_retrieve_bitlinks_by_groups
#' @param channel_guid - GUID of a target channel
#' @seealso \url{https://dev.bitly.com/api-reference/#getChannel}
#'
#' @examples
#' \dontrun{
#' gc <- bitly_retrieve_channel(channel_guid = "testing")
#' }
#'
#' @import httr2 jsonlite lubridate
#' @export
bitly_retrieve_channel <- function(channel_guid = NULL, showRequestURL = T) {
  get_channel <- paste0("https://api-ssl.bitly.com/v4/channels/", channel_guid)

  query <- list(channel_guid = channel_guid)

  df_get_channel <- doBearerTokenRequest("GET", get_channel, access_token = Sys.getenv("bitly_access_token"), queryParameters = query, showURL = showRequestURL)

  df_get_channel <- data.frame(df_get_channel)
  df_get_channel$created <- ymd_hms(df_get_channel$created, tz = "UTC")
  df_get_channel$modified <- ymd_hms(df_get_channel$modified, tz = "UTC")

  return(df_get_channel)
}


#' Update A Channel (Premium)
#'
#' @description
#' Update an existing Channel
#'
#' @inheritSection bitly_create_campaigns Campaigns
#'
#' @inheritParams bitly_create_channel
#' @inheritParams bitly_create_campaigns
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' @inheritParams bitly_create_channel
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#updateCampaign}
#'
#' @examples
#' \dontrun{
#' cc <- bitly_update_campaign(
#'   group_guid = "", channel_guids = list("1", "2", "3"),
#'   description = "description", name = "name"
#' )
#' }
#'
#' @import httr2 jsonlite lubridate
#'
#' @export
bitly_update_campaign <- function(campaign_guid = NULL, group_guid = NULL, channel_guids = NULL, description = NULL,
                                  name = NULL, showRequestURL = T) {
  update_campaign <- paste0("https://api-ssl.bitly.com/v4/campaigns", campaign_guid)

  body_req_query <- list(
    group_guid = group_guid,
    channel_guids = channel_guids, description = description, name = name
  )

  df_update_camps <- doBearerTokenRequest("PATCH", update_campaign,
    access_token = Sys.getenv("bitly_access_token"), patch_body = body_req_query,
    showURL = showRequestURL
  )

  df_update_camps <- data.frame(df_update_camps)
  df_update_camps$created <- ymd_hms(df_update_camps$created, tz = "UTC")
  df_update_camps$modified <- ymd_hms(df_update_camps$modified, tz = "UTC")

  return(df_update_camps)
}

#' Update A Channel (Premium)
#'
#' @description
#' Update an existing Channel
#'
#' @inheritSection bitly_create_campaigns Campaigns
#'
#' @inheritParams bitly_create_channel
#' @inheritParams bitly_create_campaigns
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_group
#' @inheritParams bitly_retrieve_bitlinks_by_groups
#' @inheritParams bitly_retrieve_channel
#' @seealso \url{https://dev.bitly.com/api-reference/#updateChannel}
#'
#' @examples
#' \dontrun{
#' uc <- bitly_update_channel(channel_guid = "testing", group_guid = "", name = "name")
#' }
#'
#' @import httr2 jsonlite lubridate
#'
#' @export
bitly_update_channel <- function(channel_guid = NULL, group_guid = NULL, guid = NULL, name = NULL, modified = NULL,
                                 created = NULL, campaign_guid = NULL, bitlink_id = NULL,
                                 showRequestURL = T) {
  update_channels <- paste0("https://api-ssl.bitly.com/v4/channels", channel_guid)

  body_req_query <- list(
    group_guid = group_guid, guid = guid, modified = modified,
    name = name, created = created, bitlinks = list(bitlink_id = bitlink_id, campaign_guid = campaign_guid)
  )

  df_update_camp <- doBearerTokenRequest("PATCH", update_channels,
    access_token = Sys.getenv("bitly_access_token"), patch_body = body_req_query,
    showURL = showRequestURL
  )

  df_update_camp <- data.frame(df_update_camp)
  df_update_camp$created <- ymd_hms(df_update_camp$created, tz = "UTC")
  df_update_camp$modified <- ymd_hms(df_update_camp$modified, tz = "UTC")

  return(df_update_camp)
}
