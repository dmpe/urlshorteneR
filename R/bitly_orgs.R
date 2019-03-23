#' @title Retrieve an Organization
#'
#' @description
#' Retrive details for an organization
#'
#' @section Organizations:
#' Organizations are part of our hierarchy. This is the top level where a group and user will belong.
#'
#' @seealso \url{https://dev.bitly.com/v4/#operation/getOrganization}
#'
#' @param organization_guid - string | A GUID for a Bitly organization
#'
#' @import httr jsonlite
#'
#' @examples
#' \dontrun{
#' ro <- bitly_retrieve_organization(organization_guid = "")
#' }
#' 
#' @export
bitly_retrieve_organization <- function(organization_guid = NULL) {
  org_url <- paste0("https://api-ssl.bitly.com/v4/organizations/", organization_guid)

  query <- list(access_token = bitly_token$credentials$access_token, organization_guid = organization_guid)

  df_org_details <- doRequest("GET", org_url, query)

  df_org_details <- data.frame(df_org_details, stringsAsFactors = FALSE)
  return(df_org_details)
}


#' @title Retrieve Organizations
#'
#' @description
#' Retrieve a list of organizations
#'
#' @inheritSection bitly_retrieve_organization Organizations
#'
#' @seealso \url{https://dev.bitly.com/v4/#operation/getOrganizations}
#'
#' @import httr jsonlite
#'
#' @examples
#' \dontrun{
#' ros <- bitly_retrieve_organizations()
#' }
#' 
#' @export
bitly_retrieve_organizations <- function() {
  orgs_url <- "https://api-ssl.bitly.com/v4/organizations"

  query <- list(access_token = bitly_token$credentials$access_token)

  df_orgs_details <- doRequest("GET", orgs_url, query)

  df_orgs_details <- data.frame(df_orgs_details, stringsAsFactors = FALSE)
  return(df_orgs_details)
}


#' Retrieve Organization Shorten Counts
#'
#' Retrieve all the shorten counts for a specific organization
#'
#' @seealso \url{https://dev.bitly.com/v4/#operation/getOrganizationShortenCounts}
#' @inheritParams bitly_retrieve_organization
#' @examples
#' \dontrun{
#' osc <- org_shorten_counts()
#' }
#' @export
org_shorten_counts <- function(organization_guid = NULL) {
  org_short_counts <- paste0("https://api-ssl.bitly.com/v4/organizations/", organization_guid, "/shorten_counts")

  query <- list(access_token = bitly_token$credentials$access_token, organization_guid = organization_guid)

  df_org_short_counts <- doRequest("GET", org_short_counts, query)

  df_org_short_counts <- data.frame(df_org_short_counts, stringsAsFactors = FALSE)
  return(df_org_short_counts)
}
