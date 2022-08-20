#' @title Retrieve a single Organization
#'
#' @description
#' Retrive details for the specified organization.
#'
#' @section Organizations:
#' Organizations are part of our hierarchy. This is the top level where a group and user will belong.
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getOrganization}
#'
#' @param organization_id - a required string | A GUID for a Bitly organization.
#' You may also simply pass "" (double quotes), but this should be avoided at all costs.
#'
#' @import httr jsonlite assertthat
#'
#' @examples
#' \dontrun{
#' all_orgs <- bitly_retrieve_orgs()
#' ro <- bitly_retrieve_org(organization_id = all_orgs$guid)
#' }
#'
#' @export
bitly_retrieve_org <- function(organization_id = NULL) {
  org_url <- paste0("https://api-ssl.bitly.com/v4/organizations/", organization_id)

  if (!is.string(organization_id)) {
    stop("organization_id must not be empty string, NA or NULL")
  }
  query <- list(access_token = bitly_auth_access(), organization_guid = organization_id)

  df_org_details <- doRequest("GET", url = org_url, queryParameters = query)
  df_org_details <- data.frame(df_org_details, stringsAsFactors = FALSE)

  return(df_org_details)
}


#' @title Retrieve all Organizations
#'
#' @description
#' Retrieve a list of organizations
#'
#' @inheritSection bitly_retrieve_org Organizations
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getOrganizations}
#'
#' @import httr jsonlite
#'
#' @examples
#' \dontrun{
#' ros <- bitly_retrieve_orgs()
#' }
#' @import httr jsonlite
#' @export
bitly_retrieve_orgs <- function() {
  orgs_url <- "https://api-ssl.bitly.com/v4/organizations"

  query <- list(access_token = bitly_auth_access())

  df_orgs_details <- doRequest(verb = "GET", url = orgs_url, queryParameters = query)
  df_orgs_details <- data.frame(df_orgs_details$organizations, stringsAsFactors = FALSE)

  return(df_orgs_details)
}


#' Retrieve Organization Shorten Counts
#'
#' Retrieve all the shorten counts for a specific organization
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getOrganizationShortenCounts}
#'
#' @inheritSection bitly_retrieve_org Organizations
#'
#' @inheritParams bitly_retrieve_org
#'
#' @return facet - Enum: "countries" "referrers" "referrers_by_domain" "referring_domains" "referring_networks" "shorten_counts"
#'
#' @examples
#' \dontrun{
#' all_orgs <- bitly_retrieve_orgs()
#' osc <- bitly_org_shorten_counts(organization_id = all_orgs$guid)
#' df_org_short_counts <- data.frame(osc, stringsAsFactors = FALSE)
#' }
#'
#' @import httr jsonlite assertthat
#' @export
bitly_retrieve_org_shorten_counts <- function(organization_id = NULL) {
  org_short_counts <- paste0("https://api-ssl.bitly.com/v4/organizations/", organization_id, "/shorten_counts")

  if (!is.string(organization_id)) {
    stop("organization_id must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access(), organization_guid = organization_id)

  df_org_short_counts <- doRequest("GET", org_short_counts, queryParameters = query)
  # df_org_short_counts <- data.frame(df_org_short_counts, stringsAsFactors = FALSE)

  return(df_org_short_counts)
}

#' Get Plan Limits
#'
#' Returns all plan limits and counts available for an organization.
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getPlanLimits}
#'
#' @inheritSection bitly_retrieve_org Organizations
#'
#' @inheritParams bitly_retrieve_org
#'
#' @examples
#' \dontrun{
#' all_orgs <- bitly_retrieve_orgs()
#' plan <- bitly_retrieve_org_plan_limits(organization_id = all_orgs$guid)
#' }
#'
#' @import httr jsonlite assertthat
#' @export
bitly_retrieve_org_plan_limits <- function(organization_id = NULL) {
  org_plan_limits <- paste0("https://api-ssl.bitly.com/v4/organizations/", organization_id, "/plan_limits")

  if (!is.string(organization_id)) {
    stop("organization_id must not be empty string, NA or NULL")
  }

  query <- list(access_token = bitly_auth_access(), organization_guid = organization_id)

  df_plan_limits <- doRequest("GET", org_plan_limits, queryParameters = query)
  df_org_plan_limits <- data.frame(df_plan_limits$plan_limits, stringsAsFactors = FALSE)

  return(df_org_plan_limits)
}
