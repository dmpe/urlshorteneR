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
#' @import httr2 jsonlite assertthat
#'
#' @examples
#' \dontrun{
#' bitly_bearerToken("access token")
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
  query <- list(organization_guid = organization_id)

  df_org_details <- doBearerTokenRequest("GET", url = org_url, access_token = Sys.getenv("bitly_access_token"), queryParameters = query)
  df_org_details <- data.frame(df_org_details)

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
#' @import httr2 jsonlite
#'
#' @examples
#' \dontrun{
#' bitly_bearerToken("access token")
#' ros <- bitly_retrieve_orgs()
#' }
#' @import httr2 jsonlite
#' @export
bitly_retrieve_orgs <- function() {
  orgs_url <- "https://api-ssl.bitly.com/v4/organizations"

  df_orgs_details <- doBearerTokenRequest(verb = "GET", url = orgs_url, access_token = Sys.getenv("bitly_access_token"))
  df_orgs_details <- data.frame(df_orgs_details$organizations)

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
#' bitly_bearerToken("access token")
#' all_orgs <- bitly_retrieve_orgs()
#' osc <- bitly_org_shorten_counts(organization_id = all_orgs$guid)
#' df_org_short_counts <- data.frame(osc)
#' }
#'
#' @import httr2 jsonlite assertthat
#' @export
bitly_retrieve_org_shorten_counts <- function(organization_id = NULL) {
  org_short_counts <- paste0("https://api-ssl.bitly.com/v4/organizations/", organization_id, "/shorten_counts")

  if (!is.string(organization_id)) {
    stop("organization_id must not be empty string, NA or NULL")
  }

  query <- list(organization_guid = organization_id)
  df_org_short_counts <- doBearerTokenRequest("GET", org_short_counts, access_token = Sys.getenv("bitly_access_token"), queryParameters = query)

  return(df_org_short_counts)
}

#' Retrieve Organization Shorten Counts by Group
#'
#' Returns the shorten counts for a specific organization by group for the current month.
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getOrganizationShortenCountsByGroup}
#'
#' @inheritSection bitly_retrieve_org Organizations
#'
#' @inheritParams bitly_retrieve_org
#'
#' @examples
#' \dontrun{
#' bitly_bearerToken("access token")
#' all_orgs <- bitly_retrieve_orgs()
#' osc <- bitly_retrieve_org_shorten_counts_by_group(organization_id = all_orgs$guid)
#' df_org_short_counts <- data.frame(osc)
#' }
#'
#' @import httr2 jsonlite assertthat
#' @export
bitly_retrieve_org_shorten_counts_by_group <- function(organization_id = NULL) {
  org_short_counts_by_grp <- paste0("https://api-ssl.bitly.com/v4/organizations/", organization_id, "/shorten_counts_by_group")
  
  if (!is.string(organization_id)) {
    stop("organization_id must not be empty string, NA or NULL")
  }
  
  query <- list(organization_guid = organization_id)
  df_org_short_counts_group <- doBearerTokenRequest("GET", org_short_counts_by_grp, 
                                                    access_token = Sys.getenv("bitly_access_token"), queryParameters = query)
  
  return(df_org_short_counts_group)
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
#' bitly_bearerToken("access token")
#' all_orgs <- bitly_retrieve_orgs()
#' plan <- bitly_retrieve_org_plan_limits(organization_id = all_orgs$guid)
#' }
#'
#' @import httr2 jsonlite assertthat
#' @export
bitly_retrieve_org_plan_limits <- function(organization_id = NULL) {
  org_plan_limits <- paste0("https://api-ssl.bitly.com/v4/organizations/", organization_id, "/plan_limits")

  if (!is.string(organization_id)) {
    stop("organization_id must not be empty string, NA or NULL")
  }

  query <- list(organization_guid = organization_id)

  df_plan_limits <- doBearerTokenRequest("GET", org_plan_limits, access_token = Sys.getenv("bitly_access_token"), queryParameters = query)
  df_org_plan_limits <- data.frame(df_plan_limits$plan_limits)

  return(df_org_plan_limits)
}
