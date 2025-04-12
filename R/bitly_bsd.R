#' Fetch all Branded Short Domains
#'
#' BSDs is an acronym for branded short domains. This is a custom 15
#' character or less domain for bitlinks. This allows you to customize the domain to your brand.
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getBSDs}
#'
#' @inheritParams bitly_retrieve_links_grouped
#'
#' @import httr2 jsonlite
#' @export
bitly_bsds <- function(access_token, showRequestURL = F) {
  url_bsds <- "https://api-ssl.bitly.com/v4/bsds"

  df_bsds <- doBearerTokenRequest("GET", url = url_bsds, access_token = access_token, showURL = showRequestURL)

  if (length(df_bsds$bsds) == 0) {
    warning("There are no branded domains. First create some in Bitly.com")
  } else {
    df_bsds <- data.frame(df_bsds, stringsAsFactors = FALSE)
    return(df_bsds)
  }
}

#' Branded Short Domains Group Overrides (Premium)
#'
#' @description
#' Retrieves all account overrides matching specified group_guid and bsd query filters.
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#getOverridesForGroups}
#'
#' @inheritParams bitly_user_info
#' @inheritParams bitly_retrieve_links_grouped
#'
#' @examples
#' \dontrun{
#' ui <- bitly_user_info(showRequestURL = TRUE)
#' bsds_over <- bitly_bsds_overrides(group_id = ui$default_group_guid[1])
#' }
#'
#' @import httr2 jsonlite
#' @export
bitly_bsds_overrides <- function(access_token, group_id = NA, showRequestURL = F) {
  if (is.string(group_id)) {
    url_bsds_overrides <- paste0("https://api-ssl.bitly.com/v4/groups", group_id, "/overrides")
  } else {
    stop("group_id must not be empty string, NA or NULL")
  }

  df_bsds_overrides <- doBearerTokenRequest("GET", url = url_bsds_overrides, access_token = access_token, showURL = showRequestURL)
  return(df_bsds_overrides)
}
