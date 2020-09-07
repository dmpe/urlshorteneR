#' Fetch all Branded Short Domains
#'
#' BSDs is an acronym for branded short domains. This is a custom 15
#' character or less domain for bitlinks. This allows you to customize the domain to your brand.
#'
#' @seealso \url{https://dev.bitly.com/v4/#operation/getBSDs}
#'
#' @import httr jsonlite
#' @export
bitly_bsds <- function() {
  url_bsds <- "https://api-ssl.bitly.com/v4/bsds"

  query <- list(access_token = bitly_auth_access())

  df_bsds <- doRequest("GET", url = url_bsds, queryParameters = query)

  if (length(df_bsds$bsds) == 0) {
    warning("There are no branded domains. First create some in Bitly.com")
  } else {
    df_bsds <- data.frame(df_bsds, stringsAsFactors = FALSE)
    return(df_bsds)
  }
}
