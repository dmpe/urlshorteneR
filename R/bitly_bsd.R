#' Fetch all Branded Short Domains
#'
#' @seealso \url{https://dev.bitly.com/v4/#operation/getBSDs}
#'
#' @import httr jsonlite
#' @export
bsds <- function() {
  url_bsds <- "https://api-ssl.bitly.com/v4/bsds"

  query <- list(access_token = bitly_token$credentials$access_token)

  df_bsds <- doRequest("GET", url_bsds, query)

  df_bsds <- data.frame(df_bsds, stringsAsFactors = FALSE)

  return(df_bsds)
}
