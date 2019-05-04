#' Fetch all Branded Short Domains
#'
#' @seealso \url{https://dev.bitly.com/v4/#operation/getBSDs}
#'
#' @import httr jsonlite
#' @export
bitly_bsds <- function() {
  url_bsds <- "https://api-ssl.bitly.com/v4/bsds"

  query <- list(access_token = bitly_token$credentials$access_token)

  df_bsds <- doRequest("GET", url = url_bsds, queryParameters = query)
  
  if (length(df_bsds$bsds) == 0) {
    warning("There are no branded domains. First create some in Bitly.com")
  } else {
    df_bsds <- data.frame(df_bsds, stringsAsFactors = FALSE)
    return(df_bsds)
  }
}
