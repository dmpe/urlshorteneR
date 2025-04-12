#' @title Create QR code with bit.ly link
#'
#' @description
#' Create a new QR Code and return its metadata
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#createQRCodePublic}
#'
#' @param title - a required string
#' @param group_guid - group id
#' @param access_token - bearer token for authentication
#' @param bitly_link - a bit.ly link
#'
#' @import httr2 jsonlite assertthat
#'
#' @examples
#' \dontrun{
#'   qr <- bitly_qr_create_code(group_guid = "Be2oejZbDDc", bitly_link = "bit.ly/abc1234")
#' }
#'
#' @export
bitly_qr_create_code <- function(access_token, title = NULL, group_guid = NULL, bitly_link = NULL) {
  qr_url <- "https://api-ssl.bitly.com/v4/qr-codes"

  if (!is.string(group_guid)) {
    stop("group_guid param must not be an empty string, NA or NULL")
  }
  query <- list(title = title, group_guid = group_guid, destination = list(bitlink_id = bitly_link))

  df_qr_code_details <- doBearerTokenRequest("POST", url = qr_url, access_token = access_token, queryParameters = query)
  df_qr_code_details <- data.frame(df_qr_code_details)

  return(df_qr_code_details)
}
