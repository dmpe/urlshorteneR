#' @title Create QR code with bit.ly link
#'
#' @description
#' Retrive details for the specified organization.
#'
#' @seealso \url{https://dev.bitly.com/api-reference/#createQRCodePublic}
#'
#' @param title - a required string
#' @param group_guid - group id 
#' @param destination - a description
#'
#' @import httr2 jsonlite assertthat
#'
#' @examples
#' \dontrun{
#' qr <- bitly_qr_create_code(group_guid = "aeqog323", bitly_link = "bit.ly/abc1234")
#' }
#'
#' @export
bitly_qr_create_code <- function(access_token, title = NULL, group_guid = NULL, bitly_link=NULL) {
  qr_url <- "https://api-ssl.bitly.com/v4/qr-codes"
  
  if (!is.string(group_guid)) {
    stop("organization_id must not be empty string, NA or NULL")
  }
  query <- list(group_guid = group_guid, destination = list(bitlink_id = bitly_link))
  
  df_qr_code_details <- doBearerTokenRequest("POST", url = qr_url, access_token = access_token, queryParameters = query)
  df_qr_code_details <- data.frame(df_qr_code_details)
  
  return(df_qr_code_details)
}
