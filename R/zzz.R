#' @title Set bit.ly bearer API key
#'
#' @description
#' Set bearer token as env. variable.
#'
#' @seealso \url{https://dev.bitly.com/docs/getting-started/authentication/}.
#' Inspired by \url{https://github.com/jhk0530/gemini.R}
#'
#' @export
#' @examples
#' \dontrun{
#' bitly_bearerToken("access token")
#' }
#' @keywords internal
bitly_bearerToken <- function(token) {
  Sys.setenv(bitly_access_token = token)
}
