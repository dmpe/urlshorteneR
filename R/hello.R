#'
#' 
#' @import httr
#' @export
# rbitly.api <- function(username, password, http = c('GET', 'PUT', 'POST', 'DELETE'), postdata = NULL, ...) {
#   
#   access_token.url <- "https://api-ssl.bitly.com/oauth/access_token"
#   autenticate <-  authenticate(username, password)
#   
# #   req <- POST(access_token.url, autenticate, encode = "multipart")
# #   content(req)
#   
#   switch(http,
#          GET={
#            response <- httr::GET(access_token.url, autenticate, encode = "multipart")
#          },
#          PUT={
#            response <- httr::PUT(access_token.url, autenticate, encode = "multipart")
#          },
#          POST={
#            response <- httr::POST(access_token.url, autenticate, encode = "multipart")
#          },
#          DELETE={
#            response <- httr::DELETE(access_token.url, autenticate, encode = "multipart")
#          }
#   )
#   
#   
#   if(httr::status_code(response) == 500) {
#     stop("Sorry but Bitly is currently down. Please visit http://status.bit.ly/", call. = FALSE)
#   } else if (httr::status_code(response) != 200) {
#     stop(httr::content(response, as="text"), call. = FALSE)
#   } else {
#     text_response <- httr::content(response, as="text")
#     return(jsonlite::fromJSON(text_response, simplifyVector=TRUE))
#   }
#   
#   
# }