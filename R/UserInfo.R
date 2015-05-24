#' Return or update information about a user.
#'
#' See http://dev.bitly.com/user_info.html#v3_user_info
#' 
#' @export
user.info <- function() {
  user.info.url <- "https://api-ssl.bitly.com/v3/user/info"
  doRequest(user.info.url)
}

#' Returns entries from a user's link history in reverse chronological order.
#' See http://dev.bitly.com/user_info.html#v3_user_link_history
#' @export
user.linkHistory <- function(){
  user.linkHistory.url <- "https://api-ssl.bitly.com/v3/user/link_history"
  df.all <- doRequest(user.linkHistory.url)
  adwa <<- df.all$data$link_history
}

#' @import httr
#' @import jsonlite
#' @export
doRequest <- function(url, authcode = rbitlyApi()) {
  createdUrl <- paste(url, authcode, sep = "?access_token=")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  returnGetRequest <- GET(createdUrl)
  text_response <- content(returnGetRequest, as = "text")
  json_response <- fromJSON(text_response)
  json_response
}