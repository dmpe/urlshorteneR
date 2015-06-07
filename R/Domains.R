#' @title Validate given domain for PRO features
#' 
#' Query whether a given domain is a valid bitly pro domain. Keep in mind that bitly custom 
#' short domains are restricted to less than 15 characters in length.
#'
#' @param domain - A short domain. ie: nyti.ms.
#'
#' @return bitly_pro_domain - 0 or 1 designating whether this is a current bitly domain.
#' @return domain - an echo back of the request parameter.
#' 
#' @todo testing
#'
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' bitly_pro_domain(domain = "nytidsfds.ms") 
#'
#' @import RCurl
#' @export
bitly_pro_domain <- function(domain) {
  
  bitly_pro_domain.url <- "https://api-ssl.bitly.com/v3/bitly_pro_domain"
  
  createdUrl <- paste(bitly_pro_domain.url, "?domain=", domain, sep = "")
  createdUrl <- paste(createdUrl, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df.bitly_pro_domain <- doRequest(createdUrl)
  df.bitly_pro_domain.data <- df.bitly_pro_domain$data
  
  if (df.bitly_pro_domain.data$bitly_pro_domain == FALSE) {
    cat("A short domain:", df.bitly_pro_domain.data$domain, "is NOT a valid bitly pro domain")
  } else {
    cat("A short domain:", df.bitly_pro_domain.data$domain, "is a valid bitly pro domain")
  }
  
}
