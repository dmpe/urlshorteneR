#' @title Validate given domain for PRO features
#' 
#' @seealso See \url{http://dev.bitly.com/domains.html#v3_bitly_pro_domain}
#' 
#' @description Query whether a given domain is a valid bitly pro domain. Keep in mind that bitly custom 
#' short domains are restricted to less than 15 characters in length.
#'
#' @param domain - A short domain. ie: nyti.ms.
#'
#' @return bitly_pro_domain - 0 or 1 designating whether this is a current bitly domain.
#' @return domain - an echo back of the request parameter.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' bitly_pro_domain(domain = "nytidsfds.ms") 
#'
#' @export
bitly_pro_domain <- function(domain) {
  
  bitly_pro_domain_url <- "https://api-ssl.bitly.com/v3/bitly_pro_domain"
  
  created_URL <- paste(bitly_pro_domain_url, "?domain=", domain, sep = "")
  created_URL <- paste(created_URL, "&format=json", sep = "")
  
  # call method from ApiKey.R
  df_bitly_pro_domain <- doRequest(created_URL)
  df_bitly_pro_domain_data <- df_bitly_pro_domain$data
  
  if (df_bitly_pro_domain_data$bitly_pro_domain == FALSE) {
    message("A short domain: ", df_bitly_pro_domain_data$domain, " is NOT a valid bitly pro domain")
  } else {
    message("A short domain: ", df_bitly_pro_domain_data$domain, " is a valid bitly pro domain")
  }
  
}
