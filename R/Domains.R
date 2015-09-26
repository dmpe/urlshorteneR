#' @title Validate given domain for the PRO features
#' 
#' @seealso See \url{http://dev.bitly.com/domains.html#v3_bitly_pro_domain}
#' 
#' @description Query whether a given domain is a valid bitly pro domain. Keep in mind that bitly custom 
#' short domains are restricted to less than 15 characters in length.
#'
#' @param domain - A short domain. ie: nyti.ms.
#' @param showRequestURL - show URL which has been build and requested from server. For debug purposes.
#'
#' @return bitly_pro_domain - 0 or 1 designating whether this is a current bitly domain.
#' @return domain - an echo back of the request parameter.
#' 
#' @examples
#' bitly_token <- bitly_auth(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8", secret = "b7e4abaf8b26ec4daa92b1e64502736f5cd78899")
#' bitly_pro_domain(domain = "nytidsfds.ms") 
#' bitly_pro_domain(domain = "nyti.ms", showRequestURL = TRUE) 
#'
#' @export
bitly_pro_domain <- function(domain, showRequestURL = FALSE) {
  
  bitly_pro_domain_url <- "https://api-ssl.bitly.com/v3/bitly_pro_domain"
  
  query <- list(access_token = bitly_token$credentials$access_token, domain = domain)
  
  # call method from ApiKey.R
  df_bitly_pro_domain <- doRequest(verb = "GET", bitly_pro_domain_url, "bitly", query = query, showURL = showRequestURL)

  if (df_bitly_pro_domain$data$bitly_pro_domain == FALSE) {
    message("A short domain: ", df_bitly_pro_domain$data$domain, " is NOT a valid bitly pro domain")
  } else {
    message("A short domain: ", df_bitly_pro_domain$data$domain, " is a valid bitly pro domain")
  }
  # sapply(df_bitly_pro_domain$data, class)
  
}
