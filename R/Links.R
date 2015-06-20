#' @title Query for a Bitlink based on a long URL.
#' 
#' @seealso See \url{http://dev.bitly.com/links.html#v3_link_lookup}
#'
#' @param url - one or more long URLs to lookup.
#'
#' @return url - an echo back of the url parameter.
#' @return aggregate_link - the corresponding bitly aggregate link (global hash).
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' links_Lookup(url = "http://www.google.com/") 
#' links_Lookup(url = list("http://www.google.com/", "http://www.seznam.cz/")) 
#'
#' @export
links_Lookup <- function(url) {
  
  links_lookup_url <- "https://api-ssl.bitly.com/v3/link/lookup"
  
  query <- list(access_token = rbitlyApi(), url = url)
  
  # call method from ApiKey.R
  df_link_lookup <- doRequest(links_lookup_url, query)
  df_link_lookup_data <- df_link_lookup$data$link_lookup
  
  # sapply(df_link_lookup_data, class)
  return(df_link_lookup_data)
}