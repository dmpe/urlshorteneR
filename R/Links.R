#' @title Query for a Bitlink based on a long URL.
#' 
#' @seealso See \url{http://dev.bitly.com/links.html#v3_link_lookup}
#'
#' @param url - one long URLs to lookup.
#' @param showRequestURL - show URL which has been build and requested from server. For debug purposes.
#' 
#' @section TODO: or more URLs  
#'  
#' @return url - an echo back of the url parameter.
#' @return aggregate_link - the corresponding bitly aggregate link (global hash).
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' links_Lookup(url = "http://www.seznam.cz/")
#' links_Lookup(url = "http://www.seznam.cz/", showRequestURL = TRUE) 
#' 
#' @export
links_Lookup <- function(url, showRequestURL = FALSE) {
  
  links_lookup_url <- "https://api-ssl.bitly.com/v3/link/lookup"
  
  query <- list(access_token = rbitlyApi(), url = url)
  
  # call method from ApbiKey.R
  df_link_lookup <- doRequest(links_lookup_url, query, showURL = showRequestURL)
  df_link_lookup_data <- df_link_lookup$data$link_lookup
  
  # sapply(df_link_lookup_data, class)
  return(df_link_lookup_data)
}

#' @title Used to return the page title for a given Bitlink.
#' 
#' @seealso See \url{http://dev.bitly.com/links.html#v3_info}
#' 
#' @note Either shortUrl or hash must be given as a parameter (or both).
#' @note The maximum number of shortUrl and hash parameters is 15.
#' 
#' @param hashIN - refers to one bitly hashes, (e.g.:  2bYgqR or a-custom-name). Required
#' @param shortUrl - refers to one Bitlinks e.g.: http://bit.ly/1RmnUT or http://j.mp/1RmnUT. Optional.
#' @param expand_user - optional true|false (default) - include extra user info in response.
#' @param showRequestURL - show URL which has been build and requested from server. For debug purposes.
#' 
#' @section TODO: or more URLs  
#' 
#' @return short_url - this is an echo back of the shortUrl request parameter.
#' @return hash - this is an echo back of the hash request parameter.
#' @return user_hash - the corresponding bitly user identifier.
#' @return global_hash - the corresponding bitly aggregate identifier.
#' @return error - indicates there was an error retrieving data for a given shortUrl or hash. An 
#' example error is "NOT_FOUND".
#' @return title - the HTML page title for the destination page (when available).
#' @return created_by - the bitly username that originally shortened this link, if the link is public. Otherwise, null.
#' @return created_at - the epoch timestamp when this Bitlink was created.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' links_Info(shortUrl = "http://bit.ly/DPetrov")
#' links_Info(hash = "DPetrov", showRequestURL = TRUE) 
#' links_Info(hash = "DPetrov", expand_user = "true")
#' links_Info(shortUrl = "on.natgeo.com/1bEVhwE", hash = "DPetrov") # hash is the one which is only returned 
#' 
#' @export
links_Info <- function(hashIN = NULL, shortUrl = NULL, expand_user = "true", showRequestURL = FALSE) {
  
  links_info_url <- "https://api-ssl.bitly.com/v3/info"
  
  if (is.null(hashIN)) {
    query <- list(access_token = rbitlyApi(), shortUrl = shortUrl, expand_user = expand_user)
  } else {
    query <- list(access_token = rbitlyApi(), hash = hashIN, expand_user = expand_user)
  }
  
  # call method from ApbiKey.R
  df_link_info <- doRequest(links_info_url, query, showURL = showRequestURL)
  
  df_user_info_data <- data.frame(t(sapply(unlist(df_link_info$data$info), c)), stringsAsFactors = FALSE)
  df_user_info_data$created_at <- as.POSIXct(as.integer(df_user_info_data$created_at), origin = "1970-01-01", tz = "UTC")
  
  # sapply(df_link_info_data, class)
  return(df_user_info_data)
}



#' @title Given a bitly URL or hash (or multiple), returns the target (long) URL.
#' 
#' @seealso See \url{http://dev.bitly.com/links.html#v3_expand}
#'
#' @param hashIN - refers to one bitly hashes, (e.g.:  2bYgqR or a-custom-name). Required
#' @param shortUrl - refers to one Bitlinks e.g.: http://bit.ly/1RmnUT or http://j.mp/1RmnUT. Optional.
#' @param showRequestURL - show URL which has been build and requested from server. For debug purposes.
#' 
#' @section TODO: or more URLs  
#' 
#' @note Either shortUrl or hash must be given as a parameter.
#' @note The maximum number of shortUrl and hash parameters is 15.
#' 
#' @return short_url - this is an echo back of the shortUrl request parameter.
#' @return hash - this is an echo back of the hash request parameter.
#' @return user_hash - the corresponding bitly user identifier.
#' @return global_hash - the corresponding bitly aggregate identifier.
#' @return error - indicates there was an error retrieving data for a given shortUrl or hash. An 
#' example error is "NOT_FOUND".
#' @return long_url - the URL that the requested short_url or hash points to.
#' 
#' @examples
#' rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
#' links_Expand(shortUrl = "http://bit.ly/DPetrov")
#' links_Expand(hash = "DPetrov", showRequestURL = TRUE) 
#' links_Expand(hash = "DPetrov")
#' links_Expand(shortUrl = "on.natgeo.com/1bEVhwE", hash = "1bEVhwE")
#' 
#' @export
links_Expand <- function(hashIN = NULL, shortUrl = NULL, showRequestURL = FALSE) {
  
  links_expand_url <- "https://api-ssl.bitly.com/v3/expand"
  
  if (is.null(hashIN)) {
    query <- list(access_token = rbitlyApi(), shortUrl = shortUrl)
  } else {
    query <- list(access_token = rbitlyApi(), hash = hashIN)
  }
  
  # call method from ApbiKey.R
  df_link_expand <- doRequest(links_expand_url, query, showURL = showRequestURL)
  
  df_link_expand_data <- data.frame(t(sapply(unlist(df_link_expand$data$expand), c)), stringsAsFactors = FALSE)

  # sapply(df_link_expand_data, class)
  return(df_link_expand_data)
}

















