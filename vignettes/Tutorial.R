## -----------------------------------------------------------------------------
library("knitr")
library(urlshorteneR)

if(interactive()) {
  access_token <- "0906523ec6a8c78b33f9310e84e7a5c81e500909"

  ui <- bitly_user_info(access_token, showRequestURL = TRUE)
  is_bitly_user_premium_holder()
}

## -----------------------------------------------------------------------------
if (interactive()) {
bitly_update_user(name = "John Malc", showRequestURL = TRUE)
}

## -----------------------------------------------------------------------------
if (interactive()) {
bitly_app_details()
}

## -----------------------------------------------------------------------------
if (interactive()) {

bitly_retrieve_group(ui$default_group_guid)
bitly_retrieve_groups()
}

## -----------------------------------------------------------------------------
if (interactive()) {
bitly_user_info()
}

## -----------------------------------------------------------------------------
if (interactive()) {
  df <- data.frame(pubDate = rep("2016-02-10", 4),
                   link = c("https://www.google.com",
                            "https://www.apple.com"),
                   stringsAsFactors = FALSE)
  df
  
  fin = NULL
  for (p in 1:length(df$link)) {
    fin[[p]] <- bitly_create_bitlink(long_url = df$link[p])
  }
}

