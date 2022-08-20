## -----------------------------------------------------------------------------
library("knitr")
library(urlshorteneR)

if(interactive()) {
# You must register a new pair of keys yourself
# bitly_token <- bitly_auth(key = "xxx", secret = "xxx")
# bitly_token <- bitly_auth()

  ui <- bitly_user_info(showRequestURL = TRUE)
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

## -----------------------------------------------------------------------------
isgd_LinksShorten(longUrl = "https://us.cnn.com", showRequestURL = TRUE)

## -----------------------------------------------------------------------------
vgd_LinksShorten(longUrl = "https://www.cbs.com", showRequestURL = TRUE)

