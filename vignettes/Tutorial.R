## -----------------------------------------------------------------------------
library("knitr")
library(urlshorteneR)

if (interactive()) {
  ui <- bitly_user_info(access_token = Sys.getenv("access_token"), showRequestURL = TRUE)
  is_bitly_user_premium_holder()
}

## -----------------------------------------------------------------------------
if (interactive()) {
  bitly_update_user(access_token = Sys.getenv("access_token"), name = "John Malc", showRequestURL = TRUE)
}

## -----------------------------------------------------------------------------
if (interactive()) {
  bitly_app_details()
}

## -----------------------------------------------------------------------------
if (interactive()) {
  bitly_retrieve_group(access_token = Sys.getenv("access_token"), ui$default_group_guid)
  bitly_retrieve_groups(access_token = Sys.getenv("access_token"))
}

## -----------------------------------------------------------------------------
if (interactive()) {
  bitly_user_info(access_token = Sys.getenv("access_token"))
}

## -----------------------------------------------------------------------------
if (interactive()) {
  df <- data.frame(
    pubDate = rep("2016-02-10", 4),
    link = c(
      "https://www.google.com",
      "https://www.apple.com"
    )
  )
  df

  fin <- NULL
  for (p in 1:length(df$link)) {
    fin[[p]] <- bitly_create_bitlink(access_token = Sys.getenv("access_token"), long_url = df$link[p])
  }
}

## -----------------------------------------------------------------------------
isgd_LinksShorten(longUrl = "https://www.google.com", showRequestURL = TRUE)

## -----------------------------------------------------------------------------
vgd_LinksShorten(longUrl = "https://www.apple.com", showRequestURL = TRUE)

