library(testthat)
library(urlshorteneR)
library(clipr)

Sys.setenv("CLIPR_ALLOW" = TRUE)

long_url <- "https://github.com/dmpe/urlshorteneR"
short_url <- "https://bit.ly/3hXbilf"

test_that("Clipboard shortener copies short url to clipboard", {
  write_clip(long_url, allow_non_interactive = TRUE)
  clipShortenerAddin()

  short_url <- bitly_shorten_link(long_url = long_url)
  short_url <- short_url$link[[1]]

  expect_equal(read_clip(), short_url)
})


test_that("Clipboard expander copies long url to clipboard", {
  write_clip(short_url, allow_non_interactive = TRUE)
  clipExpanderAddin()

  bitly_id <- gsub(pattern = "https://", x = short_url, replacement = "")

  long_url <- bitly_expand_link(bitlink_id = bitly_id)
  long_url <- long_url$long_url[[1]]

  expect_equal(read_clip(), long_url)
})
