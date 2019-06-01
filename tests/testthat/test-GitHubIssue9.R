# https://github.com/dmpe/urlshorteneR/issues/9

library(testthat)
library(httr)
library(jsonlite)
library(stringr)
library(urlshorteneR)

test_that("issue 9 is fixed", {
  expect_error(expect_message(bitly_shorten_link(long_url = ""), "INVALID_ARG_LONG_URL"))
})
