# https://github.com/dmpe/urlshorteneR/issues/9

library(testthat)
library(urlshorteneR)
library(httr)
library(jsonlite)
library(stringr)

bitly_token <- bitly_auth()

test_that("issue 9 is fixed", {
  expect_error(expect_message(bitly_LinksShorten(longUrl = ""), "MISSING_ARG_URI"))
})
