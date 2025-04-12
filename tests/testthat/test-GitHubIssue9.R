# https://github.com/dmpe/urlshorteneR/issues/9

library(testthat)
library(httr2)
library(jsonlite)
library(stringr)
library(urlshorteneR)
library(dotenv)

dotenv::load_dot_env("r.secret")
test_that("issue 9 is fixed", {
  expect_error(expect_message(bitly_shorten_link(access_token = Sys.getenv("access_token"), long_url = ""), "INVALID_ARG_LONG_URL"))
})
