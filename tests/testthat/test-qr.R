library(testthat)
library(httr2)
library(jsonlite)
library(stringr)
library(stringi)
library(urlshorteneR)
library(dotenv)

dotenv::load_dot_env("r.secret")
context("QR Codes of Bit.ly")

test_that("method creates a Bitlink based on a long URL.", {
  ll <- bitly_qr_create_code(
    title = "google.com",
    group_guid = "Be2oejZbDDc", bitly_link = "bit.ly/abc1234"
  )
  expect_equal(dim(ll)[[2]], 12)
})
