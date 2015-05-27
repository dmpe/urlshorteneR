library(testthat)
library(rbitly)
library(httr)
library(RCurl)
library(jsonlite)

rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")

context("User Metrics")

test_that("Returns aggregate metrics about the countries referring click traffic to all of the authenticated user's Bitlinks.", {
  umcoun <- user.metrics.countries(unit = "day", units = -1, limit = 100)
  expect_named(umcoun, c("country","clicks")) # "url" doens't need to be in there (semi-optional)
})

test_that("Returns the aggregate number of clicks on all of the authenticated user's Bitlinks.", {
  umc <- user.metrics.clicks(unit = "day", units = -1, limit = 100, rollup = "true")
  expect_named(umc, c("clicks", "country")) # "url" doens't need to be in there (semi-optional)
})

test_that("Returns the aggregate number of clicks on all of the authenticated user's Bitlinks.", {
  umc <- user.metrics.clicks(unit = "day", units = -1, limit = 100, rollup = "false")
  expect_named(umc, c("clicks", "country")) # "url" doens't need to be in there (semi-optional)
})

test_that("Returns the aggregate number of clicks on all of the authenticated user's Bitlinks.", {
  umc <- user.metrics.clicks(units = -1, limit = 100, rollup = "false")
  expect_named(umc, c("clicks", "country")) # "url" doens't need to be in there (semi-optional)
})

