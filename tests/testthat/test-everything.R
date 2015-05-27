library(testthat)
library(rbitly)
library(httr)
library(RCurl)
library(jsonlite)

rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")

context("User Info")

test_that("Return information about a user.", {
  ui <- user.info()
  expect_output(str(ui), "status_code: int 200")
})

test_that("Returns entries from a user's link history in reverse chronological order.", {
  user.linkH <- user.linkHistory()
  expect_named(ulh, c("keyword_link","archived", "user_ts" ,"title", "created_at", "tags", "modified_at", 
                      "private", "aggregate_link", "long_url", "client_id", "link", "note"))
})

test_that("Returns entries from a user's link history in reverse chronological order.", {
  user.tdl <- user.tracking_domain_list()
})

context("Link Metrics")

test_that("Returns the number of clicks on a single Bitlink.", {
  lmc <- link.metrics.clicks(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
})

test_that("Returns metrics about the countries referring click traffic to a single Bitlink.", {
  lmcc <- link.metrics.countries(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_named(lmcc, c("country", "clicks"))
})

test_that("Returns the number of users who have shortened (encoded) a single Bitlink.", {
  lmec <- link.metrics.encoders_count(link = "http://bit.ly/DPetrov")
  expect_named(lmec, c("count", "aggregate_link"))
})

test_that("Returns metrics about the domains referring click traffic to a single Bitlink.", {
  lmrd <- link.metrics.referring_domains(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_named(lmrd, c("clicks", "domain")) # "url" doens't need to be in there (semi-optional)
})

context("User Metrics")

test_that("Returns aggregate metrics about the countries referring click traffic to all of the authenticated user's Bitlinks.", {
  umcoun <- user.metrics.countries(unit = "day", units = -1, limit = 100)
  expect_named(umcoun, c("clicks", "country")) # "url" doens't need to be in there (semi-optional)
})

test_that("Returns the aggregate number of clicks on all of the authenticated user's Bitlinks.", {
  umc <- user.metrics.clicks(unit = "day", units = -1, limit = 100)
  expect_named(umc, c("clicks", "country")) # "url" doens't need to be in there (semi-optional)
})




