library(testthat)
library(RBitly)
library(httr)
library(jsonlite)
library(stringr)

rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")

context("User Info")

test_that("Return information about a user.", {
  ui <- user_Info()
  expect_equal(dim(ui)[[2]], 2)
})

test_that("Returns entries from a user's link history in reverse chronological order.", {
  user.linkH <- user_LinkHistory()
  expect_more_than(length(user.linkH), 10)
})

test_that("Returns entries from a user's link history in reverse chronological order.", {
  expect_message(user_TrackingDomains(), "It seems that you don't have any tracking domains.")
})

context("Link Metrics")

test_that("Returns the number of clicks on a single Bitlink.", {
  lmc <- link_Metrics_Clicks(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_equal(lmc, 4)
})

test_that("Returns the number of clicks on a single Bitlink.", {
  lmc <- link_Metrics_Clicks(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100, rollup = "false")
  expect_named(lmc, c("dt", "clicks"))
})

test_that("Returns metrics about the countries referring click traffic to a single Bitlink.", {
  lmcc <- link_Metrics_Countries(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_named(lmcc, c("country", "clicks"))
})

test_that("Returns users who have encoded this long URL (optionally only those in the requesting user's social graph).", {
  lme <- link_Metrics_Encoders(link = "http://bit.ly/DPetrov", my_network = "false", limit = 25)
  expect_named(lme, c("link", "user", "ts")) 
})

test_that("Returns the number of users who have shortened (encoded) a single Bitlink.", {
  lmec <- link_Metrics_EncodersCount(link = "http://bit.ly/DPetrov")
  expect_named(lmec, c("count", "aggregate_link"))
})

test_that("Returns metrics about the domains referring click traffic to a single Bitlink.", {
  lmebc <- link_Metrics_EncodersByCount(link = "http://bit.ly/DPetrov", my_network = "false", limit = 100)
  expect_named(lmebc, c("count", "link", "user", "ts")) # "url" doens't need to be in there (semi-optional)
})

test_that("Returns metrics about the domains referring click traffic to a single Bitlink.", {
  lmrd <- link_Metrics_ReferringDomains(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_named(lmrd, c("domain", "clicks")) # "url" doens't need to be in there (semi-optional)
})

test_that("Returns metrics about the pages referring click traffic to a single Bitlink.", {
  lmr <- link_Metrics_Referrers(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_named(lmr, c( "referrer", "clicks"))
})

test_that("Returns metrics about the pages referring click traffic to a single Bitlink, grouped by referring domain.", {
  lmrbd <- link_Metrics_ReferrersByDomain(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_named(lmrbd, c( "referrer", "clicks", "type"))
})

