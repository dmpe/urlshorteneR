library(testthat)
library(urlshorteneR)
library(httr)
library(jsonlite)
library(stringr)

bitly_token <- bitly_auth(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8", secret = "b7e4abaf8b26ec4daa92b1e64502736f5cd78899")
google_token <- google_auth(key = "806673580943-78jdskus76fu7r0m21erihqtltcka29i.apps.googleusercontent.com", secret = "qItL-PZnm8GFxUOYM0zPVr_t")

context("User Info")

test_that("Return information about a user.", {
  ui <- bitly_UserInfo()
  expect_equal(dim(ui)[[2]], 2)
})

test_that("Returns entries from a user's link history in reverse chronological order.", {
  user.linkH <- bitly_UserLinkHistory()
  expect_more_than(length(user.linkH), 10)
  expect_message(user_TrackingDomains(), "It seems that you don't have any tracking domains.")
})

test_that("Returns entries from a user's link history from Google.", {
  g3 <- googl_UserLinkHistory()
  expect_more_than(nrow(g3), 10)
})

context("Link Metrics")

test_that("Returns the number of clicks on a single Bitlink.", {
  lmc <- bitly_LinksMetricsClicks(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_equal(lmc, 6)
  lmc <- bitly_LinksMetricsClicks(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100, rollup = "false")
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
  expect_named(lmebc, c("count", "link", "user", "ts")) 
})

test_that("Returns metrics about the domains referring click traffic to a single Bitlink.", {
  lmrd <- link_Metrics_ReferringDomains(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_named(lmrd, c("domain", "clicks")) 
})

test_that("Returns metrics about the pages referring click traffic to a single Bitlink.", {
  lmr <- link_Metrics_Referrers(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_named(lmr, c( "referrer", "clicks"))
})

test_that("Returns metrics about the pages referring click traffic to a single Bitlink, grouped by referring domain.", {
  lmrbd <- link_Metrics_ReferrersByDomain(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_named(lmrbd, c( "referrer", "clicks", "type"))
})

context("Domains")

test_that("Query whether a given domain is a valid bitly pro domain. ", {
  expect_message(bitly_pro_domain(domain = "nytidsfds.ms"), "NOT")
  expect_message(bitly_pro_domain(domain = "nyti.ms"), "is") 
})

