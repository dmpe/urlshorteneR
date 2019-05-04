library(testthat)
library(httr)
library(jsonlite)
library(stringr)
library(urlshorteneR)

context("User Information")

test_that("Return information about a user.", {
  ui <- bitly_UserInfo()
  expect_equal(dim(ui)[[2]], 11)
})

test_that("User data can be updated", {
  uu <- bitly_update_user(name = "DOma")
  expect_silent(uu)
  expect_warning(bitly_update_user(name = "DOma", default_group_guid = "GroupIDUniqueRetsdat"))
})

test_that("User has a free/premium account", {
  bool_val <- is_bitly_user_premium_holder()
  expect_false(bool_val)
})

context("OAUTH Application Details")

test_that("Application provides some metadata", {
  ad <- bitly_app_details()
  expect_equal(dim(ad)[[2]], 4)
})







context("Group Information")

test_that("Returns entries from a user's link history in reverse chronological order.", {
  ui <- bitly_UserInfo()
  user_group <- bitly_retrieve_group(ui$default_group_guid)
  expect_equal(user_group, 10)
  expect_message(bitly_UserTrackingDomains(), "It seems that you don't have any tracking domains.")
})



context("Link Metrics")

test_that("Returns the number of clicks on a single Bitlink.", {
  lmc <- bitly_LinksMetricsClicks(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_equal(lmc, 6)
  lmc <- bitly_LinksMetricsClicks(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100, rollup = "false")
  expect_named(lmc, c("dt", "clicks"))
})

test_that("Returns metrics about the countries referring click traffic to a single Bitlink.", {
  lmcc <- bitly_LinksMetricsCountries(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_named(lmcc, c("country", "clicks"))
})

test_that("Returns users who have encoded this long URL (optionally only those in the requesting user's social graph).", {
  lme <- bitly_LinksMetricsEncoders(link = "http://bit.ly/DPetrov", my_network = "false", limit = 25)
  expect_named(lme, c("link", "user", "ts"))
})

test_that("Returns the number of users who have shortened (encoded) a single Bitlink.", {
  lmec <- bitly_LinksMetricsEncodersCount(link = "http://bit.ly/DPetrov")
  expect_named(lmec, c("count", "aggregate_link"))
})

test_that("Returns metrics about the domains referring click traffic to a single Bitlink.", {
  lmebc <- bitly_LinksMetricsEncodersByCount(link = "http://bit.ly/DPetrov", my_network = "false", limit = 100)
  expect_named(lmebc, c("count", "link", "user", "ts"))
})

test_that("Returns metrics about the domains referring click traffic to a single Bitlink.", {
  lmrd <- bitly_LinksMetricsReferringDomains(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_named(lmrd, c("domain", "clicks"))
})

test_that("Returns metrics about the pages referring click traffic to a single Bitlink.", {
  lmr <- bitly_LinksMetricsReferrers(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_named(lmr, c("referrer", "clicks"))
})

test_that("Returns metrics about the pages referring click traffic to a single Bitlink, grouped by referring domain.", {
  lmrbd <- bitly_LinksMetricsReferrersByDomain(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  expect_named(lmrbd, c("referrer", "clicks", "type"))
})

context("Domains")

test_that("Query whether a given domain is a valid bitly pro domain. ", {
  expect_message(bitly_IsProDomain(domain = "nytidsfds.ms"), "NOT")
  expect_message(bitly_IsProDomain(domain = "nyti.ms"), "is")
})
