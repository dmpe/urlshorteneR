library(testthat)
library(httr)
library(jsonlite)
library(stringr)
library(urlshorteneR)
library(lubridate)

context("User Metrics")

test_that("will rollup the click counts to a referrer about a single Bitlink.", {
  expect_message(bitly_user_metrics_referring_domains(bitlink = "cnn.it/2HomWGB", size = 100))
  umrd2 <- bitly_user_metrics_referring_domains(bitlink = "1.usa.gov/1IZgFLV", size = 100)
  expect_equal(umrd2$metrics[umrd2$metrics$value == "direct",]$clicks, 4)
})

context("Link Metrics")
 
test_that("Returns the number of clicks on a single Bitlink.", {
  lmc <- bitly_retrieve_clicks(bitlink = "cnn.it/2HomWGB", unit = "month", units = -1, size = 100)
  expect_equal(length(lmc), 4)
  lmcs <- bitly_retrieve_clicks_summary(bitlink = "bit.ly/DPetrov", unit = "day", units = -1, size = 100)
  expect_named(lmcs, c("unit_reference", "total_clicks", "units", "unit"))
})

test_that("Returns metrics about the countries referring click traffic to a single Bitlink.", {
  lmcc <- bitly_retrieve_metrics_by_countries(bitlink = "bit.ly/DPetrov", unit = "day", units = -1, size = 100)
  expect_named(lmcc, c("unit_reference", "metrics", "units", "unit", "facet"))
})

test_that("Returns metrics about the domains referring click traffic to a single Bitlink.", {
  user_info  <- bitly_user_info()
  lmrd <- bitly_retrieve_bitlinks_by_groups(group_guid = user_info$default_group_guid[1])
  expect_equal(length(lmrd), 2)
})

test_that("Returns metrics about the pages referring click traffic to a single Bitlink.", {
  lmr <- bitly_retrieve_metrics_by_referrers(link = "bit.ly/DPetrov", unit = "day", units = -1, size = 100)
  expect_named(lmr, c("referrer", "clicks"))
})

test_that("Returns metrics for a Bitlink by referrers, by domain", {
  lmrbd <- bitly_retrieve_metrics_by_referrers_by_domain(bitlink = "bit.ly/DPetrov", unit = "day", units = -1, size = 100)
  expect_named(lmrbd, c("units", "unit", "unit_reference", "facet", "referrers_by_domain"))
})
