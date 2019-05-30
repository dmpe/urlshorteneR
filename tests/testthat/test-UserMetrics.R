library(testthat)
library(httr)
library(jsonlite)
library(stringr)
library(urlshorteneR)

context("User Metrics")
# 
# test_that("Returns aggregate metrics about the countries referring click traffic to all of the authenticated user's Bitlinks.", {
#   umcoun <- bitly_UserMetricsCountries(unit = "day", units = -1, limit = 100, rollup = "true")
#   expect_named(umcoun, c("country", "clicks"))
# })
# 
# test_that("Returns the aggregate number of clicks on all of the authenticated user's Bitlinks.", {
#   umc <- bitly_UserMetricsClicks(unit = "day", units = -1, limit = 100, rollup = "true")
#   expect_more_than(umc, 5)
#   umcc <- bitly_UserMetricsClicks(unit = "day", units = -1, limit = 100, rollup = "false")
#   expect_named(umcc, c("dt", "clicks"))
# })
# 
# test_that("Returns the authenticated user's most-clicked Bitlinks (ordered by number of clicks) in a given time period.", {
#   umpl <- bitly_UserMetricsPopularLinks(unit = "month", units = -1, limit = 100)
#   expect_named(umpl, c("link", "clicks"))
# })
# 
# test_that("Returns aggregate metrics about the pages referring click traffic to all of the authenticated user's Bitlinks.", {
#   umrr <- bitly_UserMetricsReferrers(unit = "day", units = -1, limit = 100, rollup = "true")
#   expect_named(umrr, c("referrer", "clicks"))
# })

test_that("will rollup the click counts to a referrer about a single Bitlink.", {
  umrd <- bitly_user_metrics_referring_domains(bitlink = "cnn.it/2HomWGB", size = 100)
  expect_named(umrd, c("facet", "metrics"))

})
# 
# test_that("Returns the number of Bitlinks created in a given time period by the authenticated user.", {
#   umsc <- bitly_UserMetricsShortenCounts(unit = "day", units = -1, limit = 100, rollup = "true")
#   expect_more_than(umsc, 5)
#   umscf <- bitly_UserMetricsShortenCounts(unit = "day", units = -1, limit = 100, rollup = "false")
#   expect_named(umscf, c("dt", "shortens"))
# })
