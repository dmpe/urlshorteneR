library(testthat)
library(httr)
library(jsonlite)
library(stringr)
library(urlshorteneR)

context("User Metrics")

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
  expect_message(bitly_user_metrics_referring_domains(bitlink = "cnn.it/2HomWGB", size = 100))
  umrd2 <- bitly_user_metrics_referring_domains(bitlink = "1.usa.gov/1IZgFLV", size = 100)
  expect_equal(umrd2$metrics[umrd2$metrics$value == "direct",]$clicks, 4)
})

# test_that("Returns the number of Bitlinks created in a given time period by the authenticated user.", {
#   umsc <- bitly_UserMetricsShortenCounts(unit = "day", units = -1, limit = 100, rollup = "true")
#   expect_more_than(umsc, 5)
#   umscf <- bitly_UserMetricsShortenCounts(unit = "day", units = -1, limit = 100, rollup = "false")
#   expect_named(umscf, c("dt", "shortens"))
# })

# context("Link Metrics")
# 
# test_that("Returns the number of clicks on a single Bitlink.", {
#   lmc <- bitly_LinksMetricsClicks(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#   expect_equal(lmc, 6)
#   lmc <- bitly_LinksMetricsClicks(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100, rollup = "false")
#   expect_named(lmc, c("dt", "clicks"))
# })
# 
# test_that("Returns metrics about the countries referring click traffic to a single Bitlink.", {
#   lmcc <- bitly_LinksMetricsCountries(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#   expect_named(lmcc, c("country", "clicks"))
# })
# 
# test_that("Returns users who have encoded this long URL (optionally only those in the requesting user's social graph).", {
#   lme <- bitly_LinksMetricsEncoders(link = "http://bit.ly/DPetrov", my_network = "false", limit = 25)
#   expect_named(lme, c("link", "user", "ts"))
# })
# 
# test_that("Returns the number of users who have shortened (encoded) a single Bitlink.", {
#   lmec <- bitly_LinksMetricsEncodersCount(link = "http://bit.ly/DPetrov")
#   expect_named(lmec, c("count", "aggregate_link"))
# })
# 
# test_that("Returns metrics about the domains referring click traffic to a single Bitlink.", {
#   lmebc <- bitly_LinksMetricsEncodersByCount(link = "http://bit.ly/DPetrov", my_network = "false", limit = 100)
#   expect_named(lmebc, c("count", "link", "user", "ts"))
# })
# 
# test_that("Returns metrics about the domains referring click traffic to a single Bitlink.", {
#   lmrd <- bitly_LinksMetricsReferringDomains(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#   expect_named(lmrd, c("domain", "clicks"))
# })
# 
# test_that("Returns metrics about the pages referring click traffic to a single Bitlink.", {
#   lmr <- bitly_LinksMetricsReferrers(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#   expect_named(lmr, c("referrer", "clicks"))
# })
# 
# test_that("Returns metrics about the pages referring click traffic to a single Bitlink, grouped by referring domain.", {
#   lmrbd <- bitly_LinksMetricsReferrersByDomain(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
#   expect_named(lmrbd, c("referrer", "clicks", "type"))
# })
