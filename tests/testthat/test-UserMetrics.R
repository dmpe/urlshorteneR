library(testthat)
library(RBitly)
library(httr)
library(RCurl)
library(jsonlite)
library(stringr)

rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")

context("User Metrics")

test_that("Returns aggregate metrics about the countries referring click traffic to all of the authenticated user's Bitlinks.", {
  umcoun <- user.metrics.countries(unit = "day", units = -1, limit = 100, rollup = "true")
  expect_named(umcoun, c("country","clicks")) 
})

test_that("Returns aggregate metrics about the countries referring click traffic to all of the authenticated user's Bitlinks.", {
  umcounc <- user.metrics.countries(unit = "day", units = -1, limit = 100, rollup = "false")
  expect_named(umcounc, c("country","clicks")) # "url" doens't need to be in there (semi-optional)
})

test_that("Returns the aggregate number of clicks on all of the authenticated user's Bitlinks.", {
  umc <- user.metrics.clicks(unit = "day", units = -1, limit = 100, rollup = "true")
})

test_that("Returns the aggregate number of clicks on all of the authenticated user's Bitlinks.", {
  umcc <- user.metrics.clicks(unit = "day", units = -1, limit = 100, rollup = "false")
  expect_named(umcc, c("dt", "clicks")) # "url" doens't need to be in there (semi-optional)
})

test_that("Returns the authenticated user's most-clicked Bitlinks (ordered by number of clicks) in a given time period.", {
  umpl <- user.metrics.popular_links(unit = "month", units = -1, limit = 100)
  expect_named(umpl, c("link", "clicks")) 
})


test_that("Returns aggregate metrics about the pages referring click traffic to all of the authenticated user's Bitlinks.", {
  umr <- user.metrics.referrers(unit = "day", units = -1, limit = 100, rollup = "false")
  expect_named(umr, c("referrer", "clicks")) 
})

test_that("Returns aggregate metrics about the pages referring click traffic to all of the authenticated user's Bitlinks.", {
  umrr <- user.metrics.referrers(unit = "day", units = -1, limit = 100, rollup = "true")
  expect_named(umrr, c("referrer", "clicks")) 
})

test_that("Returns aggregate metrics about the domains referring click traffic to all of the authenticated user's Bitlinks.", {
  umrd <- user.metrics.referring_domains(unit = "day", units = -1, limit = 100, rollup = "true", exclude_social_networks = "false")
  expect_named(umrd, c("domain", "clicks"))
})

test_that("Returns aggregate metrics about the domains referring click traffic to all of the authenticated user's Bitlinks.", {
  umrdr <- user.metrics.referring_domains(unit = "day", units = -1, limit = 100, rollup = "true", exclude_social_networks = "true")
  
  expect_message(user.metrics.referring_domains(unit = "day", units = -1, limit = 100, rollup = "true", exclude_social_networks = "true")
                 , "You have zero referring domains given your function input.") 
})

test_that("Returns the number of Bitlinks created in a given time period by the authenticated user.", {
  umsc <- user.metrics.shorten_counts(unit = "day", units = -1, limit = 100, rollup = "true")
  expect_equal(dim(umsc)[[2]], 5)
})

test_that("Returns the number of Bitlinks created in a given time period by the authenticated user.", {
  umscf <- user.metrics.shorten_counts(unit = "day", units = -1, limit = 100, rollup = "false")
  expect_named(umscf, c("dt", "shortens"))
})
