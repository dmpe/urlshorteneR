library(testthat)
library(RBitly)
library(httr)
library(jsonlite)
library(stringr)

options(Bit.ly = "0906523ec6a8c78b33f9310e84e7a5c81e500909", Ow.ly = "")

context("Links Bit.ly")

test_that("Query for a Bitlink based on a long URL.", {
  ll <- links_Lookup(url = "http://www.google.com/")
  expect_equal(dim(ll)[[2]], 2)
})

test_that("Used to return the page title for a given Bitlink.", {
  li <- links_Info(hashIN = "DPetrov", expand_user = "true")
  li2 <- links_Info(shortUrl = "http://bit.ly/DPetrov", expand_user = "false")
  expect_equal(dim(li)[[2]], 11)
  expect_equal(dim(li2)[[2]], 7)
})

test_that("Given a bitly URL or hash (or multiple), returns the target (long) URL.", {
  le <- links_Expand(hashIN = "DPetrov")
  le2 <- links_Expand(shortUrl = "http://bit.ly/DPetrov")
  expect_equal(dim(le)[[2]], 4)
  expect_named(le2, c("short_url", "long_url", "user_hash", "global_hash"))
})

test_that("Given a bitly URL or hash (or multiple), returns the target (long) URL.", {
  ls <- links_Shorten(longUrl = "http://slovnik.seznam.cz/")
  ls2 <- links_Shorten(longUrl = "https://travis-ci.org/dmpe/rbitly/builds/68231423", domain = "j.mp")
  expect_equal(dim(ls)[[2]], 5)
  expect_named(ls2, c("long_url", "url", "hash", "global_hash", "new_hash"))
})

context("Links Ow.ly")

test_that("Used to return the page title for a given Bitlink.", {
  li2 <- links_InfoOwly(shortUrl = "http://bit.ly/DPetrov", showRequestURL = FALSE)
  expect_equal(dim(li2)[[2]], 7)
})

test_that("Given a bitly URL or hash (or multiple), returns the target (long) URL.", {
  le2 <- links_ExpandOwly(shortUrl = "http://bit.ly/DPetrov")
  expect_equal(dim(le)[[2]], 4)
  expect_named(le2, c("short_url", "long_url", "user_hash", "global_hash"))
})

test_that("Given a bitly URL or hash (or multiple), returns the target (long) URL.", {
  ls2 <- links_ShortenOwly(longUrl = "https://travis-ci.org/dmpe/rbitly/builds/68231423")
  expect_equal(dim(ls)[[2]], 5)
  expect_named(ls2, c("long_url", "url", "hash", "global_hash", "new_hash"))
})













