library(testthat)
library(RBitly)
library(httr)
library(jsonlite)
library(stringr)

rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")

context("Links")


test_that("Query for a Bitlink based on a long URL.", {
  ll <- links_Lookup(url = "http://www.google.com/")
  expect_equal(dim(ll)[[2]], 2)
})

test_that("Used to return the page title for a given Bitlink.", {
  li <- links_Info(hash = "DPetrov", expand_user = "true")
  li2 <- links_Info(shortUrl = "http://bit.ly/DPetrov", expand_user = "false")
  expect_equal(dim(li)[[2]], 11)
  expect_equal(dim(li2)[[2]], 7)
})

test_that("Given a bitly URL or hash (or multiple), returns the target (long) URL.", {
  le <- links_Expand(hash = "DPetrov")
  le2 <- links_Expand(shortUrl = "http://bit.ly/DPetrov")
  expect_equal(dim(le)[[2]], 4)
  expect_named(le2, c("short_url", "long_url", "user_hash", "global_hash"))
})





