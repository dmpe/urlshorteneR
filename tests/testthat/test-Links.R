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


test_that("Query for a Bitlink based on a long URL.", {
  ll <- links_Info(hash = "DPetrov", expand_user = "true")
  ll2 <- links_Info(shortUrl = "http://bit.ly/DPetrov", expand_user = "false")
  expect_equal(dim(ll)[[2]], 11)
  expect_equal(dim(ll2)[[2]], 7)
})








