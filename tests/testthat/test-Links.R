library(testthat)
library(httr2)
library(jsonlite)
library(stringr)
library(stringi)
library(urlshorteneR)
library(dotenv)

dotenv::load_dot_env("r.secret")

context("Links Bit.ly")

test_that("method creates a Bitlink based on a long URL.", {
  ll <- bitly_create_bitlink(
    long_url = paste0("https://www.google.com/", stri_rand_strings(1, 8, pattern = "[A-Za-z0-9]")),
    title = stri_rand_strings(1, 8, pattern = "[A-Za-z0-9]"),
    tags = list("msft", "apple"), showRequestURL = T
  )
  expect_equal(dim(ll)[[2]], 8)
})

test_that("Query for a Bitlink", {
  ll <- bitly_retrieve_bitlink(bitlink = "1.usa.gov/1IZgFLV")
  expect_equal(dim(ll)[[2]], 7)
})

test_that("Update a Bitlink.", {
  li <- bitly_update_bitlink(bitlink = "bit.ly/DPetrov", title = stri_rand_strings(1, 8, pattern = "[A-Za-z0-9]"))
  expect_gte(dim(li)[[2]], 7)
})

test_that("Given a bitly URL or hash (or multiple), returns the target (long) URL.", {
  le <- bitly_expand_link(bitlink_id = "1.usa.gov/1IZgFLV")
  le2 <- bitly_expand_link(bitlink_id = "bit.ly/DPetrov")
  expect_equal(dim(le)[[2]], 4)
  expect_named(le2, c("created_at", "link", "id", "long_url"))
})

test_that("Given a bitly URL or hash (or multiple), returns the target (long) URL.", {
  ls <- bitly_shorten_link(long_url = "http://slovnik.seznam.cz/")
  ls2 <- bitly_shorten_link(long_url = "https://travis-ci.org/dmpe/rbitly/builds/68231423")
  expect_equal(dim(ls)[[2]], 6)
  expect_named(ls2, c("created_at", "id", "link", "long_url", "archived", "references"))
})
