library(testthat)
library(urlshorteneR)
library(httr)
library(jsonlite)
library(stringr)

bitly_token <- bitly_auth(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8", secret = "b7e4abaf8b26ec4daa92b1e64502736f5cd78899")

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

context("Links Goo.gl")

test_that("expanding does work", {
  g1 <- links_ExpandGoogl(shortUrl = "http://goo.gl/vM0w4", showRequestURL = F)
  expect_output(g1$original_data$longUrl, "http://www.bi-verdict.com/fileadmin/FreeAnalyses/consolidations.htm")
}) 

test_that("shorting does work", {
  g2 <- links_ShortenGoogl(longUrl = "https://developers.google.com/url-shortener/v1/url/insert", showRequestURL = F)
  expect_more_than(nchar(g2$id), 19)
}) 

