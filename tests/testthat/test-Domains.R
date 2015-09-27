library(testthat)
library(urlshorteneR)
library(httr)
library(jsonlite)
library(stringr)

bitly_token <- bitly_auth(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8", secret = "b7e4abaf8b26ec4daa92b1e64502736f5cd78899")

context("Domains")

test_that("Query whether a given domain is a valid bitly pro domain. ", {
  expect_message(bitly_pro_domain(domain = "nytidsfds.ms"), "NOT")
  expect_message(bitly_pro_domain(domain = "nyti.ms"), "is") 
})



