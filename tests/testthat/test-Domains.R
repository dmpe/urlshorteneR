library(testthat)
library(RBitly)
library(httr)
library(RCurl)
library(jsonlite)
library(stringr)

rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")

context("Domains")

test_that("Query whether a given domain is a valid bitly pro domain. ", {
  expect_message(bitly_pro_domain(domain = "nytidsfds.ms"), "NOT") 
})

test_that("Query whether a given domain is a valid bitly pro domain. ", {
  expect_message(bitly_pro_domain(domain = "nyti.ms"), "is") 
})



