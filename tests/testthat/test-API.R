library(testthat)
library(RBitly)
library(httr)
library(RCurl)
library(jsonlite)
library(stringr)

# rbitlyApi("4cf5947d0721447a803a8d2d1328c5d0c2fff48a")
# Another account just to make sure that we can login using username/password

context("Testing API")

test_that("Login using username and password", {
  returnApiKey("asdgasdgadgsdfgasdf@yahoo.com", "qwert123456!")
})