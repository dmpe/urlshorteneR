library(testthat)
library(RBitly)
library(httr)
library(jsonlite)
library(stringr)

# rbitlyApi("4cf5947d0721447a803a8d2d1328c5d0c2fff48a")
# Another account just to make sure that we can login using username/password

context("Testing API")

test_that("Login using username and password", {
  apikey <- rbitlyApi_up("asdgasdgadgsdfgasdf@yahoo.com", "qwert123456!")
  expect_equal(nchar(apikey), 40)
})
