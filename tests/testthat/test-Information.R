library(testthat)
library(httr)
library(jsonlite)
library(stringr)
library(urlshorteneR)

context("User Information")

test_that("Return information about a user.", {
  ui <- bitly_user_info()
  expect_equal(dim(ui)[[2]], 11)
})

test_that("User data can be updated", {
  uu <- bitly_update_user(name = "DOma")
  expect_silent(uu)
  expect_warning(bitly_update_user(name = "DOma", default_group_guid = "GroupIDUniqueRetsdat"))
})

test_that("User has a free/premium account", {
  bool_val <- is_bitly_user_premium_holder()
  expect_false(bool_val)
})

context("OAUTH Application Details")

test_that("Application provides some metadata", {
  ad <- bitly_app_details()
  expect_equal(dim(ad)[[2]], 4)
})
