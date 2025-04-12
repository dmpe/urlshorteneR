library(testthat)
library(httr2)
library(jsonlite)
library(stringr)
library(stringi)
library(urlshorteneR)
library(dotenv)

dotenv::load_dot_env("r.secret")
context("User Information")

test_that("Return information about a user.", {
  ui <- bitly_user_info(access_token = Sys.getenv("access_token"))
  expect_equal(dim(ui)[[2]], 11)
})

test_that("User data can be updated", {
  uu <- bitly_update_user(access_token = Sys.getenv("access_token"), name = "Doma")
  expect_silent(uu)
  expect_warning(bitly_update_user(access_token = Sys.getenv("access_token"), name = stri_rand_strings(1, 5, pattern = "[A-Za-z]"), default_group_guid = "GroupIDUniqueRetsdat"))
})

test_that("User has a free/premium account", {
  bool_val <- is_bitly_user_premium_holder(access_token = Sys.getenv("access_token"))
  expect_false(bool_val)
})

context("OAUTH Application Details")

test_that("Application provides some metadata", {
  ad <- bitly_app_details(access_token = Sys.getenv("access_token"))
  expect_equal(dim(ad)[[2]], 5)
})
