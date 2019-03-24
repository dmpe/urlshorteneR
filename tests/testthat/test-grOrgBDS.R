library(urlshorteneR)
library(httr)
library(jsonlite)
library(testthat)

bitly_token <- bitly_auth()

context("Branded Domain Names")

test_that("BDN do work", {
  fdm <- bsds()
  expect_equal(length(as.list(fdm)), 0)
})

context("Organization")

test_that("You can retrieve specific organization", {
  organization_guid <- ""
  ro <- bitly_retrieve_organization(organization_guid)
  expect_identical(ro$guid, organization_guid)
})


test_that("You can retrieve all organizations", {
  ros <- bitly_retrieve_organizations()
  expect_gte(length(ros$guid), 0)
})

test_that("You can retrieve organizations' shorten counts", {
  organization_guid <- ""
  expect_success(org_shorten_counts(organization_guid))
})

context("Groups")







