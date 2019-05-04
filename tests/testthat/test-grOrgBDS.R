library(urlshorteneR)
library(httr)
library(jsonlite)
library(testthat)

context("Branded Domain Names")

test_that("BDN call issues a warning message, thus 0 BDNs", {
  expect_warning(bitly_bsds())
})

context("Organization")

test_that("You cannot retrieve specific organization because there is emptry string", {
  expect_error(bitly_retrieve_organization(), "organization_id must not be emptry string, NA or NULL")
})

test_that("You can retrieve all organizations", {
  ro <- bitly_retrieve_organizations()
  expect_gte(length(ro$guid), 0)
})

test_that("You can retrieve organizations' shorten counts", {
  organization_guid <- bitly_retrieve_organizations()
  expect_true(is.list(bitly_org_shorten_counts(organization_guid$guid)))
})

test_that("You cannot retrieve shorten counts for organization because there is emptry string", {
  expect_error(bitly_org_shorten_counts(), "organization_id must not be emptry string, NA or NULL")
})

context("Groups")

test_that("You can retrieve my own group", {
  user_info  <- bitly_user_info()
  expect_equal(dim(bitly_retrieve_group(group_id = user_info$default_group_guid[1]))[2], 8)
})

test_that("You cannot retrieve my own group because of incorrect string", {
  user_info  <- "test"
  expect_error(bitly_retrieve_group(user_info), "group_guid must not be empty string, NA or NULL")
})

test_that("You can retrieve my groups (PLURAL) without specifying org id", {
  user_info  <- bitly_user_info()
  my_group <- bitly_retrieve_group(user_info$default_group_guid[1])
  callm <- bitly_retrieve_groups(organization_id = my_group$organization_guid)
  expect_equal(dim(callm)[2], 9)
})

test_that("You can retrieve my groups (PLURAL) without specifying org id", {
  ret_groups <- bitly_retrieve_groups("")
  expect_equal(dim(ret_groups)[2], 9)
})

test_that("bitly_retrieve_sorted_links works, with day", {
  user_info  <- bitly_user_info()
  rsl <- bitly_retrieve_sorted_links(user_info$default_group_guid[1])
  expect_equal(dim(rsl)[[2]], 13)
})


test_that("bitly_retrieve_sorted_links works, with hours", {
  user_info  <- bitly_user_info()
  rsl <- bitly_retrieve_sorted_links(user_info$default_group_guid[1], unit = "hour")
  expect_equal(dim(rsl)[[2]], 13)
})


---------------------
test_that("bitly_update_group works, updating name and org id", {
  ui <- bitly_user_info(showRequestURL = TRUE)
  up_group <- bitly_update_group(group_id = ui$default_group_guid[1], name = "New Group Name", organization_id = "asd")
  expect_equal(xxx)
})


test_that("bitly_retrieve_group_pref can retrieve group prefs", {
  user_info  <- bitly_user_info()
  rsl <- bitly_retrieve_group_pref(user_info$default_group_guid[1], unit = "hour")
  expect_equal(dim(rsl)[2], 2)
})



