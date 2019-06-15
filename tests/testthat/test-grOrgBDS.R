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
  expect_error(bitly_retrieve_org(), "organization_id must not be emptry string, NA or NULL")
})

test_that("You can retrieve all organizations", {
  ro <- bitly_retrieve_orgs()
  expect_gte(length(ro$guid), 0)
})

test_that("You can retrieve organizations' shorten counts", {
  organization_guid <- bitly_retrieve_orgs()
  expect_true(is.list(bitly_retrieve_org_shorten_counts(organization_guid$guid)))
})

test_that("You cannot retrieve shorten counts for organization because there is emptry string", {
  expect_error(bitly_retrieve_org_shorten_counts(), "organization_id must not be emptry string, NA or NULL")
})

context("Groups")

test_that("You can retrieve my own group", {
  user_info  <- bitly_user_info()
  expect_equal(dim(bitly_retrieve_group(group_id = user_info$default_group_guid[1]))[2], 8)
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
  expect_gte(dim(rsl)[[2]], 13)
})


test_that("bitly_retrieve_sorted_links works, with hours", {
  user_info  <- bitly_user_info()
  rsl <- bitly_retrieve_sorted_links(user_info$default_group_guid[1], unit = "hour")
  expect_gte(dim(rsl)[[2]], 13)
})

test_that("bitly_update_group works, updating name and org id", {
  ui <- bitly_user_info(showRequestURL = TRUE)
  up_group_orig <- bitly_update_group(group_id = ui$default_group_guid[1], name = "fancy name", organization_id = "asd")
  up_group <- bitly_update_group(group_id = ui$default_group_guid[1], name = "NewGroupName", organization_id = "asd")
  expect_equal(up_group$name, "NewGroupName")
  expect_equal(up_group$is_active, "TRUE")
})


test_that("bitly_retrieve_group_pref can retrieve group prefs", {
  ui  <- bitly_user_info()
  rsl <- bitly_retrieve_group_pref(ui$default_group_guid[1])
  expect_equal(dim(rsl)[2], 2)
  expect_equal(rsl$domain_preference, "bit.ly")
})

test_that("bitly_update_group_pref can update group prefs to bit.ly (j.mp stopped working)", {
  ui  <- bitly_user_info()
  usl <- bitly_update_group_pref(group_id = ui$default_group_guid[1], domain_pref = "bit.ly", showRequestURL = T)
  expect_equal(dim(usl)[2], 2)
  expect_equal(usl$domain_preference, "bit.ly")
})

test_that("bitly_retrieve_links_grouped retrieves bitly links by group, deeplinks are not recieved", {
  ui  <- bitly_user_info()
  rlbg <- bitly_retrieve_links_grouped(group_id = ui$default_group_guid[1], showRequestURL = F, archived = "on")
  expect_equal(length(rlbg$links), 0)
  
  rlbg <- bitly_retrieve_links_grouped(group_id = ui$default_group_guid[1], deeplinks = "off")
  expect_gte(rlbg$pagination$total, 8)
  
  rlbg_cnn <- bitly_retrieve_links_grouped(group_id = ui$default_group_guid[1], search_query = "News")
  expect_gte(rlbg_cnn$pagination$total, 1)
})

test_that("bitly_retrieve_tags can get several tags", {
  ui  <- bitly_user_info()
  tags <- bitly_retrieve_tags(group_id = ui$default_group_guid[1], showRequestURL = T)
  expect_length(tags, 1)
})

test_that("we can get group's click metrics by countries", {
  ui  <- bitly_user_info()
  group_metrics <- bitly_retrieve_group_click_metrics_by_countries(group_id = ui$default_group_guid[1])
  expect_equal(dim(group_metrics)[[2]], 6)
})

test_that("we can get group's click metrics by referring networks", {
  ui  <- bitly_user_info()
  group_metrics_ref_net <- bitly_retrieve_group_click_metrics_by_ref_networks(group_id = ui$default_group_guid[1])
  expect_equal(dim(group_metrics_ref_net)[[2]], 6)
})

test_that("we can get group's shorten counts", {
  ui  <- bitly_user_info()
  sc <- bitly_retrieve_group_shorten_counts(group_id = ui$default_group_guid[1])
  expect_equal(dim(sc)[[2]], 6)
})