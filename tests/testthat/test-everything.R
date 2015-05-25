library(testthat)
library(rbitly)
library(httr)


rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")

test_that("Return information about a user.", {
  ui <- user.info()
  expect_output(str(ui), "status_code: int 200")
})


test_that("Returns entries from a user's link history in reverse chronological order.", {
  user.linkH <- user.linkHistory()
  expect_named(ulh, c("keyword_link","archived", "user_ts" ,"title", "created_at", "tags", "modified_at", 
                      "private", "aggregate_link", "long_url", "client_id", "link", "note"))
})

test_that("Returns the number of clicks on a single Bitlink.", {
  lmc <- link.metrics.clicks(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)

})


test_that("Returns metrics about the countries referring click traffic to a single Bitlink.", {
  lmcc <- link.metrics.countries(link = "http://bit.ly/DPetrov", unit = "day", units = -1, limit = 100)
  
})
