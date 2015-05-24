library(rbitly)
library(httr)


rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")

test_that("Data about users are returned", {
  ui <- user.info()
  expect_output(str(ui), "status_code: int 200")
})


test_that("Data about links history is parsed correctly", {
  ulh <- user.linkHistory()
  expect_named(ulh, c("keyword_link","archived", "user_ts" ,"title", "created_at", "tags", "modified_at", 
                      "private", "aggregate_link", "long_url", "client_id", "link", "note"))
})

