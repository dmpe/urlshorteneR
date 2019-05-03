library(testthat)
library(urlshorteneR)

# token <- bitly_auth()
# saveRDS(bitly_token, file = "bitly_local_token.rds")

bitly_token <- readRDS("tests/bitly_local_token.rds")

test_check("urlshorteneR")
