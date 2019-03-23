library(testthat)
library(urlshorteneR)

bitly_token <- readRDS("bitly_token.rds")

test_check("urlshorteneR")
