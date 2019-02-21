library(testthat)
library(urlshorteneR)

if (interactive()) {
  test_check("urlshorteneR")
  test_package("urlshorteneR")
}
# bitly_token <- bitly_auth(key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8", secret = "b7e4abaf8b26ec4daa92b1e64502736f5cd78899")
# 
# saveRDS(bitly_token, file = "tests/testthat/bitly_token.rds")


## https://gorails.com/setup/ubuntu/15.04
## https://github.com/jennybc/googlesheets/blob/master/internal-projects/10_token-encryption.Rmd
