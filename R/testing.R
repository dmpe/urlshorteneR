require(jsonlite)
require(httr)
#https://stackoverflow.com/questions/11493425/accessing-the-bitly-oauth2-api-from-r
bitly <- oauth_endpoint(
  authorize = "https://bitly.com/oauth/authorize",
  access = "https://api-ssl.bitly.com/oauth/access_token")

# 2. Register an application at http://dev.bitly.com/my_apps.html
# Insert your values below - if secret is omitted, it will look it up in
# the BITLY_CONSUMER_SECRET environmental variable.
myapp <- oauth_app("bitly", 
                   key = "be03aead58f23bc1aee6e1d7b7a1d99d62f0ede8", # Client ID
                   secret = "29bd6162d81d92c7a0ba146f7dc8504a563ad16a") # Client Secret

bitly_token <- oauth2.0_token(bitly, myapp, cache = FALSE) 

# 4. Use API
req <- GET("https://api-ssl.bit.ly/v3/user/info", query = list(access_token = bitly_token$credentials$access_token))
stop_for_status(req)
content(req)$data$profile_url