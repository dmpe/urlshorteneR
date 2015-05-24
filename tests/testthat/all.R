library(rbitly)
library(httr)
library(RCurl)


req <- POST("https://api-ssl.bitly.com/oauth/access_token", authenticate("cincenko@seznam.cz", "120793", type = "basic"),
            encode = "multipart")
content(req)


rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")



returnApiKey("cincenko@seznam.cz", "120793")