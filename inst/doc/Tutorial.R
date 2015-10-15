## ------------------------------------------------------------------------
library(RBitly)

## Old
## rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909") # Now officially deprecated

## Now
options(Bit.ly = "0906523ec6a8c78b33f9310e84e7a5c81e500909", Ow.ly = "F1QH-Q64B-BSBI-JASJ", Goo.gl = "AIzaSyAbJt9APfph1JGIhflkoH9UuGhOACntOjw")

user_Metrics_PopularLinks(unit = "month", units = -1, limit = 100)

## ------------------------------------------------------------------------
link_Metrics_EncodersByCount(link = "http://bit.ly/DPetrov", my_network = "false", limit = 100)

## ------------------------------------------------------------------------
user_Info()

user_TrackingDomains()

## ------------------------------------------------------------------------
bitly_pro_domain(domain = "nyti.ms")

## ------------------------------------------------------------------------
# check later & helm me with your Ow.ly API key. Please do contribute back. 
# links_ShortenOwly("")
# link_Metrics_Owly("")

## ------------------------------------------------------------------------
g2 <- links_ShortenGoogl(longUrl = "https://developers.google.com/url-shortener/v1/url/insert")
g2
g1 <- links_ExpandGoogl(shortUrl = "http://goo.gl/vM0w4", showRequestURL = F)
g1

