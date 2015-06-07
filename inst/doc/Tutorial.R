## ------------------------------------------------------------------------
library(RBitly)
rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")

user_Metrics_PopularLinks(unit = "month", units = -1, limit = 100)

## ------------------------------------------------------------------------
rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
link_Metrics_EncodersByCount(link = "http://bit.ly/DPetrov", my_network = "false", limit = 100)

## ------------------------------------------------------------------------
rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
user_Info()

user_TrackingDomains()

## ------------------------------------------------------------------------
rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
bitly_pro_domain(domain = "nyti.ms")

