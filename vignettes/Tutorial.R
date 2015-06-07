## ------------------------------------------------------------------------
rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")

user.metrics.popular_links(unit = "month", units = -1, limit = 100)

## ------------------------------------------------------------------------
rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
link.metrics.encoders_by_count(link = "http://bit.ly/DPetrov", my_network = "false", limit = 100)

## ------------------------------------------------------------------------
rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
user.info()

user.tracking_domain_list()

## ------------------------------------------------------------------------
rbitlyApi("0906523ec6a8c78b33f9310e84e7a5c81e500909")
bitly_pro_domain(domain = "nyti.ms")

