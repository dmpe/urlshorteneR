[![CircleCI](https://circleci.com/gh/dmpe/urlshorteneR/tree/master.svg?style=svg)](https://circleci.com/gh/dmpe/urlshorteneR/tree/master)
[![codecov](https://codecov.io/gh/dmpe/urlshorteneR/branch/master/graph/badge.svg)](https://codecov.io/gh/dmpe/urlshorteneR)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/urlshorteneR)](http://cran.r-project.org/package=urlshorteneR)

R package for shortening URLs, supporting `Bit.ly` and `is.gd`/`v.gd` 
=======

## Installing

Either see CRAN badge above or use `devtools` to install the latest version from Github:

```
library(devtools)
devtools::install_github("dmpe/urlshorteneR", build_vignettes = FALSE)
```

### What has (🚫t) been implemented for Bit.ly:

- Based on <https://dev.bitly.com/v4_documentation.html>

#### FINISHED (with testing)
 
<https://dev.bitly.com/v4/#tag/Organizations> DONE | Implemented :heavy_check_mark:
------------ | -------------
GET https://dev.bitly.com/v4/#operation/getOrganization | :heavy_check_mark:
GET https://dev.bitly.com/v4/#operation/getOrganizations | :heavy_check_mark:
GET https://dev.bitly.com/v4/#operation/getOrganizationShortenCounts | :heavy_check_mark:

<https://dev.bitly.com/v4/#tag/BSDs> DONE | Implemented :heavy_check_mark:
----------- | -------------
GET https://dev.bitly.com/v4/#operation/getBSDs | :heavy_check_mark:

<https://dev.bitly.com/v4/#operation/getOAuthApp> DONE | Implemented :heavy_check_mark:
------------- | ------------- 
GET https://api-ssl.bitly.com/v4/apps/{client_id} | :heavy_check_mark: 

<https://dev.bitly.com/v4/#tag/User> DONE | Implemented :heavy_check_mark:
------------- | -------------
PATCH https://api-ssl.bitly.com/v4/user | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/user | :heavy_check_mark: 

<https://dev.bitly.com/v4/#tag/Groups> DONE | Implemented :heavy_check_mark:
------------- | -------------
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/preferences | :heavy_check_mark:
PATCH https://api-ssl.bitly.com/v4/groups/{group_guid}/preferences | :heavy_check_mark: 
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/bitlinks | :heavy_check_mark: 
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/tags | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/countries | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/referring_networks | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/shorten_counts | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/bitlinks/{sort} | :heavy_check_mark:
PATCH https://api-ssl.bitly.com/v4/groups/{group_guid} | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}  | :heavy_check_mark:

<https://dev.bitly.com/v4/#tag/Campaigns> DONE | Implemented :heavy_check_mark:/🚫
------------- | -------------
GET https://api-ssl.bitly.com/v4/campaigns | :heavy_check_mark:
POST https://api-ssl.bitly.com/v4/campaigns | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/channels | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/campaigns/{campaign_guid} | :heavy_check_mark:
PATCH https://api-ssl.bitly.com/v4/campaigns/{campaign_guid} | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/channels/{channel_guid} | :heavy_check_mark:
PATCH https://api-ssl.bitly.com/v4/channels/{channel_guid} | :heavy_check_mark:

<https://dev.bitly.com/v4/#tag/Custom-Bitlinks> DONE | Implemented :heavy_check_mark:/🚫
------------- | -------------
GETPOST https://api-ssl.bitly.com/v4/custom_bitlinks | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/custom_bitlinks/{custom_bitlink}/clicks_by_destination | :heavy_check_mark:
PATCH https://api-ssl.bitly.com/v4/custom_bitlinks/{custom_bitlink} | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/custom_bitlinks/{custom_bitlink} | :heavy_check_mark:

# TODO

<https//dev.bitly.com/v4/#tag/Bitlinks> | Implemented :heavy_check_mark:/🚫
------------- | -------------
POST https://api-ssl.bitly.com/v4/bitlinks | :heavy_check_mark:
POST https://api-ssl.bitly.com/v4/shorten | :heavy_check_mark:
POST https://api-ssl.bitly.com/v4/expand | :heavy_check_mark:
PATCH https://api-ssl.bitly.com/v4/bitlinks/{bitlink} | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/bitlinks/{bitlink}/referrers | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/bitlinks/{bitlink}/clicks/summary | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/bitlinks/{bitlink}/clicks | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/bitlinks/{bitlink}/countries | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/bitlinks/{bitlink} | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/bitlinks/{bitlink}/referrers_by_domains | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/bitlinks/{bitlink}/referring_domains | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/bitlinks/{sort} | 
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/bitlinks | :heavy_check_mark:


## Help needed !!!

- Testers with the "enterprise" Bit.ly accounts. 
- Testing Ow.ly support (this requires their API key)

### Looking for examples and current implementation status ?

Check the vignette in `vignettes` folder.

### How to contribute 

See `Contribute.md`

### Credits:

- Developed by ([@dmpe](https://www.github.com/dmpe))
- Contributions by [@DataWookie](https://github.com/DataWookie) and [@RickPack](https://github.com/RickPack) :yum:. 

### Meta

- Licensed under [Apache License 2.0](https://tldrlegal.com/license/apache-license-2.0-%28apache-2.0%29).
- Report bugs in <https://github.com/dmpe/urlshorteneR/issues>.


