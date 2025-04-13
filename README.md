[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/urlshorteneR)](https://cran.r-project.org/package=urlshorteneR)

R package for shortening URLs, supporting `Bit.ly` and `is.gd`/`v.gd` 
=======

## Installing

Either see CRAN badge above or use `devtools` to install the latest version from Github:

```
library(devtools)
devtools::install_github("dmpe/urlshorteneR", build_vignettes = FALSE)
```

### What has (🚫t) been implemented for Bit.ly:

- Based on <https://dev.bitly.com/>

#### FINISHED (with testing)
 
Organizations DONE | Implemented :heavy_check_mark:
------------ | -------------
GET https://dev.bitly.com/api-reference/#getOrganization | :heavy_check_mark:
GET https://dev.bitly.com/api-reference/#getOrganizations | :heavy_check_mark:
GET https://dev.bitly.com/api-reference/#getOrganizationShortenCounts | :heavy_check_mark:
GET https://dev.bitly.com/api-reference/#getPlanLimits | :heavy_check_mark:
GET https://dev.bitly.com/api-reference/#getOrganizationShortenCountsByGroup | :heavy_check_mark:

https://dev.bitly.com/v4/#tag/BSDs DONE | Implemented :heavy_check_mark:
----------- | -------------
GET https://dev.bitly.com/api-reference/#getBSDs | :heavy_check_mark:
GET https://dev.bitly.com/api-reference/#getOverridesForGroups | :heavy_check_mark:

https://dev.bitly.com/api-reference#getOAuthApp DONE | Implemented :heavy_check_mark:
------------- | ------------- 
GET https://api-ssl.bitly.com/v4/apps/{client_id} | :heavy_check_mark: 

https://dev.bitly.com/v4/#tag/User DONE | Implemented :heavy_check_mark:
------------- | -------------
PATCH https://api-ssl.bitly.com/v4/user | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/user | :heavy_check_mark: 
GET https://api-ssl.bitly.com/v4/user/platform_limits | :heavy_check_mark: 

https://dev.bitly.com/v4/#tag/Groups DONE | Implemented :heavy_check_mark:
------------- | -------------
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/preferences | :heavy_check_mark:
PATCH https://api-ssl.bitly.com/v4/groups/{group_guid}/preferences | :heavy_check_mark: 
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/bitlinks | :heavy_check_mark: 
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/tags | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/countries | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/cities | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/devices | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/referring_networks | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/shorten_counts | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/bitlinks/{sort} | :heavy_check_mark:
PATCH https://api-ssl.bitly.com/v4/groups/{group_guid} | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}  | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/feature_usage | :heavy_check_mark:


https://dev.bitly.com/v4/#tag/Campaigns DONE | Implemented :heavy_check_mark:/🚫
------------- | -------------
GET https://api-ssl.bitly.com/v4/campaigns | :heavy_check_mark:
POST https://api-ssl.bitly.com/v4/campaigns | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/channels | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/campaigns/{campaign_guid} | :heavy_check_mark:
PATCH https://api-ssl.bitly.com/v4/campaigns/{campaign_guid} | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/channels/{channel_guid} | :heavy_check_mark:
PATCH https://api-ssl.bitly.com/v4/channels/{channel_guid} | :heavy_check_mark:

https://dev.bitly.com/v4/#tag/Custom-Bitlinks DONE | Implemented :heavy_check_mark:/🚫
------------- | -------------
GETPOST https://api-ssl.bitly.com/v4/custom_bitlinks | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/custom_bitlinks/{custom_bitlink}/clicks_by_destination | :heavy_check_mark:
PATCH https://api-ssl.bitly.com/v4/custom_bitlinks/{custom_bitlink} | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/custom_bitlinks/{custom_bitlink} | :heavy_check_mark:

https//dev.bitly.com/v4/#tag/Bitlinks | Implemented :heavy_check_mark:/🚫
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
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/bitlinks/{sort} | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}/bitlinks | :heavy_check_mark:

https://dev.bitly.com/api-reference/#createQRCodePublic Not finished | Implemented :heavy_check_mark:/🚫
------------- | -------------
GET https://api-ssl.bitly.com/v4/qr-codes | :heavy_check_mark:


## Help needed !!!

- Testers with the "enterprise" Bit.ly accounts. 
- Testing Ow.ly support (this requires their API key)

### Looking for examples and current implementation status ?

Check the vignette in `vignettes` folder.

### How to contribute 

See `Contribute.md`

### Credits:

- Developed by ([@dmpe](https://github.com/dmpe))
- Contributions by [@DataWookie](https://github.com/DataWookie) and [@RickPack](https://github.com/RickPack) :yum:. 

### Meta

- Licensed under [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0).
- Report bugs in <https://github.com/dmpe/urlshorteneR/issues>.


