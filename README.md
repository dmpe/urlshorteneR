[![Coverage Status](https://coveralls.io/repos/dmpe/urlshorteneR/badge.svg?branch=master&service=github)](https://coveralls.io/github/dmpe/urlshorteneR?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/urlshorteneR)](http://cran.r-project.org/package=urlshorteneR)
[![CircleCI](https://circleci.com/gh/dmpe/urlshorteneR/tree/master.svg?style=svg)](https://circleci.com/gh/dmpe/urlshorteneR/tree/master)
[![Maintainability](https://api.codeclimate.com/v1/badges/369c2a0a1ba107e0c98c/maintainability)](https://codeclimate.com/github/dmpe/urlshorteneR/maintainability)

R package for shortening URLs, supporting `Bit.ly`/`j.mp` and `is.gd`/`v.gd`
=======

### Installing

Either see CRAN badge above or use `devtools` to install the latest version from Github:

```
library(devtools)
devtools::install_github("dmpe/urlshorteneR", build_vignettes = TRUE)
```

## Version v4 Migration is underway

### What has (🚫t) been implemented for Bit.ly:

- Based on <https://dev.bitly.com/v4_documentation.html>

<https://dev.bitly.com/v4/#tag/User> DONE | Implemented and tested 
------------- | -------------
PATCH https://api-ssl.bitly.com/v4/user | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/user | :heavy_check_mark: |

<https://dev.bitly.com/v4/#tag/Groups>  | Implemented :heavy_check_mark:/🚫
------------- | -------------
GET https://api-ssl.bitly.com/v4/groups | :heavy_check_mark:
GET https://api-ssl.bitly.com/v4/groups/{group_guid}  | :heavy_check_mark:

<https://dev.bitly.com/v4/#tag/Organizations> DONE | Implemented :heavy_check_mark:/🚫
------------- | -------------
GET https://dev.bitly.com/v4/#operation/getOrganization | :heavy_check_mark:
GET https://dev.bitly.com/v4/#operation/getOrganizations | :heavy_check_mark:
GET https://dev.bitly.com/v4/#operation/getOrganizationShortenCounts | :heavy_check_mark:

<https://dev.bitly.com/v4/#tag/Campaigns> | Implemented :heavy_check_mark:/🚫
------------- | -------------
GET https://dev.bitly.com/v4/#operation/getCampaigns | :heavy_check_mark: but 🚫t tested
POST https://dev.bitly.com/v4/#operation/createCampaign | :heavy_check_mark: but 🚫t tested

<https://dev.bitly.com/v4/#tag/BSDs> DONE | Implemented :heavy_check_mark:/🚫
------------- | -------------
GET https://dev.bitly.com/v4/#operation/getBSDs | :heavy_check_mark:

<https://dev.bitly.com/v4/#operation/getOAuthApp> DONE | Implemented :heavy_check_mark:/🚫 
------------- | ------------- 
GET https://api-ssl.bitly.com/v4/apps/{client_id} | :heavy_check_mark: 


### Help needed !!!

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


