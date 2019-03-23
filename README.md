[![Coverage Status](https://coveralls.io/repos/dmpe/urlshorteneR/badge.svg?branch=master&service=github)](https://coveralls.io/github/dmpe/urlshorteneR?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/urlshorteneR)](http://cran.r-project.org/package=urlshorteneR)

R package for shortening URLs, supporting `Bit.ly`/`j.mp` and `is.gd`/`v.gd`
=======

### Installing

Either see CRAN badge above or use `devtools` to install the latest version from Github:

```
library(devtools)
devtools::install_github("dmpe/urlshorteneR", build_vignettes = TRUE)
```

Tested on Ubuntu 18.10 and Windows 10 (64 bit)

## Version v3

### What has (not) been implemented for Bit.ly:

- V3 -> V4 migration underway

- Based on <https://dev.bitly.com/v4_documentation.html>

<https://dev.bitly.com/v4/#tag/User>  | Implemented Yes/No
------------- | -------------
PATCH https://api-ssl.bitly.com/v4/user | Yes
GET https://api-ssl.bitly.com/v4/user | Yes

<https://dev.bitly.com/v4/#tag/Groups>  | Implemented Yes/No
------------- | -------------
GET https://api-ssl.bitly.com/v4/groups | Yes
GET https://api-ssl.bitly.com/v4/groups/{group_guid}  | Yes


<https://dev.bitly.com/v4/#tag/Organizations> | Implemented Yes/No
------------- | -------------
GET https://dev.bitly.com/v4/#operation/getOrganization | Yes
GET https://dev.bitly.com/v4/#operation/getOrganizations | Yes
GET https://dev.bitly.com/v4/#operation/getOrganizationShortenCounts | Yes

<https://dev.bitly.com/v4/#tag/BSDs> | Implemented Yes/No
------------- | -------------
GET https://dev.bitly.com/v4/#operation/getBSDs | Yes

<https://dev.bitly.com/v4/#operation/getOAuthApp> | Implemented Yes/No 
------------- | ------------- | -------------
GET https://api-ssl.bitly.com/v4/apps/{client_id} | Yes 

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


