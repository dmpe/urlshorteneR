[![Build Status](https://travis-ci.org/dmpe/urlshorteneR.svg?branch=master)](https://travis-ci.org/dmpe/urlshorteneR)
[![Coverage Status](https://coveralls.io/repos/dmpe/urlshorteneR/badge.svg?branch=master&service=github)](https://coveralls.io/github/dmpe/urlshorteneR?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/urlshorteneR)](http://cran.r-project.org/package=urlshorteneR)

R package for shortening URLs, supporting `Bit.ly` & `j.mp` & `is.gd` and `v.gd`
=======

### Installing

Either see CRAN or use `devtools` to install the latest version from Github:

```
library(devtools)
devtools::install_github("dmpe/urlshorteneR", build_vignettes = TRUE)
```

Tested on Ubuntu 18.10 and Windows 10 (64 bit)


## Version v3

### What has (not) been implemented for Bit.ly:

- V3-> V4 migration underway

- Based on <http://dev.bitly.com/>

<http://dev.bitly.com/user_info.html>  | Implemented Yes/No
------------- | -------------
/v3/oauth/app  | (**NOT**, see below)
/v3/user/info  | Yes
/v3/user/link_history | Yes
/v3/user/network_history  | **NOT**
/v3/user/tracking_domain_list  | Yes

<http://dev.bitly.com/link_metrics.html>  | Implemented Yes/No
------------- | -------------
/v3/link/clicks  | Yes
/v3/link/countries  | Yes
/v3/link/encoders  | Yes
/v3/link/encoders_by_count  | Yes
/v3/link/encoders_count  | Yes
/v3/link/referrers  | Yes
/v3/link/referrers_by_domain  | Yes
/v3/link/referring_domains  | Yes


<http://dev.bitly.com/user_metrics.html> | Implemented Yes/No | Premium
------------- | ------------- | -------------
/v3/user/clicks  | Yes 
/v3/user/countries  | Yes 
/v3/user/popular_earned_by_clicks  | **NOT**  | Yes
/v3/user/popular_earned_by_shortens  | **NOT**  | Yes
/v3/user/popular_links  | Yes
/v3/user/popular_owned_by_clicks  | **NOT**  | Yes
/v3/user/popular_owned_by_shortens | **NOT**  | Yes
/v3/user/referrers  | Yes
/v3/user/referring_domains | Yes
/v3/user/shorten_counts | Yes


<http://dev.bitly.com/domains.html> | Implemented Yes/No | Premium
------------- | ------------- | -------------
/v3/bitly_pro_domain  | Yes | No
/v3/user/tracking_domain_clicks  | **NOT** | Yes (cannot do unless somebody helps)
/v3/user/tracking_domain_shorten_counts  | **NOT** | Yes (cannot do unless somebody helps)


<http://dev.bitly.com/links.html> | Implemented Yes/No | Premium
------------- | ------------- | -------------
/v3/expand  | Yes 
/v3/info  | Yes 
/v3/link/lookup  |  Yes
/v3/shorten  | Yes  
/v3/user/link_edit  | **NOT**  | No
/v3/user/link_lookup  | **NOT**  | No
/v3/user/link_save | **NOT**  | No
/v3/user/save_custom_domain_keyword | **NOT**  | Yes


- <http://dev.bitly.com/nsq.html>  
    + None endpoints are implemented here, logically. This is only for the [NSQ](http://nsq.io/) platform.

- <http://dev.bitly.com/organization_metrics.html>
    + None endpoints are implemented here, because all are premium. Contact me if you need it. 

- Lastly, sometimes, only a **small subset of input paramaters** has been implemented. Again contact me and I will consider to implement them all. 





### Help needed !!!

- Testers with the "enterprise" Bit.ly accounts. Please try using it and then report bugs in <https://github.com/dmpe/urlshorteneR/issues>
- Testing Ow.ly support (this requires their API key)

### Looking for examples and current implementation status ?

Check the vignette in `vignettes` folder.

### How to contribute 

See `Contribute.md`

### Special thanks to:

- Developed by ([@dmpe](https://www.github.com/dmpe)) and [@DataWookie](https://github.com/DataWookie) :yum:.

## Meta

- Licensed under [Apache License 2.0](https://tldrlegal.com/license/apache-license-2.0-%28apache-2.0%29).
- Please report bugs in <https://github.com/dmpe/urlshorteneR/issues>.


