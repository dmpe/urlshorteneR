[![Build Status](https://travis-ci.org/dmpe/rbitly.svg?branch=master)](https://travis-ci.org/dmpe/rbitly)
[![Coverage Status](https://coveralls.io/repos/dmpe/rbitly/badge.svg?branch=master&service=github)](https://coveralls.io/github/dmpe/rbitly?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/RBitly)](http://cran.r-project.org/package=RBitly)

R package for Bit.ly & j.mp & Ow.ly (:new:) and other custom domains
=======

### Installing

Either see CRAN or use `devtools` to install the latest version from Github:

```
library(devtools)
devtools::install_github("dmpe/rbitly", build_vignettes = TRUE)
```
Tested on Ubuntu 15.04 and Windows 10 (64 bit using [Revolution R Open](http://mran.revolutionanalytics.com/download/))

### Help needed !!!

- Testers with the "enterprise" accounts. Please try using it and then report bugs in <https://github.com/dmpe/rbitly/issues>
- Testing Ow.ly support (this requeres their API key)

### Looking for examples and current implementation status ?

Check the vignette in `vignettes` folder.

### How to contribute 

- Fork the repository
- Create and switch to a new branch `git checkout -b [name_of_your_new_branch]`
- Do the changes (i.e. edit files)
- Push remote branch to your github `git remote add -t [name_of_your_remote]` & `git push -u origin [name_of_your_remote]`
- Create a new pull request (to my `master` branch)

Want a direct push access? No problem, just let me know and become a :family:.

**Also**:
 - Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

### Special thanks to:

- Developed by ([@dmpe](https://www.github.com/dmpe)) :yum:.
- Inspired by [Quandl R API](https://github.com/quandl/R-package/).

## Meta
- Licensed under [Apache License 2.0](https://tldrlegal.com/license/apache-license-2.0-%28apache-2.0%29).
- Please report bugs in <https://github.com/dmpe/rbitly/issues>.


