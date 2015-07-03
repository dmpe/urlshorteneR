## Test environments
* linux: R 3.2.1 (and travis) @ Ubuntu 15.04
* windows: Revolution R Pro 3.2.0 @ Windows 8.1

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 2 NOTEs:

* No repository set, so cyclic dependency check skipped

* Maintainer: ‘John Malc <cincenko@outlook.com>’
New submission
Found the following (possibly) invalid URLs:
  URL: http://cran.r-project.org/web/packages/RBitly
    From: README.md
    Status: 404
    Message: Not Found
    CRAN URL not in canonical form

  The canonical URL of the CRAN page for a package is 
  http://cran.r-project.org/package=pkgname

## Resubmission
This is a resubmission. In this version I have:

* Fixed mis-spelled word in DESCRIPTION: retreive (4:76). Thanks!

* Fixed non-standard file/directory found at top level: ‘ReadMe.md’ -> README.md

* rbitlyApi: no visible global function definition for ‘assignInMyNamespace’:
Undefined global functions or variables:
    - @Mr. Hornik: Here I haven't been able to reproduce this note/error/warning, neither with --as-cran nor with some other options. Thus, I am unsure what my solution should be. However, I have added `@importFrom utils assignInMyNamespace` to the NAMESPACE. 
