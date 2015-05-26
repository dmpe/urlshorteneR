R package for Bit.ly & Bitly.com & j.mp and other custom domains
============
Licensed under [MIT](http://en.wikipedia.org/wiki/MIT_License)

### What is implemented:

- <http://dev.bitly.com/user_info.html>
    + /v3/user/info
    + /v3/user/link_history
    
- <http://dev.bitly.com/link_metrics.html>
    + /v3/link/clicks
    + /v3/link/countries
    + /v3/link/encoders_count
    + /v3/link/referring_domains
    
- <http://dev.bitly.com/user_metrics.html>
    + /v3/user/clicks
    + /v3/user/countries
    
- <http://dev.bitly.com/organization_metrics.html>
    +

 
### What is **not** implemented (maybe #todo)

- <http://dev.bitly.com/links.html>
    + None endpoints are implemented here. 
    
    
- At the moment no OAuth support -> only basic [HTTP Basic Authentication Flow](http://dev.bitly.com/authentication.html#basicauth)


### How to Contribute

- Fork the repository
- Create and switch to a new branch `git checkout -b [name_of_your_new_branch]`
- Do the changes (i.e. edit files)
- Push remote branch to your github `git remote add [name_of_your_remote]` & `git push origin [name_of_your_remote]`
- Create a new pull request (to my `master` branch)


### Special Thanks to:

- Myself (@dmpe) :yum:
- Somewhat (well, quite a lot) inspired by [Quandl Api R](https://github.com/quandl/R-package/)









