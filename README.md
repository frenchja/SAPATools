SAPA Tools
==========

[R](http://www.r-project.org/) package to process data collected by 
the [SAPA Project](https://sapa-project.org).

Installation
------------
The latest stable release may be downloaded from the 
[SAPA Project](https://sapa-project.org):

```r
install.packages('SAPATools',repos='http://sapa-project.org/r/',type='source')
```

If you like playing with updated, bleeding-edge code, you can use 
[`devtools`](https://github.com/hadley/devtools) package, developed by [Hadley Wickam](http://had.co.nz/) and 
[Winston Chang](https://github.com/wch), 
to download the package from my Github repo. Note that because this repo is currently private, 
you'll need to enter your current Github username and password.

```r
# install.packages('devtools')
library(devtools)
install_github(repo='SAPATools',username='frenchja')
```

Using SAPA Tools
----------------
- Check out our [wiki](https://github.com/frenchja/SAPATools/wiki) for the latest tutorials.

Features
--------
- Download SAPA data directly from MySQL or HTML scraping. MySQL is much faster than HTML scraping and preserves the data type, rather than needing to reset `data.frame$date` using `as.Date()` each time...
- Scrub SAPA data of duplicate participants.
- Calculate temperament, ability, and interest norms by age and gender (added by [David M. Condon](https://sapa-project.org/dmc/)).
- SSH tunneling (if not used from SAPA server) using `check.location()`.
- Automatic database downloading based on desired date range (e.g., 5/20/2013).
- Map SAPA data by zipcode (added by [William Revelle](https://personality-project.org/revelle.html)).
- Integrate US census data with SAPA variables. (added by [David M. Condon](https://sapa-project.org/dmc/))
