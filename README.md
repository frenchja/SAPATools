SAPA Tools
==========

[R](http://www.r-project.org/) package to process data collected by 
the [SAPA Project](http://www.sapa-project.org).

Installation
------------
The latest stable release may be downloaded from the 
[SAPA Project](http://sapa-project.org):

```r
install.packages('SAPATools',repos='http://sapa-project.org/r/',type='source')
```

If you like playing with updated, bleeding-edge code, you can use 
[`devtools`](https://github.com/hadley/devtools) package, developed by [Hadley Wickam](http://had.co.nz/) and 
[Winston Chang](https://github.com/wch), 
to download the package from my Github repo. 

```r
# install.packages('devtools')
library(devtools)
install_github(repo='SAPATools',username='frenchja')
```

Features
--------
- Download SAPA data directly from MySQL or HTML scraping.
- Scrub SAPA data of duplicate participants.
- Calculate temperament, ability, and interest norms by age and gender.
- SSH tunneling (if not used from SAPA server) using `check.location()`.
- Automatic database downloading based on desired date range (e.g., 5/20/2013).
- Map SAPA data by zipcode (added by [William Revelle](http://personality-project.org/revelle.html)).
