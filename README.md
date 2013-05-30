SAPA Tools
==========

[R](http://www.r-project.org/) package to process data collected by 
the [SAPA Project](http://www.sapa-project.org).

Installation
------------
The latest stable release may be downloaded from the 
[Personality Project](http://personality-project.org):

```r
install.packages('SAPATools',repos='http://personality-project.org/r/',type='source')
```

If you like playing with updated, bleeding-edge code, you can use Hadley Wickam's
[`devtools`](https://github.com/hadley/devtools) package to download the package from my Github repo. 
Note that since the repository is currently private, you'll need to provide your Github username 
and password.

```r
# install.packages('devtools')
library(devtools)
install_github(repo='SAPATools',username='frenchja',
               auth_user='',password='')
```

Features
--------
- Download SAPA data directly from MySQL.
- Scrub SAPA data of duplicate participants.
- SSH tunneling (if not used from SAPA server) using `check.location()`.
- Automatic database downloaded based on desired date range (e.g., 5/20/2013).

To Do
-----
- [ ] Dupe checking using RIDpage, choose 1st time.  Last entry from RID on second pass.
- [x] Port over clean.sapa() function from HTTP scraping.
- [ ] Evaluate objects in memory for RMySQL connections.
