SAPA Tools
==========

[R](http://www.r-project.org/) script to process data collected by 
the [SAPA Project](http://www.sapa-project.org).

Installation
------------
The latest stable release may be downloaded from the 
[Personality Project](http://personality-project.org):

```r
install.packages('SAPATools',repos='http://personality-project.org/r/',type='source')
```

If you like playing with updated, bleeding-edge code, you can use Hadley Wickam's
`devtools` package to download the package from my Github repo:

```r
# install.packages('devtools')
library(devtools)
install_github(repo='SAPATools',username='frenchja',
               auth_user='',password='')
```

To Do
-----

1.  Dupe checking using RIDpage, choose 1st time.  Last entry from RID on second pass.