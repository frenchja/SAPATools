#!/usr/bin/env Rscript
# Author:   Jason A. French & David M. Condon
# Email:    frenchja@u.northwestern.edu
# SAPA Tools:     https://sapa-project.org/r/

getSAPAscrape <- 
function (x, vintage = NULL) 
{
    switch(vintage, oldmain = {
        url <- paste("https://sapa-project.org/data/archive/scraping/main.php?option=", 
            x, sep = "")
    }, oldexp = {
        url <- paste("https://sapa-project.org/data/archive/scraping/exploratory.php?option=", 
            x, sep = "")
    }, current = {
        url <- paste("https://sapa-project.org/data/archive/scraping/SAPAactive.php?option=", 
            x, sep = "")
    }, `NULL` = {
        print("ERROR - must provide the vintage -")
    })
    require(plyr)
    require(RCurl)
    require(XML)
    SAPAhtml <- getURL(url)
    parsed <- htmlTreeParse(SAPAhtml, useInternalNodes = TRUE)
    body <- xpathApply(parsed, "//body")[[1]]
    chunk1 <- as(body, "character")
    chunk2 <- gsub("</noscript>", "%", chunk1[[1]])
    chunk3 <- gsub("^.*?%", "%", chunk2)
    chunk4 <- gsub("%", "\n", chunk3)
    chunk5 <- gsub("<br/>", "<br>", chunk4)
    chunk6 <- gsub("<br />", "<br>", chunk5)
    chunk7 <- gsub("\n", "", chunk6)
    chunk8 <- gsub("</body>", "", chunk7)
    chunk9 <- gsub("<body>", "", chunk7)
    chunk10 <- gsub(",,", ",NA,", chunk9)
    chunk11 <- gsub(",,", ",NA,", chunk10)
    chunk12 <- gsub(",<br>", ",NA<br>", chunk11)
    strings <- strsplit(chunk12, "<br>")[[1]]
    strings.df <- ldply(1:length(strings), function(x) {
        rbind(strsplit(strings[x], ",")[[1]])
    })
    names(strings.df) <- strsplit(strings[1], ",")[[1]]
    y <- strings.df[c(2:nrow(strings.df)), ]
    y <- suppressWarnings(apply(y, 2, as.numeric))
    return(y)
}
  
uniqueSAPAscrape <- 
function (x) 
{
    if ("RIDpage" %in% colnames(x)) {
        x <- x[!duplicated(x[, "RIDpage"]), ]
    }
    x <- x[!duplicated(x[, "RID"], fromLast = TRUE), ]
    return(x)
}

dropSAPAscrape <- 
function (x, max.age, min.age) 
{
    if (missing(min.age)) 
        min.age <- 13
    if (missing(max.age)) 
        max.age <- 91
    x <- subset(x, x[, "age"] > min.age)
    x <- subset(x, x[, "age"] < max.age)
    x <- subset(x, x[, "no_code"] < 1)
    return(x)
}

cleanSAPAscrape <- 
function (x, vintage = NULL, max.age, min.age) 
{
    x <- getSAPAscrape(x, vintage = vintage)
    x <- uniqueSAPAscrape(x)
    x <- dropSAPAscrape(x, min.age = min.age, max.age = max.age)
    return(x)
}
