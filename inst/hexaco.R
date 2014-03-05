#!/usr/bin/env Rscript
# Author:  Jason A. French and David M. Condon
# HEXACO scores generator

# Install once.  Then comment out.
# devtools::install_github(c("hadley/testthat", "rstudio/shiny", "rstudio/ggvis"))

test.data <- data.frame(Aspect=c('Humility','Emotionality','Extraversion','Agreeableness','Conscientiousness','Openness'),
                        value=c(rnorm(n=6,mean=5,sd=2)))

library(ggvis)
p <- ggvis(test.data, props(x = ~as.factor(Aspect), y = ~value, fill = ~Aspect)) +
  mark_rect(props(y2 = 0, width = band())) +
  dscale("x", "nominal", padding = 0, points = FALSE)+ 
  guide_axis("x", title = "SAPA Aspect") + 
  guide_axis("y", title = "Score") +
  dscale("y", "numeric", domain = c(0, 9), nice = FALSE)

print(p)