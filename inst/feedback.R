#!/usr/bin/env Rscript
# Author:  Jason A. French
# 12 aspect scores must be passed alphabetically (see line 9)

RID <- commandArgs(trailingOnly=TRUE)[1]
Scores <-as.numeric(commandArgs(trailingOnly=TRUE)[-1])
# Random data for testing
if(length(Scores)< 12){
  Scores <-runif(12,1,9)
}
aspects <- data.frame(
  Traits=c('Assertiveness','Balance','Boldness','Compassion','Honesty',
                   'Humility','Industriousness','Intellect','Openness','Orderliness',
                   'Politeness','Sociability'),

  Score=Scores,
  Factor=c('Extraversion','Emotional Stability','Emotional Stability','Agreeableness','Integrity',
           'Integrity','Conscientiousness','Openness','Openness','Conscientiousness',
           'Agreeableness','Extraversion'))

# Return range limits for dnorm()
limitRange <- function(fun, max) {
  function(x) {
    y <- fun(x)
    y[x > max ] <- NA
    return(y)
  }
}

# Bill hates this, but if this is running in a production server
# it's necessary.
if(!require(ggplot2)){
  install.packages('ggplot2')
  require(ggplot2)
}
p <- ggplot(aspects, aes(x=Score)) +
  stat_function(fun = dnorm, args = list(mean = 5, sd = 2), colour = 'black') + 
  #stat_function(fun = limitRange(dnorm, 1), 
  #              geom='area', alpha=0.2, 
  #              data=aspects) +
  geom_vline(aes(xintercept = Score, colour = factor(Factor) ), aspects, size=1.5) + 
  facet_wrap( ~ Factor + Traits,ncol=2) + 
  scale_x_continuous(limits=c(1,9),breaks=c(2,3,4,5,6,7,8)) +
  ylab('% of People') + 
  xlab('Aspect Score') +
  ggtitle("Here are your 12 aspect scores! How do you compare?\n https://sapa-project.org") +
  theme_bw()

ggsave(plot=p,
       filename=paste('~/Desktop/',RID,'.pdf',sep=""),
       width=8.5, height=11)