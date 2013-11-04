#!/usr/bin/env Rscript
# Author:  Jason A. French
# Plot SAPA Aspects

plot.aspects <- function(aspect = 'Plasticity', sapa.data = IRTscores, 
                         by = 'discipline'){
  # This function plots SAPA aspects by discipline or major.
  # TODO:  
  # 1. scale() mean.
  # 2. Add by switch.
  
  # Check package
  if(!require(ggplot2)){
    stop('Install ggplot2 package.')
  }
  
  switch(by,
         'discipline'={
           factor.labels <- c('Undecided','Arts','Business','Communications','Community and Social Services','Computer and Information Sciences',
                             'Cultural and Regional Studies','Education','Engineering and Technology','Language and Literature Studies',
                             'Mathematics','Medicine and Allied Health','Natural Sciences','Social Sciences')
         },
         'major'={
           factor.labels <- c('Art History','Art Theory and Practice','Arts Education ','Culinary Arts and Sciences',
                              'Dance','Design and Applied Arts','Drama/Theater Arts','Fashion','Fine and Studio Arts',
                              'Graphic Arts','Interior Design','Landscape Design','Music','Music Education',
                              'Performance Studies','Other Performing or Visual Art Major','Accounting','Agricultural Businesses',
                              'Business Administration and Management','Entrepreneurship','Finance and Financial Management',
                              'General Business','Hospitality Administration/Management','Human Resource Administration',
                              'International Business','Labor Relations','Logistics and Supply Chain Management',
                              'Marketing','Real Estate Management and Services','Sales and Marketing Operations',
                              'Other Business Major','Communication Disorders and Services','Communication Sciences',
                              'Communications and Media Studies','Journalism','Public Relations and Advertising',
                              'Radio, Television and Film Communication','Other Communications Major','Criminal Justice and Corrections',
                              'Family and Consumer Science','Human Development and Family Studies','Parks, Recreation and Leisure Studies',
                              'Social Policy','Social Work','Urban and Municipal Planning','Other Community and Social Services Major',
                              'Computer and Information Systems - General','Computer Graphics','Computer Programming',
                              'Electronic Commerce','Management Information Systems','Other Computer and Information Sciences Major',
                              'African-American Studies','American Studies','Asian Studies','Classical Studies','European Studies',
                              'Gender Studies','International Studies','Jewish Studies','Latina and Latino Studies','Middle Eastern Studies',
                              'Urban Studies','Other Cultural and Regional Studies Major','Education Administration','Elementary Education',
                              'Health Education','Kindergarten/Preschool Education','Language Arts Education','Learning and Organizational Change',
                              'Mathematics Education','Physical Education','Science Education','Secondary Education','Special Education',
                              'Other Education Major','Aerospace Engineering','Biomedical Engineering','Chemical and Biological Engineering',
                              'Civil Engineering','Computer Engineering','Electrical Engineering','Environmental Engineering',
                              'Industrial Engineering','Manufacturing and Design Engineering','Materials Science and Engineering',
                              'Mechanical Engineering',
                              'Other Engineering and Technology Major','Asian Languages and Literature','Classical Languages (Latin and Greek)',
                              'Comparative Literature Studies','English','Fiction Writing','French','German','Italian','Linguistics',
                              'Middle East Languages and Literature','Non-Fiction Writing','Poetry Writing','Portuguese','Slavic Languages',
                              'Spanish','Other Language and Literature Studies Major','Actuarial Sciences','Applied Mathematics',
                              'Mathematical Methods in the Social Sciences','Mathematics','Statistics','Other Mathematics Major',
                              'Dentistry','Health Sciences - General','Health Services and Administration','Medical Assisting',
                              'Medical Laboratory/Technology','Medicine (Pre-Med)','Nursing','Nutrition and Wellness',
                              'Pharmacology','Other Medicine and Allied Health Major','Animal Sciences','Astronomy','Biology','Botany','Chemistry',
                              'Cognitive Science','Environmental Sciences','Forensic Science','Geological Sciences','Neuroscience',
                              'Oceanography','Physics','Other Natural Sciences Major','Anthropology','Criminology','Economics','Geography',
                              'Government','History','Law and Legal Studies','Philosophy','Political Science','Psychology','Religion',
                              'Sociology','Other Social Sciences Major')
         },
         'job'={
           factor.labels <- c()
         } )
  
  sapa.data[[by]] <- factor(as.factor(sapa.data[[by]]), 
                                 levels=c(levels(as.factor(sapa.data[[by]]))),
                                 labels=factor.labels) 
  # Winston's Function
  summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                        conf.interval=.95, .drop=TRUE) {
    require(plyr)
    
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
      if (na.rm) sum(!is.na(x))
      else       length(x)
    }
    
    # This is does the summary; it's not easy to understand...
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun= function(xx, col, na.rm) {
                     c( N    = length2(xx[,col], na.rm=na.rm),
                        mean = mean   (xx[,col], na.rm=na.rm),
                        sd   = sd     (xx[,col], na.rm=na.rm)
                     )
                   },
                   measurevar,
                   na.rm
    )
    
    # Rename the "mean" column    
    datac <- rename(datac, c("mean"=measurevar))
    
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    
    return(datac)
  }
  
  group.data <- summarySE(data = sapa.data, measurevar = aspect, 
                          groupvars = 'discipline', na.rm=TRUE)
  
  c <- ggplot(group.data, aes(x = reorder(discipline,get(aspect)), 
                              y = get(aspect)))
  
  sapa.plot <- c + geom_pointrange(aes(ymin=get(aspect)-ci, 
                                       ymax=get(aspect)+ci),size=.8) + coord_flip() + 
    xlab(label='Academic Discipline') + ylab(label=paste(aspect,'Mean')) +
    theme_bw() + 
   geom_hline(aes(yintercept = mean(group.data[[aspect]])),colour="#BB0000", linetype="dashed")
  print(sapa.plot)
}