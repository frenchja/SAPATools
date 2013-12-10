#!/usr/bin/env Rscript
# Author:  Jason A. French
# Plot SAPA Aspects

plot.aspects <- function(aspect = 'Plasticity', sapa.data = IRTscores, 
                         by = 'discipline', facet = FALSE){
  # This function plots SAPA aspects by discipline or major.
  # TODO:  
  # 1. Facet is broken..
  
  by <<- by
  aspect <<- aspect
  sapa.data[[aspect]] <- scale(sapa.data[[aspect]])
  
  # Check package
  if(!require(ggplot2)){
    stop('Install ggplot2 package.')
  }
  
  switch(by,
         'discipline'={
           discipline.labels <- c('Undecided','Arts','Business','Communications','Community and Social Services','Computer and Information Sciences',
                             'Cultural and Regional Studies','Education','Engineering and Technology','Language and Literature Studies',
                             'Mathematics','Medicine and Allied Health','Natural Sciences','Social Sciences')
           discipline.levels <- c(0,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200)
         },
         'major'={
           discipline.labels <- c('Undecided','Arts','Business','Communications','Community and Social Services','Computer and Information Sciences',
                                  'Cultural and Regional Studies','Education','Engineering and Technology','Language and Literature Studies',
                                  'Mathematics','Medicine and Allied Health','Natural Sciences','Social Sciences')
           discipline.levels <- c(0,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200)
           major.labels <- c('Undecided','Art History','Art Theory and Practice','Arts Education ','Culinary Arts and Sciences',
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
           major.levels <- c(0,1001,1002,1003,1004,1005,1006,1007,1008,1009,1010,1011,1012,1013,1014,1015,1016,1101,1102,1103,1104,1105,
                              1106,1107,1108,1109,1110,1111,1112,1113,1114,1115,1201,1202,1203,1204,1205,1206,1207,1301,1302,1303,1304,1305,
                              1306,1307,1308,1401,1402,1403,1404,1405,1406,1501,1502,1503,1504,1505,1506,1507,1508,1509,1510,1511,1512,1601,
                              1602,1603,1604,1605,1606,1607,1608,1609,1610,1611,1612,1701,1702,1703,1704,1705,1706,1707,1708,1709,1710,1711,
                              1712,1801,1802,1803,1804,1805,1806,1807,1808,1809,1810,1811,1812,1813,1814,1815,1816,1901,1902,1903,1904,1905,
                              1906,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2101,2102,2103,2104,2105,2106,2107,2108,2109,2110,2111,
                              2112,2113,2201,2202,2203,2204,2205,2206,2207,2208,2209,2210,2211,2212,2213)
         },
         'job'={
         } )
  
  sapa.data[[by]] <- factor(as.factor(sapa.data[[by]]), 
                                 levels=get(paste(by,'.levels',sep='')),
                                 labels=get(paste(by,'.labels',sep='')))
  
  if(isTRUE(facet)){
    sapa.data[['discipline']] <- factor(as.factor(sapa.data[['discipline']]),
                                        levels=discipline.levels,
                                        labels=discipline.labels)
  }
  
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
  if(isTRUE(facet)){
    group.data <- summarySE(data = sapa.data, measurevar = aspect, 
                            groupvars = c('discipline','major'), na.rm=TRUE)
  }
#   else if(by == 'major'){
#     group.data <- summarySE(data = sapa.data, measurevar = aspect, 
#                             groupvars = 'major', na.rm=TRUE)
#   } else if(by == 'discipline') {
#     group.data <- summarySE(data = sapa.data, measurevar = aspect, 
#                             groupvars = 'discipline', na.rm=TRUE)
#   } 
  else {
    group.data <- summarySE(data = sapa.data, measurevar = aspect, 
                            groupvars = by, na.rm=TRUE)
  }
  
  # Get rid of majors with NAs
  group.data <- group.data[complete.cases(group.data),]
  
  # Get rid of majors with < 20
  group.data <- subset(x=group.data,subset=group.data$N > 20)
  
  c <- ggplot(group.data, aes(x = reorder(get(by),get(aspect)), 
                              y = get(aspect)))
  
  sapa.plot <- c + geom_pointrange(aes(ymin=get(aspect)-ci, 
                                       ymax=get(aspect)+ci),size=.8) + 
                coord_flip() + 
                xlab(label=paste('Academic',by)) + ylab(label=paste(aspect,'Mean')) + 
                theme_bw(base_size=10) + 
                geom_hline(aes(yintercept = 0),colour="#BB0000", linetype="dashed")
  
  if(isTRUE(facet)){
  sapa.plot <- sapa.plot + facet_wrap(~ discipline)
  }
  # Density Plot
  # c <- ggplot(sapa.data,aes(x=ICAR60)) + geom_density(adjust=5,aes(color=factor(discipline),linetype=factor(discipline)),size=1)  + scale_color_grey()
  print(sapa.plot)
}