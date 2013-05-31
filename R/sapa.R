#!/usr/bin/env Rscript
# Author:   Jason A. French & David M. Condon
# Email:    frenchja@u.northwestern.edu
# SAPA:     http://sapa-project.org

# Check packages
if (!require(RMySQL)) {
    stop('RMySQL not found. Please install!')
}

check.location <- function() {
  # Check hostname and location
  if(system('hostname', intern=TRUE) != 'revelle.ci.northwestern.edu'){
    warning('This script is meant be run from the SAPA Project server!\n',
            immediate.=TRUE)
    switch(Sys.info()['sysname'],
           Windows={
             stop('Sorry!  Windows doesn\'t do SSH tunneling!')},
           {choice <- menu(choices=c('Yes','No'),
                           title='Do you want to try tunneling over SSH?')
            if(choice == 1 & nchar(Sys.which('ssh')) > 0) {
              message(paste('SSH located at ',Sys.which('ssh'),'. Connecting.', sep=''))
              user <- readline(prompt='Enter your NetID: ')
              cmd <- paste('ssh -fNg -L 3306:127.0.0.1:3306 ',
                           user,'@revelle.ci.northwestern.edu',
                           sep='')
              system(cmd) # wait=TRUE?
              return(TRUE)
              } else {
                stop('Cannot proceed! Either run this script from the server or tunnel using SSH!')
              }
           } 
    )
  } else {
    return(TRUE)
  }
}

# Setup MySQL connections
# con.list <- c('SAPAactive','SAPAcurrent','SAPAarchive')
# con.connect <- function(x) {
#   paste('con.',
#         x,
#         " <- dbConnect(MySQL(),user='username',password='password',dbname='",
#         x,
#         "',host='localhost')",sep="")
# }
# 
# lapply(X=con.list,
#        FUN=con.connect)

sapa.db <- function(database,all=FALSE) {
  # Establishes connect to SAPA MySQL database
  #
  # Args:
  #   database: Database to connect
  #   all:      Connect to all databases?
  #
  # Returns: RMySQL connection
  
  check.location()
  
  # Check if database argument passed
  if (!hasArg(database)) {
    database <- select.list(choices=c('SAPAactive','SAPAcurrent','SAPAarchive'),
                            title='Choose the database to connect: ',
                            multiple=FALSE)
  }
  
  # Check that database argument is character string
  if (!is.character(database)) {
    warning('Something is wrong with your database argument. Attempting to convert it to a string!',
            immediate.=TRUE)
    database <- toString(database)
  }
  
  print(paste('Connecting to',database))
  
  # Connect to database
  con <- switch(database,
         'SAPAactive'={dbConnect(MySQL(),
                                    user="username",
                                    password="password",
                                    dbname="SAPAactive",
                                    host="127.0.0.1",
                                    port=3306)
         },
         'SAPAarchive'={dbConnect(MySQL(),
                                     user="username",
                                     password="password",
                                     dbname="SAPAarchive",
                                     host="127.0.0.1",
                                     port=3306)
         },
         'SAPAcurrent'={dbConnect(MySQL(),
                                     user="username",
                                     password="password",
                                     dbname="SAPAcurrent",
                                     host="127.0.0.1",
                                     port=3306)
         })
  return(con)
}

sapa.table <- function(table.name,con,all=FALSE) {
  # Imports a SAPA table from MySQL into R
  #
  # Args:
  #   table.name:  MySQL table name
  #   all:    Import all tables into R
  #   con:    RMySQL connection
  #
  # Returns:  R data.frame
  
  # Check if table argument passed
  if (!hasArg(table.name)) {
    # Choose Tables to export to R
    table.choice <- select.list(choices=dbListTables(con),
                                title='Choose which table to convert to a data.frame: ',
                                multiple=TRUE)
  } else{
    # Check that sapa.table argument is valid
    tryCatch({
      is.element(el=table,set=dbListTables(con))
    },
             error =  function(e) {
               warning('Invalid table choice!',immediate.=TRUE)
               # Choose Tables to export to R
               table.choice <- select.list(choices=dbListTables(con),
                                    title='Choose which table to convert to a data.frame: ')
             }
    )
    table.choice <- toString(table.name)
  }
  
  # Check for active RMySQL connection
  if (!hasArg(con)) {
    message('RMySQL connection not specified. Calling sapa.db() for you.')
    # Could use dbListConnections(MySQL())
    con <- sapa.db()
  }
  
  write.name <- readline(prompt='Choose a name for your table in R: ')
  data <- dbReadTable(conn=con,name=table.choice)
  
  table.write <- menu(choices=c('Yes','No'),
                      title='Would you like to save your table to disk?')
  switch(table.write,
         {write.table(x=table.name[1],
                        file=paste(write.name,Sys.Date,'.data',sep=''))
         },
         {warning('Data not saved to disk yet...')
         })
  return(data)
  on.exit(dbDisconnect(con))
}

clean.sapa <- function(x, max.age=91, min.age=13) {
  # Returns data.frame of unique participants
  #
  # Args:
  #   x:            SAPA data.frame from sapa.table()
  #   max.age:      Max age to allow
  #   min.age:      Min age to allow
  
  # Take first RIDpage entry if duplicates
  x <- x[!duplicated(x[,'RIDpage']),]
  # Take last RID entry (i.e., latest page) if duplicates
  x <- x[!duplicated(x[,'RID'],fromLast=TRUE),]
  
  # Approach 2:  Suggested by Winston Chang
  # dat2 <- dat[order(dat$PID, dat$time),] or dat2 <- dat[order(dat$PID, dat$Page),]
  # lastPID <- !rev(duplicated(rev(dat2$PID)))
  # dat2[lastPID,]
  
  # Approach 3:  Using tapply() and $time
  # indices <- tapply(seq_along(time), pid, function(x) { x[which.max(time[x])] })
  # indices <- tapply(seq_along(x$time),
  #                  x$RIDpage,
  #                  FUN=function(x) { x[which.max(sapa.jason$time[x])] })

  # Scrub page ages
  if (missing(min.age) | is.null(min.age))
    min.age <- 13
  if (missing(max.age) | is.null(max.age))
    max.age <- 91
  x <- x[which(x$age > min.age & x$age<max.age & x$no_code<1),]
  return(x)
}

get.sapa <- function(date='2013-05-20',filename) {
  # Get SAPA Project data
  #
  # Args:
  #   date:   Minimum date cutoff (YYYY-MM-DD)
  #
  # Returns:  data.frame

  # Check for date argument
  if(hasArg(date)){
    print(paste('Obtaining SAPA date from ',date))
    date <- as.Date(date,format="%Y-%m-%d")
  } else {
    warning('No date supplied.  Using 05/20/2013!')
    date <- as.Date('2013-5-20',format="%Y-%m-%d")
  }
  
  # List of tables on SAPAactive
  sapa.active.list <- c('TAIE_responses_052013','CAI_responses_052013',
                         'emotion_responses_052013','health_responses_052013',
                         'peer_responses_052013')
  # Establish connection to SAPAactive
  con.active <- sapa.db(database=SAPAactive)
  
  list.of.data.frames <- as.list(sapa.active.list)
  # Vectorize list of tables
  lapply(X=sapa.active,
         FUN=function(x){           
           x <- sapa.table(sapa.table=x,con=con.active)
         })
  # Close db connection
  dbDisconnect(con.active)
  
  # Use different MySQL database than SAPAactive if needed
  if(date < as.Date('2013-5-20',format="%Y-%m-%d")) {
    sapa.archive.list <- c('b5_responses_011112', 'b5_responses_012513', 'b5_responses_071712',
                           'b5_responses_100312', 'exp_responses_011111', 'exp_responses_011112',
                           'exp_responses_012213', 'exp_responses_052011', 'exp_responses_070111',
                           'exp_responses_071712', 'exp_responses_081612', 'exp_responses_081810',
                           'exp_responses_090512', 'exp_responses_090611open', 'exp_responses_090611orvis',
                           'exp_responses_090611other', 'exp_responses_092010a', 'exp_responses_092010b',
                           'exp_responses_092010c', 'exp_responses_100312', 'exp_responses_101812',
                           'exp_responses_111811', 'iq_responses_011112', 'iq_responses_012213',
                           'iq_responses_050511', 'iq_responses_052611', 'iq_responses_06',            
                           'iq_responses_071712', 'iq_responses_080812',        
                           'iq_responses_083112', 'iq_responses_100312', 'ws_responses_011112',
                           'ws_responses_071712')
    lapply(X=sapa.archive.list,
           FUN=function(x){
            x <- sapa.table(sapa.table=x,con=con.archive)
            list.of.data.frames <- c(list.of.data.frames,x)
           })
    # Close db connection
    dbDisconnect(con.archive)
  }
  
  data$time <- as.Date(data$time,format="%Y-%m-%d %H:%M:%S")
  
  merged.data.frame = Reduce(function(...) merge(..., all=T,by="RID"), list.of.data.frames)
  
  # Save .rdata if argument passed
  if(hasArg(filename)){
    print(paste('Writing SAPA to ',filename))
    save(merged.data.frame,
         file=filename)
  } else {
    warning('No filename supplied.  Outputting as object!')
  }
  return(merged.data.frame)
}

make.sapa <- function(file){
  # Builds the SAPA.rdata, scoring the IQ items.
  #
  # Args:
  #   file: Specify filename output
  #
  # Returns:
  #   data.frame in the sapa.rdata file
  
  # Begin with SAPA Archive SQL table
  con.archive <- sapa.db(database='SAPAarchive')
  
  # List of IQ Tables to pass to lapply()
  iq.tables <- c('iq_responses_011112','iq_responses_012213','iq_responses_050511',
                 'iq_responses_052611','')
  
  # Vectorize the tables
  lapply(X=iq.tables,
         FUN=function(x){
           x <- sapa.table(sapa.table=x,con=con.archive)
           return(x)
         })
  
  # Check file argument
  if(!hasArg(file)) {
    filename <- paste('sapa.',Sys.Date(),'.rdata',sep="")
  } else {
    filename <- file
  }
  
  # Save the Big Kahuna with today's date
  save(c(''),
       file=filename)
}