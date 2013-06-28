#!/usr/bin/env Rscript
# Author:   Jason A. French & David M. Condon
# Email:    frenchja@u.northwestern.edu
# SAPA Tools:     http://sapa-project.org/r/

check.location <- function(ssh.user){
  # Check hostname and location
  hostname <- system('hostname', intern=TRUE)
  switch(hostname,
         revelle.ci.northwetern.edu={
           return(TRUE)
         },
         hardin={
           user <- Sys.info()['user']
           cmd <- paste('ssh -fNg -L 3306:127.0.0.1:3306 ',
                        user,'@revelle.ci.northwestern.edu',
                        sep='')
           system(cmd) # wait=TRUE?
           return(TRUE)
         },{
         warning('This script is meant be run from the SAPA Project server!')
         switch(Sys.info()['sysname'],
                Windows={
                  stop('Sorry!  Windows doesn\'t do SSH tunneling!')},
{
  choice <- menu(choices=c('Yes','No'),
                 title='Do you want to try tunneling over SSH?')
  if(choice == 1 & nchar(Sys.which('ssh')) > 0) {
    message(paste('SSH located at ',Sys.which('ssh'),'. Connecting.', sep=''))
    if(hasArg(ssh.user)){
      user <- ssh.user
    }
    else{
      user <- readline(prompt='Enter your NetID: ')
    }
    cmd <- paste('ssh -fNg -L 3306:127.0.0.1:3306 ',
                 user,'@revelle.ci.northwestern.edu',
                 sep='')
    system(cmd)
    return(TRUE)
  }
  else {
    stop('Cannot proceed! Either run this script from the server or tunnel using SSH!')
  }
})})
  # Kill SSH Tunnel on.exit()
  on.exit(system('pkill ssh'))
}

sapa.db <- function(database,user,password,ssh.user,all=FALSE) {
  # Establishes connect to SAPA MySQL database
  #
  # Args:
  #   database: Database to connect
  #   user:     MySQL SAPA username
  #   password: MySQL SAPA password
  #   ssh.user: SSH NetID
  #   all:      Connect to all databases?
  #
  # Returns: RMySQL connection
  
  check.location(ssh.user)
  
  # Check packages
  if (!require(RMySQL)) {
    stop('RMySQL not found. Please install!')
  }
  
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
  
  if (!hasArg(user) | !hasArg(password)) {
    user <- readline(prompt="Enter the SAPA MySQL username: ")
    password <- readline(prompt="Enter the SAPA MySQL password: ")
  }
  print(paste('Connecting to',database))
  
  # Connect to database
  con <- switch(database,
         'SAPAactive'={dbConnect(MySQL(),
                                    user=user,
                                    password=password,
                                    dbname="SAPAactive",
                                    host="127.0.0.1",
                                    port=3306)
         },
         'SAPAarchive'={dbConnect(MySQL(),
                                     user=user,
                                     password=password,
                                     dbname="SAPAarchive",
                                     host="127.0.0.1",
                                     port=3306)
         })
  return(con)
}

get.sapa <- function(table.name,con,filename,write=FALSE,user,password) {
  # Imports a SAPA table from MySQL into R
  #
  # Args:
  #   table.name: MySQL table name
  #   write:      Write table to disk
  #   con:        RMySQL connection
  #   filename:   Filename to write
  #   user:       MySQL username for sapa.db()
  #   password:   MySQL password for sapa.db()
  #
  # Returns:  R data.frame
  
  # Check for active RMySQL connection
  if (!hasArg(con)) {
    message('RMySQL connection not specified. Calling sapa.db() for you.')
    # Could use dbListConnections(MySQL())
    con <- sapa.db(user=user,password=password)
  }
  
  # Check if table argument passed
  if (!hasArg(table.name)) {
    # Choose Tables to export to R
    table.choice <- select.list(choices=dbListTables(con),
                                title='Choose which table to convert to a data.frame: ',
                                multiple=FALSE)
  } else{
    # Check that sapa.table argument is valid
    if (dbExistsTable(conn=con,name=table.name)) {
      table.choice <- table.name
    } else {
      warning('Invalid table choice!',immediate.=TRUE)
      # Choose Tables to export to R
      table.choice <- select.list(choices=dbListTables(con),
                                  title='Choose which table to convert to a data.frame: ')
    }}
  
  data <- dbReadTable(conn=con,name=table.choice)
  
  if (write == TRUE & !hasArg(filename)) {
    filename <- readline(prompt='Choose a name for your table in R: ')
  }
  
  if (write == TRUE & hasArg(filename)) {
    save(x=data,
         file=paste(filename,'.',as.character(Sys.Date()),'.Rdata',sep=''),
         compression_level=9,
         compress='bzip2')
  }

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
  if ('RIDpage' %in% colnames(x) ){
    x <- x[!duplicated(x[,'RIDpage']),]
  }
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

merge.sapa <- function(date='2013-05-20',filename) {
  # Get SAPA Project data
  #
  # Args:
  #   date:     Minimum date cutoff (YYYY-MM-DD)
  #   filename: Filename used to write data.frame
  #
  # Returns:  data.frame

  # Check for date argument
  if(hasArg(date)){
    print(paste('Obtaining SAPA date from ',date))
    date <- as.Date(date)
  }
  # Protect against NULL 
  else {
    warning('No date supplied.  Using 05/20/2013!')
    date <- as.Date('2013-5-20')
  }
  
  # List of tables on SAPAactive
  sapa.active.list <- c('TAIE_responses_052013','CAI_responses_052013',
                         'emotion_responses_052013','health_responses_052013',
                         'peer_responses_052013')
  # Establish connection to SAPAactive
  con.active <- sapa.db(database='SAPAactive')

  # NEW
  sapa.active <- Reduce(function(...) merge(..., all=TRUE),
                             sapply(sapa.active.list,function(x){
     y <- sapa.table(table.name=as.character(x),con=con.active,write=FALSE)
     },simplify=FALSE))
  sapa.active$time <- as.Date(sapa.active$time,format="%Y-%m-%d %H:%M:%S")
  
  # Close db connection
  dbDisconnect(con.active)
  rm(con.active)
  rm(sapa.active.list)

  # Use different MySQL database than SAPAactive if needed
  if(date < as.Date('2013-5-20')) {
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
    # Establish connection to SAPAactive
    con.archive <- sapa.db(database='SAPAarchive')
    
    # NEW
    sapa.archive <- Reduce(function(...) merge(..., all=TRUE),
                          sapply(sapa.archive.list,function(x){
                            y <- sapa.table(table.name=as.character(x),con=con.archive,write=FALSE)
                          },simplify=FALSE))
    
    sapa.archive <- sapply(X=sapa.archive.list,function(x){
      y <- sapa.table(table.name=as.character(x),con=con.archive,write=FALSE)
      },simplify=FALSE)
    data.frame.list <- c(sapa.active,sapa.archive)
    # Close db connection
    dbDisconnect(con.archive)
    rm(con.archive)
  }
  # p_num is participant number is some tables...
  merged.data.frame = Reduce(function(...) merge(..., all=TRUE),
                             sapa.active)

  merged.data.frame$time <- as.Date(merged.data.frame$time,format="%Y-%m-%d %H:%M:%S")
  merged.data.frame$age <- merged.data.frame$age.x
  merged.data.frame$gender <- merged.data.frame$gender.x
  merged.data.frame <- subset(merged.data.frame,
                              select = -c(age.x,age.y,gender.x,gender.y))  
  
  merged.data.frame <- clean.sapa(x=merged.data.frame,
                                  max.age=91,
                                  min.age=13)
  
  # Save .rdata if argument passed
  if(hasArg(filename)){
    print(paste('Writing SAPA to ',filename,'.',Sys.Date(),'.Rdata',sep=''))
    save(merged.data.frame,
         file=paste(filename,'.',as.character(Sys.Date()),'.Rdata',sep=''))
  } else {
    warning('No filename supplied.  Outputting as object!')
  }
  return(merged.data.frame)
}

make.sapa <- function(filename){
#   # Builds the SAPA.rdata, scoring the IQ items.
#   #
#   # Args:
#   #   file: Specify filename output
#   #
#   # Returns:
#   #   data.frame in the sapa.rdata file
#   
#   data <- get.sapa(date='2011-05-20')
#   
#   # Remove Bad IQ Participants
#   # Do some last.iq <- grep() to find last iq_ variable in big.kahuna
#   bad.iq <- rowSums(data[,c((which(colnames(data)=="ACT")+1):last.iq)],
#                     na.rm=TRUE)
#   data <- data[bad.iq>0,]
#   
#   # Scrub responses to change all zero values to ‘NA’.
# #   iq.a <- scrub(iq.a, where = c("relstatus", "ethnic"), isvalue = 0)
# #   iq.a <- scrub(iq.a, where = c((which(colnames(iq.a)=="SATV")):ncol(iq.a)),
# #                 isvalue = 0)
# #   iq.a <- scrub(iq.a, where="p1occ", isvalue=c(98, 99), newvalue=c(0))
# #   iq.a <- scrub(iq.a, where="p2occ", isvalue=c(97, 98, 99), newvalue=c(0))
# #   iq.a <- scrub(iq.a, where="gender", isvalue=c(1), newvalue=c(0))
# #   iq.a <- scrub(iq.a, where="gender", isvalue=c(2), newvalue=c(1))
# #   iq.a <- scrub(iq.a, where="relstatus", isvalue=c(1), newvalue=c(0))
# #   iq.a <- scrub(iq.a, where="relstatus", isvalue=c(2), newvalue=c(1))
# #   iq.a <- scrub(iq.a, where="state", isvalue=c(0))
# #   iq.a <- scrub(iq.a, where="status", isvalue=c(99))
# #   iq.a <- scrub(iq.a, where="p2edu", isvalue=c(999))
# #   iq <- subset(iq.a, select=-c(participant_number, no_code))
#   
#   # Check file argument
#   if(!hasArg(filename)) {
      filename <- readline(prompt='Please choose a name for the file: ')
#     filepath <- paste(filename,'.',Sys.Date(),'.rdata',sep="")
#   }
#   
#   # Save the Big Kahuna with today's date
#   save(c(''),
#        file=filepath)
}