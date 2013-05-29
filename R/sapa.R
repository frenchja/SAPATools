#!/usr/bin/env Rscript
# Author:   Jason A. French
# Email:    frenchja@u.northwestern.edu
# SAPA:     http://sapa-project.org

# Check packages
if (!require(RMySQL)) {
    warning('RMySQL not found. Attempting to install!')
    install.packages('RMySQL',dependencies=TRUE)
    require(RMySQL,character.only=TRUE)
}

check.location <- function() {
  # Check hostname and location
  if(Sys.getenv("HOSTNAME") != 'revelle.ci.northwestern.edu'){
    warning('This script is meant be run from the SAPA Project server!\n')
    
    switch(Sys.info()['sysname'],
           Windows={
             stop('Sorry!  Windows doesn\'t do SSH tunneling!')},
           {choice <- menu(choices=c('Yes','No'),
                           title='Do you want to try tunneling over ssh?')
            if(choice == 1) {
              user <- readline(prompt='Enter your NetID: ')
              cmd <- paste('ssh -fNg -L 3306:127.0.0.1:3306 ',
                           user,'@revelle.ci.northwestern.edu',
                           sep='')
              system(cmd)
              return(TRUE)
              } else {
                stop('Cannot proceed!\n Either run this script from the server or input your NetID!')
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
  #   database: Choose the database to connect
  #   all:      Connect to all databases?
  #
  # Returns: RMySQL connection
  
  check.location()
  
  # Check if database argument passed
  if (!hasArg(database)) {
    database <- menu(choices=c('SAPAactive','SAPAcurrent','SAPAarchive'),
                     title='Choose which database to build: ')
  }
  
  print(paste('Connecting to ',database))
  
  # Connect to database
  con <- switch(database,
         {dbConnect(MySQL(),
                                    user="username",
                                    password="password",
                                    dbname="SAPAactive",
                                    host="127.0.0.1",
                                    port=3306)
         },
         {dbConnect(MySQL(),
                                     user="username",
                                     password="password",
                                     dbname="SAPAarchive",
                                     host="127.0.0.1",
                                     port=3306)
         },
         {dbConnect(MySQL(),
                                     user="username",
                                     password="password",
                                     dbname="SAPAcurrent",
                                     host="127.0.0.1",
                                     port=3306)
         })
  return(con)
}

sapa.table <- function(sapa.table,con,all=FALSE) {
  # Imports a SAPA table from MySQL into R
  #
  # Args:
  #   table:  MySQL table name
  #   all:    Import all tables into R
  #   con:    RMySQL connection
  #
  # Returns:  R data.frame
  
  # Check if table argument passed
  if (length(sapa.table) < 1) {
    # Choose Tables to export to R
    table.choice <- menu(choices=dbListTables(con),
                         title='Choose which table to convert to a data.frame: ')
  } else{
    # Check that sapa.table argument is valid
    tryCatch({
      is.element(el=table,set=dbListTables(con))
    },
             error =  function(e) {
               warning('Invalid table choice!')
               # Choose Tables to export to R
               table.choice <- menu(choices=dbListTables(con),
                                    title='Choose which table to convert to a data.frame: ')
             }
    )
    table.choice <- sapa.table
  }
  
  # Check for active RMySQL connection
  if (!hasArg(con)) {
    con <- sapa.db()
  }
  
  table.name <- readline(prompt='Choose a name for your table in R: ')
  paste(table.name[1]) <- dbReadTable(con,name=dbListTables(con)[table.choice])
  
  table.write <- menu(choices=c('Yes','No'),
                      title='Would you like to save your table to disk?')
  switch(table.write,
         1={write.table(x=table.name[1],
                        file=paste(table.name[1],Sys.Date,'.data',sep=''))
         },
         2={warning('Data not saved to disk yet...')
         })
  return(paste(table.name[1]))
  on.exit(dbDisconnect(con))
}

clean.sapa <- function(x, max.age, min.age) {
  # Returns data.frame of unique participants
  #
  # Args:
  #   x:  SAPA data.frame from sapa.table()
  #   max.age:  Max age to allow
  #   min.age:  Min age to allow
  
  dupli <- duplicated(x[,"RID"])+0
  x <- x[dupli==0,]
  if (missing(min.age))
    min.age <- 13
  if (missing(max.age))
    max.age <- 91
  x <- subset(x, x[,"age"]>min.age)
  x <- subset(x, x[,"age"]<max.age)
  x <- subset(x, x[,"no_code"]<1)
  return(x)
}

get.sapa <- function(date,filename) {
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
  con.active <- sapa.db(database=con.active)
  # Vectorize list of tables
  lapply(X=sapa.active,
         FUN=function(x){           
           x <- sapa.table(sapa.table=x,con=con.active)
         })
  # Close db connetion
  dbDisconnect(con.active)
  
  # Use different MySQL Table than SAPAactive if needed
  if(date < as.Date('2013-5-20',format="%Y-%m-%d")) {
    sapa.archive.list <- c('')
    lapply(X=sapa.archive.list,
           FUN=function(x){
             
           })
  }
  
  data$time <- as.Date(data$time,format="%Y-%m-%d %H:%M:%S")
  
  if(hasArg(filename)){
    print(paste('Writing SAPA to ',filename))
    save(c(''),
         file=filename)
  } else {
    warning('No filename supplied.  Outputting as object!')
    
  }
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