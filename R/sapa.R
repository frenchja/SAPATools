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
    choice <- menu(choices=c('Yes','No'),
                   title='Do you want to try tunneling over ssh?')
    if(choice == 1) {
      user <- readline(prompt='Enter your NetID: ')
      cmd <- paste('ssh -fNg -L 3306:127.0.0.1:3306 ',
                   user,'@revelle.ci.northwestern.edu',
                   sep='')
      system(cmd)
    }
  } else {
    stop('Cannot proceed!\n
       Either run this script from the server or input your NetID!')
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
  check.location()
  
  # Check if database argument passed
  if (!hasArg(database)) {
    database <- menu(choices=c('SAPAactive','SAPAcurrent','SAPAarchive'),
                     title='Choose which database to build: ')
  }
  print(paste('Connecting to ',database))
  # Connect to database
  switch(database,
         1={con.active <- dbConnect(MySQL(),
                                    user="username",
                                    password="password",
                                    dbname="SAPAactive",
                                    host="127.0.0.1",
                                    port=3306)
            con <- con.active
            return(con)},
         2={con.archive <- dbConnect(MySQL(),
                                     user="username",
                                     password="password",
                                     dbname="SAPAarchive",
                                     host="127.0.0.1",
                                     port=3306)
            con <- con.archive
            return(con)},
         3={con.current <- dbConnect(MySQL(),
                                     user="username",
                                     password="password",
                                     dbname="SAPAcurrent",
                                     host="127.0.0.1",
                                     port=3306)
            con <- con.current
            return(con)})
  # Add stop('Choose a database!')
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
    # Check that table argument is valid
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
         2={warning('Data not saved to disk...')
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

make.sapa <- function(file){
  # Builds the SAPA.rdata, scoring the IQ items.
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
  
  if(!hasArg(file)) {
    filename <- paste('sapa.',Sys.Date(),'.rdata',sep="")
  } else {
    filename <- file
  }
  
  # Save the Big Kahuna with today's date
  save(c(''),
       file=filename)
}