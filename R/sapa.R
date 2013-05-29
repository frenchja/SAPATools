#!/usr/bin/env Rscript
# Author:  Jason A. French
# Website:  http://sapa-project.org

# Check packages
if (!require(RMySQL)) {
    warning('RMySQL not found. Attempting to install!')
    install.packages('RMySQL',dependencies=TRUE)
    require(RMySQL,character.only=TRUE)
}

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
database <- menu(choices=c('SAPAactive','SAPAcurrent','SAPAarchive'),
                 title='Choose which database to build: ')
switch(database,
       1={con.active <- dbConnect(MySQL(),
                                  user="username",
                                  password="password",
                                  dbname="SAPAactive",
                                  host="127.0.0.1",
                                  port=3306)
          con <- con.active
          },
       2={con.archive <- dbConnect(MySQL(),
                                   user="username",
                                   password="password",
                                   dbname="SAPAarchive",
                                   host="127.0.0.1",
                                   port=3306)
          con <- con.archive},
       3={con.current <- dbConnect(MySQL(),
                                   user="username",
                                   password="password",
                                   dbname="SAPAcurrent",
                                   host="127.0.0.1",
                                   port=3306)
          con <- con.current},
       stop('Choose a database!'))

# Choose Tables to export to R
table.choice <- menu(choices=dbListTables(con),
                     title='Choose which table to convert to a data.frame: ')
table.name <- readline(prompt='Choose a name for your table: ')
table.name[1] <- dbReadTable(con,name=dbListTables(con)[table.choice])

table.write <- menu(choices=c('Yes','No'),
                    title='Would you like to save your table to disk?')
switch(table.write,
       1={write.table(x=table.name[1],
                      file=paste(table.name[1],'.data'))
          },
       2={warning('Data not saved to disk...')
          })

on.exit(dbDisconnect(con))