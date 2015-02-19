#!/usr/bin/env Rscript
# Authors:   Jason A. French & David M. Condon
# Email:    frenchja@u.northwestern.edu
# SAPA Tools:     https://sapa-project.org/r/

check.location <- function(ssh.user, ssh.pass, vpn.check = TRUE, ssh.tunnel = TRUE){
  # Establishes connect to SAPA MySQL database
  #
  # Args:
  #   ssh.user:   String of username for SSH tunnel.
  #   ssh.pass:   String of ssh password if user is too lazy 
  #               to setup ssh-copy-id.
  #   vpn.check:  Set FALSE to avoid VPN check.
  #   ssh.tunnel: Set TRUE to force ssh tunnel (for scripting)
  #
  # Returns:
  #   TRUE if SSH tunnel is established when needed
  
  # Windows check
  if(Sys.info()['sysname'] == 'Windows'){
    stop('Sorry!  Windows doesn\'t do SSH tunneling!')
  }
  
  # Check hostname and location
  hostname <- system('hostname', intern=TRUE)
  # Switching is faster than nested ifs
  switch(hostname,
         revelle.ci.northwestern.edu={
           return(TRUE)
         },
         hardin={
           user <- Sys.info()['user']
           cmd <- paste('ssh -fNg -L 3306:127.0.0.1:3306 ',
                        user, '@revelle.ci.northwestern.edu',
                        sep='')
           system(cmd) # wait=TRUE?
           ssh.id <- system('lsof -t -i :3306 -i@revelle.ci.northwestern.edu', intern = TRUE)
           return(TRUE)
         },
         # SSH Tunnel if hostname does not match
         {
           warning('This script is meant be run from the SAPA Project or Hardin server!')
           
           # Check for NU VPN connection
           if(vpn.check){
             if(!grepl(pattern = "*.vpn.northwestern.edu$",
                       x = system('hostname', intern=TRUE))){
               stop('VPN connection to Northwestern not detected.
Please see http://www.it.northwestern.edu/oncampus/vpn/
If you\'re certain you\'re connected, set the vpn.check = FALSE argument.')
             }
           }
           
           # Prompt for SSH Tunnel if ssh.tunnel = FALSE
           if(!isTRUE(ssh.tunnel)){
             if(menu(choices = c('Yes', 'No'), title='Do you want to try tunneling over SSH?') == 2){
               stop('You\'ve chosen to not initiate an SSH tunnel.  Please login to SAPA and load library(SAPATools) manually.')
             }
           }
           
           if(!hasArg(ssh.user)){
             choice <- menu(choices = c(paste('Use', user), "Let me change my SSH name."),
                            title = paste("You didn't specify an ssh.user argument.  Do you want to try with ", user, "?", sep = ""))
             if(choice == 2){
               user <- readline(prompt = "Please enter an ssh.user: ")
             }         
           }
           
           # Detect ssh binary
           if(nchar(Sys.which('ssh')) > 0) {
             # Give user tunneling command if ssh-copy-id not used
             # ssh.command <- paste("Type the following into a new Terminal tab:\n ssh -fNg -L 3306:127.0.0.1:3306 ", user, "@revelle.ci.northwestern.edu", sep="")
             # message(ssh.command)
             # readline(prompt='Press [Enter] when you\'ve established the SSH Tunnel.')
             ssh.command <- paste("ssh -fNg -L 3306:127.0.0.1:3306 ", user, "@revelle.ci.northwestern.edu", sep = "")
             if(hasArg(ssh.pass)){
               system(command = ssh.command, input = ssh.pass)
             } else {
               system(command = ssh.command)
             }
             
             # Track SSH process ID
             ssh.id <- system('lsof -t -i :3306 -i@revelle.ci.northwestern.edu', intern = TRUE)
             return(TRUE)
           } else {
             stop('Cannot proceed! I didn\'t detect SSH on your system!')
           }}
         )
}

kill.tunnel <- function(){
  # Function to terminal SSH tunnel at the end of your scripts.
  #
  # TODO:  Fix lsof exit status
  ssh.id <- as.integer(system('lsof -t -i :3306 -i@revelle.ci.northwestern.edu', intern = TRUE))
  tools::pskill(ssh.id)
}

sapa.db <- function(database, user, password, ssh.user, all=FALSE) {
  # Establishes connect to SAPA MySQL database
  #
  # Args:
  #   database: Database to connect
  #   user:     MySQL SAPA username
  #   password: MySQL SAPA password
  #   ssh.user: SSH NetID, passed to check.location()
  #
  # Returns: RMySQL connection
  tunnel <- system(command="pgrep -f 'ssh -fNg -L 3306'",intern=TRUE)
  if (is.null(tunnel) || length(tunnel) == 0) {
    check.location(ssh.user)
  }
  
  # Check packages
  if (!require(RMySQL)) {
    stop('RMySQL not found. Please install!')
  }
  
  # Check if database argument passed
  if (!hasArg(database)) {
    database <- select.list(choices=c('SAPAactive','SAPAarchive'),
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

get.sapa <- function(table.name, con, filename, write = FALSE, user, password) {
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
    con <- sapa.db(user = user, password = password)
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

clean.sapa <- function(x, max.age=91, min.age=13, age=TRUE, unique=TRUE) {
  # Returns data.frame of unique participants
  #
  # Args:
  #   x:            SAPA data.frame from sapa.table()
  #   max.age:      Max age to allow
  #   min.age:      Min age to allow
  #   age:          Filter based on age
  #   unique:       Return only unique RIDs
  
  # Take last RIDpage entry if duplicates
  if ('RIDpage' %in% colnames(x) ){
    x <- x[!duplicated(x[,'RIDpage']),]
  }
  # Take first RID entry (i.e., latest page) if duplicates
  if (hasArg(unique)){
    x <- x[!duplicated(x[,'RID'],fromLast=TRUE),]
  }
  
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
  if (hasArg(age)){
    x <- x[which(x$age > min.age & x$age<max.age & x$no_code<1),]
  }

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

nonNAs <- function(x) {
  # Returns items answered by each participant
  #
  # Args:
  #   x:  SAPA data.frame
  #
  as.vector(apply(x, 1, function(x) length(which(!is.na(x)))))
}

make.sapa <- function(filename){
   # Builds the SAPA.rdata, scoring the IQ items.
   #
   # Args:
   #   file: Specify filename output
   #
   # Returns:
   #   data.frame in the sapa.rdata file
   
   # Connect to SAPAarchive via SQL
   con.archive <- sapa.db(database='SAPAarchive',user='RobertHooke',password='polymath')

   b5_responses_012513 <- get.sapa(con=con.archive,table.name='b5_responses_012513')
   b5_responses_012513 <- clean.sapa(b5_responses_012513)

   temp1OrderedUnscrubbed <- subset(b5_responses_012513, 
     select=c(RID,gender:relstatus, height, weight, country, state, ethnic, zip, education, discipline,
      major, status, field, occupation, p1occ, p1edu, p2occ, p2edu, q_6, q_22, q_23, q_39, q_40, q_44, 
      q_52, q_53, q_55, q_66, q_76, q_79, q_80, q_94, q_98, q_101, q_105, q_107, q_108, q_124, q_126, 
      q_128, q_131, q_132, q_134, q_139, q_140, q_141, q_142, q_145, q_146, q_150, q_151, q_152, q_154, 
      q_156, q_157, q_158, q_159, q_169, q_170, q_176, q_177, q_179, q_184, q_185, q_194, q_195, q_200, 
      q_217, q_225, q_238, q_239, q_240, q_241, q_248, q_251, q_253, q_254, q_260, q_262, q_278, q_279, 
      q_292, q_316, q_319, q_321, q_322, q_331, q_332, q_334, q_348, q_364, q_401, q_403, q_413, q_422, 
      q_455, q_463, q_477, q_460, q_491, q_492, q_493, q_497, q_500, q_501, q_516, q_519, q_520, q_525, 
      q_530, q_538, q_539, q_549, q_554, q_559, q_571, q_582, q_594, q_602, q_608, q_609, q_610, q_611, 
      q_612, q_619, q_626, q_634, q_636, q_637, q_651, q_665, q_670, q_673, q_690, q_698, q_704, q_711, 
      q_712, q_716, q_721, q_727, q_668, q_706, q_744, q_747, q_757, q_761, q_776, q_804, q_811, q_815, 
      q_819, q_820, q_832, q_838, q_844, q_861, q_871, q_890, q_898, q_901, q_904, q_915, q_917, q_923, 
      q_926, q_930, q_931, q_946, q_952, q_958, q_960, q_962, q_964, q_965, q_966, q_974, q_979, q_985, 
      q_986, q_988, q_994, q_995, q_997, q_1020, q_1024, q_1027, q_1031, q_1033, q_1041, q_1043, q_1050, 
      q_1051, q_1053, q_1054, q_1055, q_1058, q_1063, q_1064, q_1083, q_1088, q_1090, q_1099, q_1104, 
      q_1110, q_1114, q_1132, q_1146, q_1150, q_1151, q_1159, q_1162, q_1163, q_1173, q_1180, q_1185, 
      q_1196, q_1201, q_1205, q_1206, q_1232, q_1242, q_1243, q_1253, q_1254, q_1255, q_1260, q_1276, 
      q_1290, q_1296, q_1300, q_1301, q_1306, q_1307, q_1321, q_1327, q_1333, q_1357, q_1359, q_1364, 
      q_1367, q_1372, q_1373, q_1374, q_1379, q_1385, q_1388, q_1389, q_1392, q_1393, q_1395, q_1397, 
      q_1401, q_1410, q_1419, q_1422, q_1424, q_1429, q_1441, q_1446, q_1447, q_1448, q_1452, q_1479, 
      q_1480, q_1483, q_1490, q_1495, q_1505, q_1507, q_1511, q_1517, q_1521, q_1536, q_1537, q_1543, 
      q_1550, q_1555, q_1563, q_1571, q_1573, q_1575, q_1577, q_1578, q_1583, q_1585, q_1588, q_1556, 
      q_1591, q_1596, q_1601, q_1609, q_1610, q_1616, q_1624, q_1633, q_1635, q_1643, q_1648, q_1652, 
      q_1653, q_1657, q_1663, q_1667, q_1668, q_1671, q_1675, q_1676, q_1677, q_1679, q_1681, q_1682, 
      q_1683, q_1685, q_1687, q_1696, q_1703, q_1705, q_1706, q_1709, q_1725, q_1730, q_1738, q_1742, 
      q_1747, q_1752, q_1754, q_1759, q_1761, q_1762, q_1763, q_1765, q_1766, q_1768, q_1774, q_1775, 
      q_1780, q_1787, q_1792, q_1795, q_1803, q_1808, q_1810, q_1832, q_1834, q_1848, q_1853, q_1861, 
      q_1867, q_1869, q_1871, q_1893, q_1894, q_1896, q_1899, q_1904, q_1913, q_1915, q_1916, q_1917, 
      q_1924, q_1943, q_1949, q_1957, q_1964, q_1969, q_1979, q_1989, q_1992, q_1993, q_2001, q_2003, 
      q_2005, q_2008, q_2011, q_2016, q_2018, q_2021, q_2024, q_2025, q_2029, q_2160, q_2161, q_2737, 
      q_2756, q_2765, q_2775, q_2786, q_2853, q_2891, q_2900))
  
  # Re-order demographic columns and rename
  colnames(temp1OrderedUnscrubbed)[c(15:16)] <- c("jobstatus", "jobfield")

  iq_responses_012213 <- get.sapa(con = con.archive, table.name = 'iq_responses_012213')
  iq1OrderedUnscrubbedWSkipAlls <- subset(iq_responses_012213, 
    select=c(RID, iq_10001, iq_10003, iq_10004, iq_10005, iq_10006, iq_10007, iq_10009, iq_10011, 
      iq_10013, iq_10014, iq_10016, iq_10017, iq_10018, iq_10019, iq_10023, iq_10026, iq_10031, iq_10032, 
      iq_10033, iq_10034, iq_10035, iq_10036, iq_10039, iq_10042, iq_10043, iq_10044, iq_10045, iq_10046, 
      iq_10047, iq_10048, iq_10050, iq_10053, iq_10054, iq_10055, iq_10056, iq_10058, iq_11001, iq_11002, 
      iq_11003, iq_11004, iq_11005, iq_11006, iq_11007, iq_11008, iq_11009, iq_11010, iq_11011, iq_11012, 
      iq_11013, iq_11014, iq_11015, iq_11016, iq_11017, iq_11018, iq_11019, iq_11020, iq_11021, iq_11022, 
      iq_11023, iq_11024))

  colnames(iq1OrderedUnscrubbedWSkipAlls)[c(2:61)] <- c("q_12001", "q_12003", "q_12004", "q_12005", "q_12006", 
    "q_12007", "q_12009", "q_12011", "q_12013", "q_12014", "q_12016", "q_12017", "q_12018", "q_12019", 
    "q_12023", "q_12026", "q_12031", "q_12032", "q_12033", "q_12034", "q_12035", "q_12036", "q_12039", 
    "q_12042", "q_12043", "q_12044", "q_12045", "q_12046", "q_12047", "q_12048", "q_12050", "q_12053", 
    "q_12054", "q_12055", "q_12056", "q_12058", "q_11001", "q_11002", "q_11003", "q_11004", "q_11005", 
    "q_11006", "q_11007", "q_11008", "q_11009", "q_11010", "q_11011", "q_11012", "q_11013", "q_11014", 
    "q_11015", "q_11016", "q_11017", "q_11018", "q_11019", "q_11020", "q_11021", "q_11022", "q_11023", 
    "q_11024")

  exp_responses_012213 <- get.sapa(table.name = 'exp_responses_012213',con = con.archive)

ORVIS1OrderedUnscrubbed <- subset(exp_responses_012213, select=c(RID, q_3102, q_3103, q_3104, q_3105, 
  q_3106,  q_3107, q_3108, q_3109, q_3110, q_3111, q_3112, q_3113, q_3114, q_3115, q_3116, q_3117, q_3118,  
  q_3119, q_3120, q_3121, q_3122, q_3123, q_3124,  q_3125, q_3126, q_3127, q_3128, q_3129, q_3130, q_3131, 
  q_3132, q_3133, q_3134, q_3135, q_3136,  q_3137, q_3138, q_3139, q_3140, q_3141, q_3142,  q_3143, q_3144, 
  q_3145, q_3146, q_3147, q_3148, q_3149, q_3150, q_3151, q_3152, q_3153, q_3154,  q_3155, q_3156, q_3157, 
  q_3158, q_3159, q_3160,  q_3161, q_3162, q_3163, q_3164, q_3165, q_3166, q_3167, q_3168, q_3169, q_3170, 
  q_3171, q_3172,  q_3173, q_3174, q_3175, q_3176, q_3177, q_3178,  q_3179, q_3180, q_3181, q_3182, q_3183, 
  q_3184, q_3185, q_3186, q_3187, q_3188, q_3189, q_3190,  q_3191, q_3192, q_3193))

   # Check file argument
   if(!hasArg(filename)) {
     filename <- readline(prompt='Please choose a name for the file: ')
     filepath <- paste(filename,'.',Sys.Date(),'.rdata',sep="")
   }
   
   # Save the Big Kahuna with today's date
   save(c(''),
        file=filepath)
}
