# Wiktionary R API
# Authors:  David M. Condon and Jason A. French <jason@sapa-project.org>

wiki.login <- function(user,password){
  require(RCurl)
  https://en.wiktionary.org/w/api.php?action=login&lgname=user&lgpassword=password
}

wiki.logout <- function(){
  https://en.wiktionary.org/w/api.php?action=logout
}

wiki.search <- function(x){
  require(RCurl)
  require(RJSONIO)
  data <- getForm(
    "https://en.wiktionary.org/w/api.php", 
    action  = "opensearch", 
    search  = x
  )
  return(fromJSON(data))
}

wiki.compound <- function(x){
  # Function to find compound words from a given stem
  # Arg:
  #   x: stem word (e.g., 'book')
  # Returns: list of compound words
  
  require(RCurl)
  require(RJSONIO)
  words <- getForm(
    "https://en.wiktionary.org/w/api.php", 
    action  = "opensearch", 
    search  = x
  )
  words <- fromJSON(words)[[2]]
  compounds <- words[which(nchar(words)>(nchar(x)+3))]
  return(unlist(compounds))
}

wiki.synonym <- function(x){
  require(RCurl)
  require(RJSONIO)
  definitions <- getForm(
    "https://en.wiktionary.org/w/api.php",
    action = 'query',
    title = x,
    rvprop = 'content'
    )
  definitions <- fromJSON(definitions)
}

wiki.antonym <- function(x){
  
}
  