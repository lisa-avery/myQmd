# Useful functions for the project
# If you fork the github repository to your own, you can keep this up to date with your most useful functions

# tidy rounding keeping trailing zeros for output tables
rnd <- function(x,digits=2){
  out <- sapply(x, function(x){
    format(round(x,digits),nsmall=digits)
  })
  out <- unlist(out,use.names = FALSE)
  return(out)
}