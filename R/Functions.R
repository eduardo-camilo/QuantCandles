prices <- function(ativo) {
  library(quantmod)
  getSymbols(ativo)
  pr<-get(ativo)[,1:4]
  names(pr)<-c("open","high","low","close")
  return(pr)
}
