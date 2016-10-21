# Calculates the eta square coefficient for the "correlation" between qualitative and quantitative variable
# https://en.wikipedia.org/wiki/Effect_size#Eta-squared.2C_.CE.B72
eta <- function(x,y){
  m <- mean(x,na.rm = TRUE)
  sct <- sum((x-m)^2,na.rm = TRUE)
  n <- table(y)
  mk <- tapply(x,y,mean)
  sce <- sum(n*(mk-m)^2)
  return(ifelse(sct>0,sce/sct,0)) # SS_Treatment / SS_Total
}

a <-  factor(c("M","NA","F","F","NA","M","F"))
b <-  factor(c("y","y","y","n","n","n","y"))
d <- c(2,5,4,8,9,6,4)
e <- c(5,7,8,5,6,9,7) 

qual = list(a=a, b=b)
quant = list(e=e,d=d)

m = matrix(mapply(eta, rep(quant, length(qual)), rep(qual, each=length(quant))), ncol=length(qual))

rownames(m) = names(quant)
colnames(m) = names(qual)
