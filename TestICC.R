library("psych")
library("ICC")
library("multilevel")
library("nlme")

library("Hmisc")

sf <- matrix(c(9,    2,   5,    8,
               6,    1,   3,    2,
               8,    4,   6,    8,
               7,    1,   2,    6,
               10,   5,   6,    9,
               6,   2,   4,    7),ncol=4,byrow=TRUE)
colnames(sf) <- paste("J",1:4,sep="")
rownames(sf) <- paste("S",1:6,sep="")
sf  #example from Shrout and Fleiss (1979)
results = ICC(sf)
resultsdf = results$results

ICCest(J1, J2, data = sf, CI.type = "S")

data(ChickWeight)
ICCest(Chick, weight, data = ChickWeight, CI.type = "S")

data(bh1996)
mult.icc(bh1996[,c("HRS","LEAD","COHES")],grpid=bh1996$GRP)

ICC(bh1996[, c("HRS", "GRP")])
mult.icc(sf[, 1:2], grpid = sf[, 1])

as.data.frame(rcorr(as.matrix(dfComplete),type = "pearson")$P)
as.data.frame(rcorr(as.matrix(data_frame),type = "pearson")$r)


