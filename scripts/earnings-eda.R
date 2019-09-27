load('data/comp_funda.RData')
load('data/comp_fundq.RData')

keep.vars <- c("gvkey", "datadate", "fyear", "fyr", "tic", "prcc_f", "seq", 
               "ceq", "txditc", "txdb", "itcb", "pstkrv", "pstkl", "pstk", "prcc_f", "csho", 
               "epsfx", "epsfi", "oprepsx", "opeps", "ajex", "ebit", "spi", "nopi", "sale", 
               "ibadj", "dvc", "dvp", "ib", "oibdp", "dp", "oiadp", "gp", "revt", "cogs", 
               "pi", "ibc", "dpc", "at", "ni", "ibcom", "icapt", "mib", "ebitda", "xsga",
               "xido", "xint", "mii", "ppent", "act", "lct", "dltt", "dlc", "che", "invt", 
               "lt", "rect", "xopr", "oancf", "txp", "txt", "ap", "xrd", "xad", "xlr", "capx")


## get year counts for ech firm
o <- which(comp.fundq$fyearq >= 2001)
x <- comp.fundq[o, c("gvkey", "fyearq", "fyr", "datacqtr", "datafqtr", "tic", "conm", "atq", "niq","cshoq","ceqq","prccq")]

gvkey.quarters <- aggregate(x$gvkey, by = list(x$gvkey), FUN = length)
names(gvkey.quarters) <- c("gvkey", "quarters")
o <- which(gvkey.quarters$quarters >= 20)
gvkey.quarters <- gvkey.quarters[o,]
x1 <-merge(x, gvkey.quarters)
x1 <- x1[order(x1$gvkey, x1$fyearq), ]

## Plot earnings over time
v <- x1[1,"gvkey"]
tic <- x1[1,"tic"]
earnings <- x1[which(x1$gvkey == v), c("fyear", "ni")]
plot(earnings, 
     type="l")

par(mfrow=c(4,4))
for(i in 1:16){
  v <- gvkey.quarters[i,"gvkey"]
  tic <- unique(x1[which(x1$gvkey == v),"tic"])
  earnings <- x1[which(x1$gvkey == v), c("fyearq", "niq")]
  barplot(earnings$niq, 
       main = tic)
}

load("data/ibescrspcomp.Rda")
names(X)
x <- X[X$tic == "MSFT", ]
