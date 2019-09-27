## Reginald Edwards
## 11 June 2018
## 17 July 2018
## Import quarterly Compustat fundamentals data
## Clean and filter the data
## Generate features for modeling
## Compute quarterly financial ratios.
###############################################################################

rm(list=ls())
gc()
library(RPostgres)
source("../0_code/useful-fn.R")
source('../0_datasets/wrds_login.R')
#wrds <- dbConnect(Postgres(),  host='wrds-pgdata.wharton.upenn.edu', port=9737, user=wrds_user, password=wrds_pw, sslmode='require', dbname='wrds')

## Raw Compustat data are in a PostgresQL database "import.comp_fundq"
localdb <- dbConnect(Postgres(), 
  host='localhost',
  port=5432,
  user='postgres',
  password = 'havoc')


###############################################################################
## Retrieve compustat fundamentals from SQL database
###############################################################################
res <- RPostgres::dbSendQuery(conn = localdb, statement = "SELECT * FROM fundq_short;")
#comp.fundq <- RPostgres::dbFetch(res, n = 100)
comp.fundq <- RPostgres::dbFetch(res)
RPostgres::dbClearResult(res)

## number of observations of "baseline" data
n <- nrow(comp.fundq)


###############################################################################
## Convert character variables to numeric variables, date variables etc. as
## appropriate.
###############################################################################

vars <- c("apq",
"ajexq",
"atq",
"capxy",
"cheq",
"cshoq",
"ceqq",
"cogsq",
"actq",
"lctq",
"dlcq",
"txdbq",
"txditcq",
"dpcy",
"dpq",
"dvpq",
"dvpy",
"oepsxy",
"epspxq",
"epsfxq",
"epsfiq",
"opepsq",
"xidoq",
"ibq",
"ibadjq",
"ibcomq",
"ibcy",
"txtq",
"txpq",
"xintq",
"invtq",
"icaptq",
"ltq",
"dlttq",
"niq",
"miiq",
"mibq",
"nopiq",
"oancfy",
"xoprq",
"oiadpq",
"oibdpq",
"pstkrq",
"pstkq",
"piq",
"prccq",
"ppentq",
"rectq",
"xrdq",
"revtq",
"saleq",
"xsgaq",
"spiq",
"seqq")

X <- comp.fundq
for(v in vars) X[,v] <- as.numeric(X[,v])
comp.fundq <- X
rm("X")



###############################################################################
## Create a global "quarter" time variable as quarters since the first
## quarter of 1960
###############################################################################
get.quarter <- function(x){
  ## x is a datacqtr from Compustat
  year <- as.numeric(substr(x, 1, 4))
  qtr <- as.numeric(substr(x, 6,6))
  return((year - 1960)*4 + qtr)
}
comp.fundq$qtr <- get.quarter(comp.fundq$datacqtr)


###############################################################################
## Add industry codes
###############################################################################
## TODO: move this code to SQL
load("../0_datasets/comp_funda.RData")
industries <- comp.funda[,c("gvkey", "fyear", "sich")]
rm(comp.funda)
comp.fundq <- merge(comp.fundq, industries, by.x = c("gvkey", "fyearq"), by.y = c("gvkey", "fyear"), all.x = TRUE)

###############################################################################
## Apply some data filters
###############################################################################
## Create a table for keeping track of data filters and observations at each
## step.
comp.fundq.filters <- data.frame(matrix(NA, nrow = 0, ncol = 2))
names(comp.fundq.filters) <- c("data.filter", "obs.remaining")
comp.fundq.filters[1, ] <- c("baseline", nrow(comp.fundq))

## Data filters, from most to least essential
## 1. Positive total assets that are not missing (atq > 0)
comp.fundq <- comp.fundq[-which(is.na(comp.fundq$atq)), ]
comp.fundq <- comp.fundq[comp.fundq$atq > 0 , ]
comp.fundq.filters[nrow(comp.fundq.filters) + 1, ] <- c("atq_gt_0", nrow(comp.fundq))

## 2. Positive revenue/sales that are not missing (saleq > 0)
comp.fundq <- comp.fundq[-which(is.na(comp.fundq$saleq)), ]
comp.fundq <- comp.fundq[comp.fundq$saleq > 0, ]
comp.fundq.filters[nrow(comp.fundq.filters) + 1, ] <- c("saleq_gt_0", nrow(comp.fundq))

## 3. Non-missing stock price at end of quarter greater than $2 per share (prccq > 2)
comp.fundq <- comp.fundq[-which(is.na(comp.fundq$prccq)), ]
comp.fundq <- comp.fundq[comp.fundq$prccq > 2, ]
comp.fundq.filters[nrow(comp.fundq.filters) + 1, ] <- c("prccq_gt_2", nrow(comp.fundq))

## 4. EPS is not missing (epspxq not NA)
comp.fundq <- comp.fundq[-which(is.na(comp.fundq$epspxq)), ]
comp.fundq.filters[nrow(comp.fundq.filters) + 1, ] <- c("epspxq_not_na", nrow(comp.fundq))

## 5. Date is not missing
comp.fundq <- comp.fundq[!(comp.fundq$datacqtr == ""), ]
comp.fundq <- comp.fundq[!(comp.fundq$datadate == ""), ]
comp.fundq.filters[nrow(comp.fundq.filters) + 1, ] <- c("date_not_na", nrow(comp.fundq))


## . Remove firms with fewer than 20 quarters (5 years) of continuous EPS data
## TODO: Deal with gaps


###############################################################################
## Lag and lead Variables
###############################################################################
## Sort by gvkey and quarter
comp.fundq <- comp.fundq[order(comp.fundq$gvkey, comp.fundq$datacqtr), ]

## Create a lag of quarter time variable to check for gaps in the time series 
## for each firm.
comp.fundq <- lag.df(comp.fundq, "qtr", "gvkey", "qtr")
comp.fundq$gap <- comp.fundq$qtr - comp.fundq$qtr.lag1
hist(comp.fundq$gap)
## Approx. 93% of the data is continuous chuncks (gap == 1)

## Create a one-quarter-ahead lead of EPS
comp.fundq <- lead.df(comp.fundq, "epspxq", "gvkey", "qtr")

###############################################################################
## Save data to project data folder
###############################################################################
comp.fundq <- comp.fundq[order(comp.fundq$gvkey, comp.fundq$datacqtr, decreasing = FALSE), ]
save(comp.fundq, file = '../0_datasets/comp_fundq.RData')
write.csv(x = comp.fundq, file = "../0_datasets/comp_fundq.csv")
save(comp.fundq.filters, file = "data/comp_fundq_filters.RData")
