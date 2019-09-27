## Reginald Edwards
## 16 June 2018
##
## Investigate missingness in Compustat fundamentals quarterly
rm(list=ls())
gc()

library(RPostgres)

## Raw Compustat data are in a PostgresQL database "import.comp_fundq"
localdb <- dbConnect(Postgres(), 
                     host='localhost',
                     port=5432,
                     user='postgres',
                     password = 'havoc')


## Investigate missingness
res <- dbSendQuery(localdb, "SELECT * from null_counts2;")
null_counts <- dbFetch(res)
dbClearResult(res)
null_counts$pctmissing <- 100*null_counts$sum/null_counts$count
null_counts <- null_counts[order(null_counts$pctmissing, decreasing = TRUE), ]
plot(ecdf(null_counts$pctmissing))
hist(null_counts$pctmissing)
