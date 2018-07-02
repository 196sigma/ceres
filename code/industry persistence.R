rm(list=ls())
library(sas7bdat)
library(xtable)
X <- read.sas7bdat('../earnings forecasting/data/fy1_modeling.sas7bdat')

X.names <- names(X)
names(X) <- tolower(X.names)
n.firms <- length(unique(X$tic))
firms <- unique(as.character(X$tic))
X <- X[order(X$tic, X$fyear), ]
X <- X[which(X$nyears>10), ]

industries <- unique(X$ind)
n.ind <- length(industries)
persistence <- data.frame(matrix(NA, nrow=0, ncol=3))
names(persistence) <- c("ind", "beta","rsq")
for(i in 1:n.ind){
    o <- which(X$ind==industries[i])
    A <- X[o, ]
    m <- lm(actual ~ actual_lag1, data=A)
    persistence[i, ] <- c(industries[i], m$coeff[[2]], summary(m)$r.squared)
}

write.csv(x=persistence, file="eps-persistence-by-ind.csv", quote=FALSE, row.names=FALSE)


sic.to.industry <- function(siccd) {
    industry <- "other"
    if(siccd>=100 & siccd<=999) industry <- 'Consumer NonDurables'
    if(siccd>=2000 & siccd<=2399) industry <- 'Consumer NonDurables'
    if(siccd>=2700 & siccd<=2749) industry <- 'Consumer NonDurables'
    if(siccd>=2770 & siccd<=2799) industry <- 'Consumer NonDurables'
    if(siccd>=3100 & siccd<=3199) industry <- 'Consumer NonDurables'
    if(siccd>=3940 & siccd<=3989) industry <- 'Consumer NonDurables'
    if(siccd>=2500 & siccd<=2519) industry <- 'Consumer Durables'
    if(siccd>=2590 & siccd<=2599) industry <- 'Consumer Durables'
    if(siccd>=3630 & siccd<=3659) industry <- 'Consumer Durables'
    if(siccd>=3710 & siccd<=3711) industry <- 'Consumer Durables'
    if(siccd>=3714 & siccd<=3714) industry <- 'Consumer Durables'
    if(siccd>=3716 & siccd<=3716) industry <- 'Consumer Durables'
    if(siccd>=3750 & siccd<=3751) industry <- 'Consumer Durables'
    if(siccd>=3792 & siccd<=3792) industry <- 'Consumer Durables'
    if(siccd>=3900 & siccd<=3939) industry <- 'Consumer Durables'
    if(siccd>=3990 & siccd<=3999) industry <- 'Consumer Durables'
    if(siccd>=2520 & siccd<=2589) industry <- 'Manufacturing'
    if(siccd>=2600 & siccd<=2699) industry <- 'Manufacturing'
    if(siccd>=2750 & siccd<=2769) industry <- 'Manufacturing'
    if(siccd>=2800 & siccd<=2829) industry <- 'Manufacturing'
    if(siccd>=2840 & siccd<=2899) industry <- 'Manufacturing'
    if(siccd>=3000 & siccd<=3099) industry <- 'Manufacturing'
    if(siccd>=3200 & siccd<=3569) industry <- 'Manufacturing'
    if(siccd>=3580 & siccd<=3621) industry <- 'Manufacturing'
    if(siccd>=3623 & siccd<=3629) industry <- 'Manufacturing'
    if(siccd>=3700 & siccd<=3709) industry <- 'Manufacturing'
    if(siccd>=3712 & siccd<=3713) industry <- 'Manufacturing'
    if(siccd>=3715 & siccd<=3715) industry <- 'Manufacturing'
    if(siccd>=3717 & siccd<=3749) industry <- 'Manufacturing'
    if(siccd>=3752 & siccd<=3791) industry <- 'Manufacturing'
    if(siccd>=3793 & siccd<=3799) industry <- 'Manufacturing'
    if(siccd>=3860 & siccd<=3899) industry <- 'Manufacturing'
    if(siccd>=1200 & siccd<=1399) industry <- 'Enrgy Oil, Gas, and Coal Extraction and Products'
    if(siccd>=2900 & siccd<=2999) industry <- 'Enrgy Oil, Gas, and Coal Extraction and Products'
    if(siccd>=3570 & siccd<=3579) industry <- 'High-tech'
    if(siccd>=3622 & siccd<=3622) industry <- 'High-tech'
    if(siccd>=3660 & siccd<=3692) industry <- 'High-tech'
    if(siccd>=3694 & siccd<=3699) industry <- 'High-tech'
    if(siccd>=3810 & siccd<=3839) industry <- 'High-tech'
    if(siccd>=7370 & siccd<=7372) industry <- 'High-tech'
    if(siccd>=7373 & siccd<=7373) industry <- 'High-tech'
    if(siccd>=7374 & siccd<=7374) industry <- 'High-tech'
    if(siccd>=7375 & siccd<=7375) industry <- 'High-tech'
    if(siccd>=7376 & siccd<=7376) industry <- 'High-tech'
    if(siccd>=7377 & siccd<=7377) industry <- 'High-tech'
    if(siccd>=7378 & siccd<=7378) industry <- 'High-tech'
    if(siccd>=7379 & siccd<=7379) industry <- 'High-tech'
    if(siccd>=7391 & siccd<=7391) industry <- 'High-tech'
    if(siccd>=8730 & siccd<=8734) industry <- 'High-tech'
    if(siccd>=4800 & siccd<=4899) industry <- 'Telcm Telephone and Television Transmission'
    if(siccd>=5000 & siccd<=5999) industry <- 'Shops Wholesale, Retail, and Some Services'
    if(siccd>=7200 & siccd<=7299) industry <- 'Shops Wholesale, Retail, and Some Services'
    if(siccd>=7600 & siccd<=7699) industry <- 'Shops Wholesale, Retail, and Some Services'
    if(siccd>=2830 & siccd<=2839) industry <- 'Healthcare'
    if(siccd>=3693 & siccd<=3693) industry <- 'Healthcare'
    if(siccd>=3840 & siccd<=3859) industry <- 'Healthcare'
    if(siccd>=8000 & siccd<=8099) industry <- 'Healthcare'
    if(siccd>=4900 & siccd<=4949) industry <- 'Utilities'
    return(industry)
}

for(i in 1:nrow(X)) X[i, 'sicind'] <- sic.to.industry(X[i, 'siccd'])

sic.industries <- unique(X$sicind)
n.sicind <- length(sic.industries)
persistence2 <- data.frame(matrix(NA, nrow=0, ncol=3))
names(persistence2) <- c("sicind", "beta","rsq")
for(i in 1:n.sicind){
    o <- which(X$sicind==sic.industries[i])
    A <- X[o, ]
    m <- lm(actual ~ actual_lag1, data=A)
    persistence2[i, ] <- c(sic.industries[i], m$coeff[[2]], summary(m)$r.squared)
}

write.csv(x=persistence2, file="eps-persistence-by-sicind.csv", quote=FALSE, row.names=FALSE)
