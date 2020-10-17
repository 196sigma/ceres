# Visualizing clusters of companies by fundamentals
rm(list=ls())
gc()
library(Rtsne)
load('data/comp_funda.RData')
###################################################################################################

firms <- comp.funda[comp.funda$fyear == 2017, c('fyear', 'gvkey', 'tic')]
fundamental.variables <- c('ni', 'at', 'sale','ceq')
fundamentals <- comp.funda[comp.funda$fyear == 2017, fundamental.variables]

fundamentals[is.na(fundamentals)] <- 0

#t-SNE
## Run the t-SNE algorithm and store the results into an object called tsne_results
tsne.results <- Rtsne(fundamentals, 
                      perplexity=30, 
                      check_duplicates = FALSE) 

## Generate the t_SNE plot
par(mfrow=c(1,1)) # To plot two images side-by-side
plot(tsne.results$Y, col = "blue", pch = 19, cex = 1.5) # Plotting the first image
