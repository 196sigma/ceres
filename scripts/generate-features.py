## Reginald Edwards
## 23 July 2018
##
## Generate features for earnings forecasts

import pandas as pd

## Load data from CSV file 
fundq = pd.read_csv("../../0_datasets/comp_fundq.csv")
fundq[:3]
