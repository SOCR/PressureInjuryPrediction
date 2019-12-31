#############################################################
###### Imputation script ####################################
#############################################################

static_data <- read.csv("Data/static_agg_data.csv", header=T, row.names = 1)
print("Static data")
print(dim(static_data))

library(missForest)
library(doParallel)

registerDoParallel(cores = 6)

#imp_static <- missForest(static_data, maxiter = 3, ntree = 10, parallelize = "variable", verbose = TRUE)

#imp_data <- imp_static$ximp

#print(imp_static$OOBerror)

#summary(imp_data)

#saveRDS(imp_data, file = "imp_static.rds")

#############

## Imputation using the mice package

library(mice)

## Define imputation function
impute <- function (fname) {
  
  raw_data <- read.csv(paste0("Analysis/",fname), header=T, row.names = 1)
  print(paste("Imputing", fname,"data"))
  print(dim(raw_data))
  imp_data <- mice(raw_data,m=3,maxit=10,meth='pmm',seed=500)
  saveRDS(imp_data, file = paste0(fname,"mice.rds"))
}

## Files list
files.name <- c("static_agg_data.csv", "braden_agg_perc.csv", "location_agg.csv", 
                "periop_numeric_agg.csv", "periop_yn_agg.csv", "surgical_data_agg.csv", 
                "ts_binary_agg.csv", "ts_numeric_agg.csv")


## Impute all data
#####################
for (i in files.name) {
  impute(i)
}

