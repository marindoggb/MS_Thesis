source("gogn_preprocess.R")  #load data and packages
# the data gognmNA has only the variables used for imputation (also pin for analysis model), 
#and all Cant/wont answer has been changed to NAs

# Begin by creating a imputations to ease making a prediction matrix and method for imputations
imp<-mice(gognmNA, m=1, maxit=0)
predM<-imp$predictorMatrix
predM[c(1:25), 1]<-0  # Not use pin for imputing
meth<-imp$method
meth[25]<-"logreg"  # use logistic regression for imputing coping ability as it is numeric for analysis model

## MI-PMM

imp_PMM_m25_WD <- mice(gognmNA,method="pmm",predictorMatrix = predM ,m=25, seed = 71332, print = FALSE,maxit=10) 
save(imp_PMM_m25_WD, file="imp_PMM_m25_WD")


## MI-MICE

imp_MICE_m25_WD <- mice(gognmNA,m=25,predictorMatrix = predM,method=meth, seed = 71332, print = FALSE,maxit=10) 
save(imp_MICE_m25_WD, file="imp_MICE_m25_WD")


## SI-PMM

imp_SI_m25_WD <- mice(gognmNA,method="pmm",predictorMatrix = predM,m=1, seed = 71332, print = FALSE,maxit=10) 
save(imp_SI_m25_WD, file="imp_SI_m25_WD")


