# Create imputation on each data with various levels of missingness and sample size - First MAR then MNAR

source("gogn_preprocess.R") # load data and packages


k_1000r_0.05<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.05") # load some dataset to create inital imp
#to construct prediction matrix

imp<-mice(k_1000r_0.05, m=1, maxit=0)
predM<-imp$predictorMatrix
predM[c(1:25), 1]<-0 # not use pin for imputations
meth<-imp$method



# 1000r 

## PMM

## 0.05 missingness
k_1000r_0.05<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.05") # load data with 0.05 proportion missing
PMM_ampute_05_1000r<-mice(k_1000r_0.05, method= "pmm" ,m=20,predictorMatrix = predM, maxit=15, seed=71332, print=FALSE)
save(PMM_ampute_05_1000r, file="imp_ampute_PMM_05_1000r")



## 0.10 missingness
k_1000r_0.1<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.1")
PMM_ampute_10_1000r<-mice(k_1000r_0.1, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_10_1000r, file="imp_ampute_PMM_10_1000r")


## 0.25 missingness
k_1000r_0.25<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.25")
PMM_ampute_25_1000r<-mice(k_1000r_0.25, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_25_1000r, file="imp_ampute_PMM_25_1000r")


## 0.50 missingness
k_1000r_0.5<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.5")
PMM_ampute_50_1000r<-mice(k_1000r_0.5, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_50_1000r, file="imp_ampute_PMM_50_1000r")

## 0.75 missingness
k_1000r_0.75<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.75")
PMM_ampute_75_1000r<-mice(k_1000r_0.75, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_75_1000r, file="imp_ampute_PMM_75_1000r")



## SI

## 0.05 missingness
SI_ampute_05_1000r<-mice(k_1000r_0.05, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_05_1000r, file="imp_ampute_SI_05_1000r")


## 0.10 missingness
SI_ampute_10_1000r<-mice(k_1000r_0.1, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_10_1000r, file="imp_ampute_SI_10_1000r")


## 0.25 missingness
SI_ampute_25_1000r<-mice(k_1000r_0.25, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_25_1000r, file="imp_ampute_SI_25_1000r")


## 0.50 missingness
SI_ampute_50_1000r<-mice(k_1000r_0.5, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_50_1000r, file="imp_ampute_SI_50_1000r")

## 0.75 missingness
SI_ampute_75_1000r<-mice(k_1000r_0.75, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_75_1000r, file="imp_ampute_SI_75_1000r")




## MICE

## 0.05 missingness
MICE_ampute_05_1000r<-mice(k_1000r_0.05,  m=20,predictorMatrix = predM,method=meth, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_05_1000r, file="imp_ampute_MICE_05_1000r")


## 0.10 missingness
MICE_ampute_10_1000r<-mice(k_1000r_0.1,  m=20,predictorMatrix = predM,method=meth, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_10_1000r, file="imp_ampute_MICE_10_1000r")


## 0.25 missingness
MICE_ampute_25_1000r<-mice(k_1000r_0.25,  m=20,predictorMatrix = predM,method=meth, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_25_1000r, file="imp_ampute_MICE_25_1000r")


## 0.50 missingness
MICE_ampute_50_1000r<-mice(k_1000r_0.5,  m=20,predictorMatrix = predM,method=meth, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_50_1000r, file="imp_ampute_MICE_50_1000r")

## 0.75 missingness
MICE_ampute_75_1000r<-mice(k_1000r_0.75,  m=20,predictorMatrix = predM,method=meth, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_75_1000r, file="imp_ampute_MICE_75_1000r")









# 5000r 

## PMM

## 0.05 missingness
k_5000r_0.05<-readRDS("./gogn_og_imp/gogn_fImpute_5000r_0.05")
PMM_ampute_05_5000r<-mice(k_5000r_0.05, method= "pmm" ,m=20,predictorMatrix = predM, maxit=15, seed=71332, print=FALSE)
save(PMM_ampute_05_5000r, file="imp_ampute_PMM_05_5000r")



## 0.10 missingness
k_5000r_0.1<-readRDS("./gogn_og_imp/gogn_fImpute_5000r_0.1")
PMM_ampute_10_5000r<-mice(k_5000r_0.1, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_10_5000r, file="imp_ampute_PMM_10_5000r")


## 0.25 missingness
k_5000r_0.25<-readRDS("./gogn_og_imp/gogn_fImpute_5000r_0.25")
PMM_ampute_25_5000r<-mice(k_5000r_0.25, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_25_5000r, file="imp_ampute_PMM_25_5000r")


## 0.50 missingness
k_5000r_0.5<-readRDS("./gogn_og_imp/gogn_fImpute_5000r_0.5")
PMM_ampute_50_5000r<-mice(k_5000r_0.5, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_50_5000r, file="imp_ampute_PMM_50_5000r")

## 0.75 missingness
k_5000r_0.75<-readRDS("./gogn_og_imp/gogn_fImpute_5000r_0.75")
PMM_ampute_75_5000r<-mice(k_5000r_0.75, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_75_5000r, file="imp_ampute_PMM_75_5000r")



## SI

## 0.05 missingness
SI_ampute_05_5000r<-mice(k_5000r_0.05, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_05_5000r, file="imp_ampute_SI_05_5000r")


## 0.10 missingness
SI_ampute_10_5000r<-mice(k_5000r_0.1, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_10_5000r, file="imp_ampute_SI_10_5000r")


## 0.25 missingness
SI_ampute_25_5000r<-mice(k_5000r_0.25, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_25_5000r, file="imp_ampute_SI_25_5000r")


## 0.50 missingness
SI_ampute_50_5000r<-mice(k_5000r_0.5, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_50_5000r, file="imp_ampute_SI_50_5000r")

## 0.75 missingness
SI_ampute_75_5000r<-mice(k_5000r_0.75, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_75_5000r, file="imp_ampute_SI_75_5000r")




## MICE

## 0.05 missingness
MICE_ampute_05_5000r<-mice(k_5000r_0.05,  m=20,predictorMatrix = predM,method=meth, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_05_5000r, file="imp_ampute_MICE_05_5000r")


## 0.10 missingness
MICE_ampute_10_5000r<-mice(k_5000r_0.1,  m=20,predictorMatrix = predM,method=meth, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_10_5000r, file="imp_ampute_MICE_10_5000r")


## 0.25 missingness
MICE_ampute_25_5000r<-mice(k_5000r_0.25,  m=20,predictorMatrix = predM,method=meth, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_25_5000r, file="imp_ampute_MICE_25_5000r")


## 0.50 missingness
MICE_ampute_50_5000r<-mice(k_5000r_0.5,  m=20,predictorMatrix = predM,method=meth, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_50_5000r, file="imp_ampute_MICE_50_5000r")

## 0.75 missingness
MICE_ampute_75_5000r<-mice(k_5000r_0.75,  m=20,predictorMatrix = predM,method=meth, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_75_5000r, file="imp_ampute_MICE_75_5000r")










# 10000r 

## PMM

## 0.05 missingness
k_10000r_0.05<-readRDS("./gogn_og_imp/gogn_fImpute_10000r_0.05")
PMM_ampute_05_10000r<-mice(k_10000r_0.05, method= "pmm" ,m=20,predictorMatrix = predM, maxit=15, seed=71332, print=FALSE)
save(PMM_ampute_05_10000r, file="imp_ampute_PMM_05_10000r")



## 0.10 missingness
k_10000r_0.1<-readRDS("./gogn_og_imp/gogn_fImpute_10000r_0.1")
PMM_ampute_10_10000r<-mice(k_10000r_0.1, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_10_10000r, file="imp_ampute_PMM_10_10000r")


## 0.25 missingness
k_10000r_0.25<-readRDS("./gogn_og_imp/gogn_fImpute_10000r_0.25")
PMM_ampute_25_10000r<-mice(k_10000r_0.25, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_25_10000r, file="imp_ampute_PMM_25_10000r")


## 0.50 missingness
k_10000r_0.5<-readRDS("./gogn_og_imp/gogn_fImpute_10000r_0.5")
PMM_ampute_50_10000r<-mice(k_10000r_0.5, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_50_10000r, file="imp_ampute_PMM_50_10000r")

## 0.75 missingness
k_10000r_0.75<-readRDS("./gogn_og_imp/gogn_fImpute_10000r_0.75")
PMM_ampute_75_10000r<-mice(k_10000r_0.75, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_75_10000r, file="imp_ampute_PMM_75_10000r")



## SI

## 0.05 missingness
SI_ampute_05_10000r<-mice(k_10000r_0.05, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_05_10000r, file="imp_ampute_SI_05_10000r")


## 0.10 missingness
SI_ampute_10_10000r<-mice(k_10000r_0.1, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_10_10000r, file="imp_ampute_SI_10_10000r")


## 0.25 missingness
SI_ampute_25_10000r<-mice(k_10000r_0.25, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_25_10000r, file="imp_ampute_SI_25_10000r")


## 0.50 missingness
SI_ampute_50_10000r<-mice(k_10000r_0.5, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_50_10000r, file="imp_ampute_SI_50_10000r")

## 0.75 missingness
SI_ampute_75_10000r<-mice(k_10000r_0.75, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_75_10000r, file="imp_ampute_SI_75_10000r")




## MICE

## 0.05 missingness
MICE_ampute_05_10000r<-mice(k_10000r_0.05,  m=20,predictorMatrix = predM,method=meth, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_05_10000r, file="imp_ampute_MICE_05_10000r")


## 0.10 missingness
MICE_ampute_10_10000r<-mice(k_10000r_0.1,  m=20,predictorMatrix = predM,method=meth, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_10_10000r, file="imp_ampute_MICE_10_10000r")


## 0.25 missingness
MICE_ampute_25_10000r<-mice(k_10000r_0.25,  m=20,predictorMatrix = predM,method=meth, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_25_10000r, file="imp_ampute_MICE_25_10000r")


## 0.50 missingness
MICE_ampute_50_10000r<-mice(k_10000r_0.5,  m=20,predictorMatrix = predM,method=meth, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_50_10000r, file="imp_ampute_MICE_50_10000r")

## 0.75 missingness
MICE_ampute_75_10000r<-mice(k_10000r_0.75,  m=20,predictorMatrix = predM,method=meth, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_75_10000r, file="imp_ampute_MICE_75_10000r")











# 15000r 

## PMM

## 0.05 missingness
k_15000r_0.05<-readRDS("./gogn_og_imp/gogn_fImpute_15000r_0.05")
PMM_ampute_05_15000r<-mice(k_15000r_0.05, method= "pmm" ,m=20,predictorMatrix = predM, maxit=15, seed=71332, print=FALSE)
save(PMM_ampute_05_15000r, file="imp_ampute_PMM_05_15000r")



## 0.10 missingness
k_15000r_0.1<-readRDS("./gogn_og_imp/gogn_fImpute_15000r_0.1")
PMM_ampute_10_15000r<-mice(k_15000r_0.1, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_10_15000r, file="imp_ampute_PMM_10_15000r")


## 0.25 missingness
k_15000r_0.25<-readRDS("./gogn_og_imp/gogn_fImpute_15000r_0.25")
PMM_ampute_25_15000r<-mice(k_15000r_0.25, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_25_15000r, file="imp_ampute_PMM_25_15000r")


## 0.50 missingness
k_15000r_0.5<-readRDS("./gogn_og_imp/gogn_fImpute_15000r_0.5")
PMM_ampute_50_15000r<-mice(k_15000r_0.5, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_50_15000r, file="imp_ampute_PMM_50_15000r")

## 0.75 missingness
k_15000r_0.75<-readRDS("./gogn_og_imp/gogn_fImpute_15000r_0.75")
PMM_ampute_75_15000r<-mice(k_15000r_0.75, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_75_15000r, file="imp_ampute_PMM_75_15000r")



## SI

## 0.05 missingness
SI_ampute_05_15000r<-mice(k_15000r_0.05, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_05_15000r, file="imp_ampute_SI_05_15000r")


## 0.10 missingness
SI_ampute_10_15000r<-mice(k_15000r_0.1, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_10_15000r, file="imp_ampute_SI_10_15000r")


## 0.25 missingness
SI_ampute_25_15000r<-mice(k_15000r_0.25, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_25_15000r, file="imp_ampute_SI_25_15000r")


## 0.50 missingness
SI_ampute_50_15000r<-mice(k_15000r_0.5, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_50_15000r, file="imp_ampute_SI_50_15000r")

## 0.75 missingness
SI_ampute_75_15000r<-mice(k_15000r_0.75, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_75_15000r, file="imp_ampute_SI_75_15000r")




## MICE

## 0.05 missingness
MICE_ampute_05_15000r<-mice(k_15000r_0.05,  m=20,predictorMatrix = predM,method=meth, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_05_15000r, file="imp_ampute_MICE_05_15000r")


## 0.10 missingness
MICE_ampute_10_15000r<-mice(k_15000r_0.1,  m=20,predictorMatrix = predM,method=meth, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_10_15000r, file="imp_ampute_MICE_10_15000r")


## 0.25 missingness
MICE_ampute_25_15000r<-mice(k_15000r_0.25,  m=20,predictorMatrix = predM,method=meth, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_25_15000r, file="imp_ampute_MICE_25_15000r")


## 0.50 missingness
MICE_ampute_50_15000r<-mice(k_15000r_0.5,  m=20,predictorMatrix = predM,method=meth, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_50_15000r, file="imp_ampute_MICE_50_15000r")

## 0.75 missingness
MICE_ampute_75_15000r<-mice(k_15000r_0.75,  m=20,predictorMatrix = predM,method=meth, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_75_15000r, file="imp_ampute_MICE_75_15000r")














# 20000r 

## PMM

## 0.05 missingness
k_20000r_0.05<-readRDS("./gogn_og_imp/gogn_fImpute_20000r_0.05")
PMM_ampute_05_20000r<-mice(k_20000r_0.05, method= "pmm" ,m=20,predictorMatrix = predM, maxit=15, seed=71332, print=FALSE)
save(PMM_ampute_05_20000r, file="imp_ampute_PMM_05_20000r")



## 0.10 missingness
k_20000r_0.1<-readRDS("./gogn_og_imp/gogn_fImpute_20000r_0.1")
PMM_ampute_10_20000r<-mice(k_20000r_0.1, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_10_20000r, file="imp_ampute_PMM_10_20000r")


## 0.25 missingness
k_20000r_0.25<-readRDS("./gogn_og_imp/gogn_fImpute_20000r_0.25")
PMM_ampute_25_20000r<-mice(k_20000r_0.25, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_25_20000r, file="imp_ampute_PMM_25_20000r")


## 0.50 missingness
k_20000r_0.5<-readRDS("./gogn_og_imp/gogn_fImpute_20000r_0.5")
PMM_ampute_50_20000r<-mice(k_20000r_0.5, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_50_20000r, file="imp_ampute_PMM_50_20000r")

## 0.75 missingness
k_20000r_0.75<-readRDS("./gogn_og_imp/gogn_fImpute_20000r_0.75")
PMM_ampute_75_20000r<-mice(k_20000r_0.75, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_75_20000r, file="imp_ampute_PMM_75_20000r")



## SI

## 0.05 missingness
SI_ampute_05_20000r<-mice(k_20000r_0.05, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_05_20000r, file="imp_ampute_SI_05_20000r")


## 0.10 missingness
SI_ampute_10_20000r<-mice(k_20000r_0.1, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_10_20000r, file="imp_ampute_SI_10_20000r")


## 0.25 missingness
SI_ampute_25_20000r<-mice(k_20000r_0.25, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_25_20000r, file="imp_ampute_SI_25_20000r")


## 0.50 missingness
SI_ampute_50_20000r<-mice(k_20000r_0.5, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_50_20000r, file="imp_ampute_SI_50_20000r")

## 0.75 missingness
SI_ampute_75_20000r<-mice(k_20000r_0.75, method= "pmm", m=1, predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_75_20000r, file="imp_ampute_SI_75_20000r")




## MICE

## 0.05 missingness
MICE_ampute_05_20000r<-mice(k_20000r_0.05,  m=20,predictorMatrix = predM,method=meth, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_05_20000r, file="imp_ampute_MICE_05_20000r")


## 0.10 missingness
MICE_ampute_10_20000r<-mice(k_20000r_0.1,  m=20,predictorMatrix = predM,method=meth, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_10_20000r, file="imp_ampute_MICE_10_20000r")


## 0.25 missingness
MICE_ampute_25_20000r<-mice(k_20000r_0.25,  m=20,predictorMatrix = predM,method=meth, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_25_20000r, file="imp_ampute_MICE_25_20000r")


## 0.50 missingness
MICE_ampute_50_20000r<-mice(k_20000r_0.5,  m=20,predictorMatrix = predM,method=meth, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_50_20000r, file="imp_ampute_MICE_50_20000r")

## 0.75 missingness
MICE_ampute_75_20000r<-mice(k_20000r_0.75,  m=20,predictorMatrix = predM,method=meth, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_75_20000r, file="imp_ampute_MICE_75_20000r")





### MNAR







# 1000r_MNAR 

## PMM

## 0.05 missingness
k_1000r_MNAR_0.05<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_1000r_MNAR_0.05")
PMM_ampute_05_1000r_MNAR<-mice(k_1000r_MNAR_0.05, method= "pmm" ,m=20,predictorMatrix = predM, maxit=15, seed=71332, print=FALSE)
save(PMM_ampute_05_1000r_MNAR, file="imp_ampute_PMM_05_1000r_MNAR")



## 0.10 missingness
k_1000r_MNAR_0.1<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_1000r_MNAR_0.1")
PMM_ampute_10_1000r_MNAR<-mice(k_1000r_MNAR_0.1, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_10_1000r_MNAR, file="imp_ampute_PMM_10_1000r_MNAR")


## 0.25 missingness
k_1000r_MNAR_0.25<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_1000r_MNAR_0.25")
PMM_ampute_25_1000r_MNAR<-mice(k_1000r_MNAR_0.25, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_25_1000r_MNAR, file="imp_ampute_PMM_25_1000r_MNAR")


## 0.50 missingness
k_1000r_MNAR_0.5<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_1000r_MNAR_0.5")
PMM_ampute_50_1000r_MNAR<-mice(k_1000r_MNAR_0.5, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_50_1000r_MNAR, file="imp_ampute_PMM_50_1000r_MNAR")

## 0.75 missingness
k_1000r_MNAR_0.75<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_1000r_MNAR_0.75")
PMM_ampute_75_1000r_MNAR<-mice(k_1000r_MNAR_0.75, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_75_1000r_MNAR, file="imp_ampute_PMM_75_1000r_MNAR")






## MICE

## 0.05 missingness
k_1000r_MNAR_0.05<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_1000r_MNAR_0.05")
MICE_ampute_05_1000r_MNAR<-mice(k_1000r_MNAR_0.05, method= meth ,m=20,predictorMatrix = predM, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_05_1000r_MNAR, file="imp_ampute_MICE_05_1000r_MNAR")



## 0.10 missingness
k_1000r_MNAR_0.1<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_1000r_MNAR_0.1")
MICE_ampute_10_1000r_MNAR<-mice(k_1000r_MNAR_0.1, method= meth, m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_10_1000r_MNAR, file="imp_ampute_MICE_10_1000r_MNAR")


## 0.25 missingness
k_1000r_MNAR_0.25<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_1000r_MNAR_0.25")
MICE_ampute_25_1000r_MNAR<-mice(k_1000r_MNAR_0.25, method= meth, m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_25_1000r_MNAR, file="imp_ampute_MICE_25_1000r_MNAR")


## 0.50 missingness
k_1000r_MNAR_0.5<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_1000r_MNAR_0.5")
MICE_ampute_50_1000r_MNAR<-mice(k_1000r_MNAR_0.5, method= meth, m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_50_1000r_MNAR, file="imp_ampute_MICE_50_1000r_MNAR")

## 0.75 missingness
k_1000r_MNAR_0.75<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_1000r_MNAR_0.75")
MICE_ampute_75_1000r_MNAR<-mice(k_1000r_MNAR_0.75, method= meth, m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_75_1000r_MNAR, file="imp_ampute_MICE_75_1000r_MNAR")






## SI

## 0.05 missingness
k_1000r_MNAR_0.05<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_1000r_MNAR_0.05")
SI_ampute_05_1000r_MNAR<-mice(k_1000r_MNAR_0.05, method= "pmm" ,m=1,predictorMatrix = predM, maxit=15, seed=71332, print=FALSE)
save(SI_ampute_05_1000r_MNAR, file="imp_ampute_SI_05_1000r_MNAR")



## 0.10 missingness
k_1000r_MNAR_0.1<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_1000r_MNAR_0.1")
SI_ampute_10_1000r_MNAR<-mice(k_1000r_MNAR_0.1, method= "pmm", m=1,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_10_1000r_MNAR, file="imp_ampute_SI_10_1000r_MNAR")


## 0.25 missingness
k_1000r_MNAR_0.25<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_1000r_MNAR_0.25")
SI_ampute_25_1000r_MNAR<-mice(k_1000r_MNAR_0.25, method= "pmm", m=1,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_25_1000r_MNAR, file="imp_ampute_SI_25_1000r_MNAR")


## 0.50 missingness
k_1000r_MNAR_0.5<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_1000r_MNAR_0.5")
SI_ampute_50_1000r_MNAR<-mice(k_1000r_MNAR_0.5, method= "pmm", m=1,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_50_1000r_MNAR, file="imp_ampute_SI_50_1000r_MNAR")

## 0.75 missingness
k_1000r_MNAR_0.75<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_1000r_MNAR_0.75")
SI_ampute_75_1000r_MNAR<-mice(k_1000r_MNAR_0.75, method= "pmm", m=1,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_75_1000r_MNAR, file="imp_ampute_SI_75_1000r_MNAR")












# 10000r_MNAR 

## PMM

## 0.05 missingness
k_10000r_MNAR_0.05<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_10000r_MNAR_0.05")
PMM_ampute_05_10000r_MNAR<-mice(k_10000r_MNAR_0.05, method= "pmm" ,m=20,predictorMatrix = predM, maxit=15, seed=71332, print=FALSE)
save(PMM_ampute_05_10000r_MNAR, file="imp_ampute_PMM_05_10000r_MNAR")



## 0.10 missingness
k_10000r_MNAR_0.1<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_10000r_MNAR_0.1")
PMM_ampute_10_10000r_MNAR<-mice(k_10000r_MNAR_0.1, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_10_10000r_MNAR, file="imp_ampute_PMM_10_10000r_MNAR")


## 0.25 missingness
k_10000r_MNAR_0.25<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_10000r_MNAR_0.25")
PMM_ampute_25_10000r_MNAR<-mice(k_10000r_MNAR_0.25, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_25_10000r_MNAR, file="imp_ampute_PMM_25_10000r_MNAR")


## 0.50 missingness
k_10000r_MNAR_0.5<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_10000r_MNAR_0.5")
PMM_ampute_50_10000r_MNAR<-mice(k_10000r_MNAR_0.5, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_50_10000r_MNAR, file="imp_ampute_PMM_50_10000r_MNAR")

## 0.75 missingness
k_10000r_MNAR_0.75<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_10000r_MNAR_0.75")
PMM_ampute_75_10000r_MNAR<-mice(k_10000r_MNAR_0.75, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_75_10000r_MNAR, file="imp_ampute_PMM_75_10000r_MNAR")






## MICE

## 0.05 missingness
k_10000r_MNAR_0.05<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_10000r_MNAR_0.05")
MICE_ampute_05_10000r_MNAR<-mice(k_10000r_MNAR_0.05, method= meth ,m=20,predictorMatrix = predM, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_05_10000r_MNAR, file="imp_ampute_MICE_05_10000r_MNAR")



## 0.10 missingness
k_10000r_MNAR_0.1<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_10000r_MNAR_0.1")
MICE_ampute_10_10000r_MNAR<-mice(k_10000r_MNAR_0.1, method= meth, m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_10_10000r_MNAR, file="imp_ampute_MICE_10_10000r_MNAR")


## 0.25 missingness
k_10000r_MNAR_0.25<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_10000r_MNAR_0.25")
MICE_ampute_25_10000r_MNAR<-mice(k_10000r_MNAR_0.25, method= meth, m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_25_10000r_MNAR, file="imp_ampute_MICE_25_10000r_MNAR")


## 0.50 missingness
k_10000r_MNAR_0.5<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_10000r_MNAR_0.5")
MICE_ampute_50_10000r_MNAR<-mice(k_10000r_MNAR_0.5, method= meth, m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_50_10000r_MNAR, file="imp_ampute_MICE_50_10000r_MNAR")

## 0.75 missingness
k_10000r_MNAR_0.75<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_10000r_MNAR_0.75")
MICE_ampute_75_10000r_MNAR<-mice(k_10000r_MNAR_0.75, method= meth, m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_75_10000r_MNAR, file="imp_ampute_MICE_75_10000r_MNAR")






## SI

## 0.05 missingness

k_10000r_MNAR_0.05<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_10000r_MNAR_0.05")
SI_ampute_05_10000r_MNAR<-mice(k_10000r_MNAR_0.05, method= "pmm" ,m=1,predictorMatrix = predM, maxit=15, seed=71332, print=FALSE)
save(SI_ampute_05_10000r_MNAR, file="imp_ampute_SI_05_10000r_MNAR")



## 0.10 missingness

k_10000r_MNAR_0.1<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_10000r_MNAR_0.1")
SI_ampute_10_10000r_MNAR<-mice(k_10000r_MNAR_0.1, method= "pmm", m=1,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_10_10000r_MNAR, file="imp_ampute_SI_10_10000r_MNAR")


## 0.25 missingness
k_10000r_MNAR_0.25<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_10000r_MNAR_0.25")
SI_ampute_25_10000r_MNAR<-mice(k_10000r_MNAR_0.25, method= "pmm", m=1,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_25_10000r_MNAR, file="imp_ampute_SI_25_10000r_MNAR")


## 0.50 missingness
k_10000r_MNAR_0.5<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_10000r_MNAR_0.5")
SI_ampute_50_10000r_MNAR<-mice(k_10000r_MNAR_0.5, method= "pmm", m=1,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_50_10000r_MNAR, file="imp_ampute_SI_50_10000r_MNAR")

## 0.75 missingness
k_10000r_MNAR_0.75<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_10000r_MNAR_0.75")
SI_ampute_75_10000r_MNAR<-mice(k_10000r_MNAR_0.75, method= "pmm", m=1,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_75_10000r_MNAR, file="imp_ampute_SI_75_10000r_MNAR")












# 20000r_MNAR 

## PMM
 
## 0.05 missingness
k_20000r_MNAR_0.05<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_20000r_MNAR_0.05")
PMM_ampute_05_20000r_MNAR<-mice(k_20000r_MNAR_0.05, method= "pmm" ,m=20,predictorMatrix = predM, maxit=15, seed=71332, print=FALSE)
save(PMM_ampute_05_20000r_MNAR, file="imp_ampute_PMM_05_20000r_MNAR")



## 0.10 missingness
k_20000r_MNAR_0.1<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_20000r_MNAR_0.1")
PMM_ampute_10_20000r_MNAR<-mice(k_20000r_MNAR_0.1, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_10_20000r_MNAR, file="imp_ampute_PMM_10_20000r_MNAR")


## 0.25 missingness

k_20000r_MNAR_0.25<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_20000r_MNAR_0.25")
PMM_ampute_25_20000r_MNAR<-mice(k_20000r_MNAR_0.25, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_25_20000r_MNAR, file="imp_ampute_PMM_25_20000r_MNAR")


## 0.50 missingness
k_20000r_MNAR_0.5<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_20000r_MNAR_0.5")
PMM_ampute_50_20000r_MNAR<-mice(k_20000r_MNAR_0.5, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_50_20000r_MNAR, file="imp_ampute_PMM_50_20000r_MNAR")

## 0.75 missingness
k_20000r_MNAR_0.75<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_20000r_MNAR_0.75")
PMM_ampute_75_20000r_MNAR<-mice(k_20000r_MNAR_0.75, method= "pmm", m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(PMM_ampute_75_20000r_MNAR, file="imp_ampute_PMM_75_20000r_MNAR")






## MICE

## 0.05 missingness

k_20000r_MNAR_0.05<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_20000r_MNAR_0.05")
MICE_ampute_05_20000r_MNAR<-mice(k_20000r_MNAR_0.05, method= meth ,m=20,predictorMatrix = predM, maxit=15, seed=71332, print=FALSE)
save(MICE_ampute_05_20000r_MNAR, file="imp_ampute_MICE_05_20000r_MNAR")



## 0.10 missingness

k_20000r_MNAR_0.1<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_20000r_MNAR_0.1")
MICE_ampute_10_20000r_MNAR<-mice(k_20000r_MNAR_0.1, method= meth, m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_10_20000r_MNAR, file="imp_ampute_MICE_10_20000r_MNAR")


## 0.25 missingness

k_20000r_MNAR_0.25<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_20000r_MNAR_0.25")
MICE_ampute_25_20000r_MNAR<-mice(k_20000r_MNAR_0.25, method= meth, m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_25_20000r_MNAR, file="imp_ampute_MICE_25_20000r_MNAR")


## 0.50 missingness

k_20000r_MNAR_0.5<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_20000r_MNAR_0.5")
MICE_ampute_50_20000r_MNAR<-mice(k_20000r_MNAR_0.5, method= meth, m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_50_20000r_MNAR, file="imp_ampute_MICE_50_20000r_MNAR")

## 0.75 missingness

k_20000r_MNAR_0.75<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_20000r_MNAR_0.75")
MICE_ampute_75_20000r_MNAR<-mice(k_20000r_MNAR_0.75, method= meth, m=20,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(MICE_ampute_75_20000r_MNAR, file="imp_ampute_MICE_75_20000r_MNAR")






## SI

## 0.05 missingness

k_20000r_MNAR_0.05<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_20000r_MNAR_0.05")
SI_ampute_05_20000r_MNAR<-mice(k_20000r_MNAR_0.05, method= "pmm" ,m=1,predictorMatrix = predM, maxit=15, seed=71332, print=FALSE)
save(SI_ampute_05_20000r_MNAR, file="imp_ampute_SI_05_20000r_MNAR")



## 0.10 missingness

k_20000r_MNAR_0.1<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_20000r_MNAR_0.1")
SI_ampute_10_20000r_MNAR<-mice(k_20000r_MNAR_0.1, method= "pmm", m=1,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_10_20000r_MNAR, file="imp_ampute_SI_10_20000r_MNAR")


## 0.25 missingness

k_20000r_MNAR_0.25<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_20000r_MNAR_0.25")
SI_ampute_25_20000r_MNAR<-mice(k_20000r_MNAR_0.25, method= "pmm", m=1,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_25_20000r_MNAR, file="imp_ampute_SI_25_20000r_MNAR")


## 0.50 missingness

k_20000r_MNAR_0.5<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_20000r_MNAR_0.5")
SI_ampute_50_20000r_MNAR<-mice(k_20000r_MNAR_0.5, method= "pmm", m=1,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_50_20000r_MNAR, file="imp_ampute_SI_50_20000r_MNAR")

## 0.75 missingness

k_20000r_MNAR_0.75<-readRDS("./MNAR/gogn_og_imp/gogn_fImpute_20000r_MNAR_0.75")
SI_ampute_75_20000r_MNAR<-mice(k_20000r_MNAR_0.75, method= "pmm", m=1,predictorMatrix = predM, maxit=15,  seed=71332, print=FALSE)
save(SI_ampute_75_20000r_MNAR, file="imp_ampute_SI_75_20000r_MNAR")



