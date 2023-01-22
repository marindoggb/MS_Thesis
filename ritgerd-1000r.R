#load the data with various levels of missing
gogn_fImpute_1000r_0.05<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.05")
gogn_fImpute_1000r_0.1<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.1")
gogn_fImpute_1000r_0.25<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.25")
gogn_fImpute_1000r_0.5<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.5")
gogn_fImpute_1000r_0.75<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.75")
load("./gogn_og_imp/gognDNA1000r") # load 1000r data before missingness was created



### CCA before missingness

fit_CC<-geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ 
                 agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)
               +factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin), data=gognDNA1000r_rett)
fit_CC_tidy<-tidy(fit_CC, exp=TRUE, conf.int = TRUE) # For comparisons later




### PMM
#load the imputations
load("./gogn_og_imp/imp_ampute_PMM_05_1000r")
load("./gogn_og_imp/imp_ampute_PMM_10_1000r")
load("./gogn_og_imp/imp_ampute_PMM_25_1000r")
load("./gogn_og_imp/imp_ampute_PMM_50_1000r")
load("./gogn_og_imp/imp_ampute_PMM_75_1000r")

### 0.05
#begin by fitting the datasets 
fit_ampute_pmm05 <- with(PMM_ampute_05_1000r, geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
est_ampute_pmm05 <- pool(fit_ampute_pmm05) # pool estimates
coef_ampute_pmm05<-summary(pool(fit_ampute_pmm05), exp=TRUE, conf.int=TRUE) # for later comparisons



### Evaluation PMM


fit<-fit_ampute_pmm05 

coef<-14 #number of coefficients in model
m<-20 #number of imputations

a <- summary(fit)$estimate # Create a dataframe with all estimates from each fit to use for calculating coverage and PB and more
datsss<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss[i]<-a[seq(i, length(a), coef)]
}
datsss<-data.frame(t(datsss))

b <- summary(fit)$std.error # same thing as above except for confidence intervals
datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_error[i]<-b[seq(i, length(b), coef)]
}
datsss_error<-data.frame(t(datsss_error))

true <- fit_CC$coefficients # mark the correct estimate from the data before missingness was created
ci1<-(datsss-(1.96*datsss_error)) #lower CI
ci2<-(datsss+(1.96*datsss_error)) #higher CI
rM<-rowMeans(datsss) #just to simplify the following calculations
RB <- rM - true #raw  bias
PB <- 100 * abs((rM - true)/ true) #percentage bias
CR <- rowMeans(ci1< true & true < ci2) #coverage
AW <- rowMeans(ci2 - ci1) #average width of CI
RMSE <- sqrt(rowMeans((datsss - true)^2)) #RMSE
ci1<-rowMeans(ci1) #lower CI for comparisons
ci2<-rowMeans(ci2) # higher CI for comparisons
huhu05_PMM_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2) # create data frame to print

#Print for use in Latex
library(foreign)
library(xtable)
library(stargazer)

print(xtable(huhu05_PMM_1000r, digits=4,type = "latex"), file = "huhu05_PMM_1000r.tex")



#### the rest is all the same except with different level of missingness and different methods

### 0.1

fit_ampute_pmm10 <- with(PMM_ampute_10_1000r, geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
est_ampute_pmm10 <- pool(fit_ampute_pmm10) 
coef_ampute_pmm10<-summary(pool(fit_ampute_pmm10) , exp=TRUE, conf.int=TRUE)



### Evaluation PMM


fit<-fit_ampute_pmm10

coef<-14
m<-20

a <- summary(fit)$estimate
datsss<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss[i]<-a[seq(i, length(a), coef)]
}
datsss<-data.frame(t(datsss))

b <- summary(fit)$std.error
datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_error[i]<-b[seq(i, length(b), coef)]
}
datsss_error<-data.frame(t(datsss_error))

true <- fit_CC$coefficients
ci1<-(datsss-(1.96*datsss_error))
ci2<-(datsss+(1.96*datsss_error))
rM<-rowMeans(datsss)
RB <- rM - true
PB <- 100 * abs((rM - true)/ true)
CR <- rowMeans(ci1< true & true < ci2)
AW <- rowMeans(ci2 - ci1)
RMSE <- sqrt(rowMeans((datsss - true)^2))
ci1<-rowMeans(ci1)
ci2<-rowMeans(ci2)
huhu10_PMM_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2)

print(xtable(huhu10_PMM_1000r, digits=4,type = "latex"), file = "huhu10_PMM_1000r.tex")









### 0.25

fit_ampute_pmm25 <- with(PMM_ampute_25_1000r, geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
est_ampute_pmm25 <- pool(fit_ampute_pmm25) 
coef_ampute_pmm25<-summary(pool(fit_ampute_pmm25) , exp=TRUE, conf.int=TRUE)



### Evaluation PMM


fit<-fit_ampute_pmm25

coef<-14
m<-20

a <- summary(fit)$estimate
datsss<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss[i]<-a[seq(i, length(a), coef)]
}
datsss<-data.frame(t(datsss))

b <- summary(fit)$std.error
datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_error[i]<-b[seq(i, length(b), coef)]
}
datsss_error<-data.frame(t(datsss_error))

true <- fit_CC$coefficients
ci1<-(datsss-(1.96*datsss_error))
ci2<-(datsss+(1.96*datsss_error))
rM<-rowMeans(datsss)
RB <- rM - true
PB <- 100 * abs((rM - true)/ true)
CR <- rowMeans(ci1< true & true < ci2)
AW <- rowMeans(ci2 - ci1)
RMSE <- sqrt(rowMeans((datsss - true)^2))
ci1<-rowMeans(ci1)
ci2<-rowMeans(ci2)
huhu25_PMM_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2)

print(xtable(huhu25_PMM_1000r, digits=4,type = "latex"), file = "huhu25_PMM_1000r.tex")









### 0.5

fit_ampute_pmm50 <- with(PMM_ampute_50_1000r, geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
est_ampute_pmm50 <- pool(fit_ampute_pmm50) 
coef_ampute_pmm50<-summary(pool(fit_ampute_pmm50) , exp=TRUE, conf.int=TRUE)



### Evaluation PMM


fit<-fit_ampute_pmm50

coef<-14
m<-20

a <- summary(fit)$estimate
datsss<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss[i]<-a[seq(i, length(a), coef)]
}
datsss<-data.frame(t(datsss))

b <- summary(fit)$std.error
datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_error[i]<-b[seq(i, length(b), coef)]
}
datsss_error<-data.frame(t(datsss_error))

true <- fit_CC$coefficients
ci1<-(datsss-(1.96*datsss_error))
ci2<-(datsss+(1.96*datsss_error))
rM<-rowMeans(datsss)
RB <- rM - true
PB <- 100 * abs((rM - true)/ true)
CR <- rowMeans(ci1< true & true < ci2)
AW <- rowMeans(ci2 - ci1)
RMSE <- sqrt(rowMeans((datsss - true)^2))
ci1<-rowMeans(ci1)
ci2<-rowMeans(ci2)
huhu50_PMM_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2)

print(xtable(huhu50_PMM_1000r, digits=4,type = "latex"), file = "huhu50_PMM_1000r.tex")








### 0.75

fit_ampute_pmm75 <- with(PMM_ampute_75_1000r, geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
est_ampute_pmm75 <- pool(fit_ampute_pmm75) 
coef_ampute_pmm75<-summary(pool(fit_ampute_pmm75) , exp=TRUE, conf.int=TRUE)



### Evaluation PMM


fit<-fit_ampute_pmm75

coef<-14
m<-20

a <- summary(fit)$estimate
datsss<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss[i]<-a[seq(i, length(a), coef)]
}
datsss<-data.frame(t(datsss))

b <- summary(fit)$std.error
datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_error[i]<-b[seq(i, length(b), coef)]
}
datsss_error<-data.frame(t(datsss_error))

true <- fit_CC$coefficients
ci1<-(datsss-(1.96*datsss_error))
ci2<-(datsss+(1.96*datsss_error))
rM<-rowMeans(datsss)
RB <- rM - true
PB <- 100 * abs((rM - true)/ true)
CR <- rowMeans(ci1< true & true < ci2)
AW <- rowMeans(ci2 - ci1)
RMSE <- sqrt(rowMeans((datsss - true)^2))
ci1<-rowMeans(ci1)
ci2<-rowMeans(ci2)
huhu75_PMM_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2)

print(xtable(huhu75_PMM_1000r, digits=4,type = "latex"), file = "huhu75_PMM_1000r.tex")














### MICE



load("./gogn_og_imp/imp_ampute_MICE_05_1000r")
load("./gogn_og_imp/imp_ampute_MICE_10_1000r")
load("./gogn_og_imp/imp_ampute_MICE_25_1000r")
load("./gogn_og_imp/imp_ampute_MICE_50_1000r")
load("./gogn_og_imp/imp_ampute_MICE_75_1000r")



### 0.05

fit_ampute_MICE05 <- with(MICE_ampute_05_1000r, geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
est_ampute_MICE05 <- pool(fit_ampute_MICE05) 
coef_ampute_MICE05<-summary(pool(fit_ampute_MICE05), exp=TRUE, conf.int=TRUE)



### Evaluation MICE


fit<-fit_ampute_MICE05

coef<-14
m<-20

a <- summary(fit)$estimate
datsss<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss[i]<-a[seq(i, length(a), coef)]
}
datsss<-data.frame(t(datsss))

b <- summary(fit)$std.error
datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_error[i]<-b[seq(i, length(b), coef)]
}
datsss_error<-data.frame(t(datsss_error))

true <- fit_CC$coefficients
ci1<-(datsss-(1.96*datsss_error))
ci2<-(datsss+(1.96*datsss_error))
rM<-rowMeans(datsss)
RB <- rM - true
PB <- 100 * abs((rM - true)/ true)
CR <- rowMeans(ci1< true & true < ci2)
AW <- rowMeans(ci2 - ci1)
RMSE <- sqrt(rowMeans((datsss - true)^2))
ci1<-rowMeans(ci1)
ci2<-rowMeans(ci2)
huhu05_MICE_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2)

print(xtable(huhu05_MICE_1000r, digits=4,type = "latex"), file = "huhu05_MICE_1000r.tex")












### 0.1

fit_ampute_MICE10 <- with(MICE_ampute_10_1000r, geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
est_ampute_MICE10 <- pool(fit_ampute_MICE10) 
coef_ampute_MICE10<-summary(pool(fit_ampute_MICE10) , exp=TRUE, conf.int=TRUE)



### Evaluation MICE


fit<-fit_ampute_MICE10

coef<-14
m<-20

a <- summary(fit)$estimate
datsss<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss[i]<-a[seq(i, length(a), coef)]
}
datsss<-data.frame(t(datsss))

b <- summary(fit)$std.error
datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_error[i]<-b[seq(i, length(b), coef)]
}
datsss_error<-data.frame(t(datsss_error))

true <- fit_CC$coefficients
ci1<-(datsss-(1.96*datsss_error))
ci2<-(datsss+(1.96*datsss_error))
rM<-rowMeans(datsss)
RB <- rM - true
PB <- 100 * abs((rM - true)/ true)
CR <- rowMeans(ci1< true & true < ci2)
AW <- rowMeans(ci2 - ci1)
RMSE <- sqrt(rowMeans((datsss - true)^2))
ci1<-rowMeans(ci1)
ci2<-rowMeans(ci2)
huhu10_MICE_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2)

print(xtable(huhu10_MICE_1000r, digits=4,type = "latex"), file = "huhu10_MICE_1000r.tex")









### 0.25

fit_ampute_MICE25 <- with(MICE_ampute_25_1000r, geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
est_ampute_MICE25 <- pool(fit_ampute_MICE25) 
coef_ampute_MICE25<-summary(pool(fit_ampute_MICE25) , exp=TRUE, conf.int=TRUE)



### Evaluation MICE


fit<-fit_ampute_MICE25

coef<-14
m<-20

a <- summary(fit)$estimate
datsss<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss[i]<-a[seq(i, length(a), coef)]
}
datsss<-data.frame(t(datsss))

b <- summary(fit)$std.error
datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_error[i]<-b[seq(i, length(b), coef)]
}
datsss_error<-data.frame(t(datsss_error))

true <- fit_CC$coefficients
ci1<-(datsss-(1.96*datsss_error))
ci2<-(datsss+(1.96*datsss_error))
rM<-rowMeans(datsss)
RB <- rM - true
PB <- 100 * abs((rM - true)/ true)
CR <- rowMeans(ci1< true & true < ci2)
AW <- rowMeans(ci2 - ci1)
RMSE <- sqrt(rowMeans((datsss - true)^2))
ci1<-rowMeans(ci1)
ci2<-rowMeans(ci2)
huhu25_MICE_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2)

print(xtable(huhu25_MICE_1000r, digits=4,type = "latex"), file = "huhu25_MICE_1000r.tex")









### 0.5

fit_ampute_MICE50 <- with(MICE_ampute_50_1000r, geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
est_ampute_MICE50 <- pool(fit_ampute_MICE50) 
coef_ampute_MICE50<-summary(pool(fit_ampute_MICE50) , exp=TRUE, conf.int=TRUE)



### Evaluation MICE


fit<-fit_ampute_MICE50

coef<-14
m<-20

a <- summary(fit)$estimate
datsss<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss[i]<-a[seq(i, length(a), coef)]
}
datsss<-data.frame(t(datsss))

b <- summary(fit)$std.error
datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_error[i]<-b[seq(i, length(b), coef)]
}
datsss_error<-data.frame(t(datsss_error))

true <- fit_CC$coefficients
ci1<-(datsss-(1.96*datsss_error))
ci2<-(datsss+(1.96*datsss_error))
rM<-rowMeans(datsss)
RB <- rM - true
PB <- 100 * abs((rM - true)/ true)
CR <- rowMeans(ci1< true & true < ci2)
AW <- rowMeans(ci2 - ci1)
RMSE <- sqrt(rowMeans((datsss - true)^2))
ci1<-rowMeans(ci1)
ci2<-rowMeans(ci2)
huhu50_MICE_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2)

print(xtable(huhu50_MICE_1000r, digits=4,type = "latex"), file = "huhu50_MICE_1000r.tex")








### 0.75

fit_ampute_MICE75 <- with(MICE_ampute_75_1000r, geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
est_ampute_MICE75 <- pool(fit_ampute_MICE75) 
coef_ampute_MICE75<-summary(pool(fit_ampute_MICE75) , exp=TRUE, conf.int=TRUE)



### Evaluation MICE


fit<-fit_ampute_MICE75

coef<-14
m<-20

a <- summary(fit)$estimate
datsss<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss[i]<-a[seq(i, length(a), coef)]
}
datsss<-data.frame(t(datsss))

b <- summary(fit)$std.error
datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_error[i]<-b[seq(i, length(b), coef)]
}
datsss_error<-data.frame(t(datsss_error))

true <- fit_CC$coefficients
ci1<-(datsss-(1.96*datsss_error))
ci2<-(datsss+(1.96*datsss_error))
rM<-rowMeans(datsss)
RB <- rM - true
PB <- 100 * abs((rM - true)/ true)
CR <- rowMeans(ci1< true & true < ci2)
AW <- rowMeans(ci2 - ci1)
RMSE <- sqrt(rowMeans((datsss - true)^2))
ci1<-rowMeans(ci1)
ci2<-rowMeans(ci2)
huhu75_MICE_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2)

print(xtable(huhu75_MICE_1000r, digits=4,type = "latex"), file = "huhu75_MICE_1000r.tex")












## SI






load("./gogn_og_imp/imp_ampute_SI_05_1000r")
load("./gogn_og_imp/imp_ampute_SI_10_1000r")
load("./gogn_og_imp/imp_ampute_SI_25_1000r")
load("./gogn_og_imp/imp_ampute_SI_50_1000r")
load("./gogn_og_imp/imp_ampute_SI_75_1000r")



### 0.05

fit_ampute_SI05 <- with(SI_ampute_05_1000r, geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
coef_ampute_SI05<-summary((fit_ampute_SI05) , exp=TRUE, conf.int=TRUE)



### Evaluation SI


fit<-fit_ampute_SI05

coef<-14
m<-1

a <- summary(fit)$estimate
datsss<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss[i]<-a[seq(i, length(a), coef)]
}
datsss<-data.frame(t(datsss))

b <- summary(fit)$std.error
datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_error[i]<-b[seq(i, length(b), coef)]
}
datsss_error<-data.frame(t(datsss_error))

true <- fit_CC$coefficients
ci1<-(datsss-(1.96*datsss_error))
ci2<-(datsss+(1.96*datsss_error))
rM<-rowMeans(datsss)
RB <- rM - true
PB <- 100 * abs((rM - true)/ true)
CR <- rowMeans(ci1< true & true < ci2)
AW <- rowMeans(ci2 - ci1)
RMSE <- sqrt(rowMeans((datsss - true)^2))
ci1<-rowMeans(ci1)
ci2<-rowMeans(ci2)
huhu05_SI_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2)

print(xtable(huhu05_SI_1000r, digits=4,type = "latex"), file = "huhu05_SI_1000r.tex")












### 0.1

fit_ampute_SI10 <- with(SI_ampute_10_1000r, geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
coef_ampute_SI10<-summary((fit_ampute_SI10) , exp=TRUE, conf.int=TRUE)



### Evaluation SI


fit<-fit_ampute_SI10

coef<-14
m<-1

a <- summary(fit)$estimate
datsss<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss[i]<-a[seq(i, length(a), coef)]
}
datsss<-data.frame(t(datsss))

b <- summary(fit)$std.error
datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_error[i]<-b[seq(i, length(b), coef)]
}
datsss_error<-data.frame(t(datsss_error))

true <- fit_CC$coefficients
ci1<-(datsss-(1.96*datsss_error))
ci2<-(datsss+(1.96*datsss_error))
rM<-rowMeans(datsss)
RB <- rM - true
PB <- 100 * abs((rM - true)/ true)
CR <- rowMeans(ci1< true & true < ci2)
AW <- rowMeans(ci2 - ci1)
RMSE <- sqrt(rowMeans((datsss - true)^2))
ci1<-rowMeans(ci1)
ci2<-rowMeans(ci2)
huhu10_SI_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2)

print(xtable(huhu10_SI_1000r, digits=4,type = "latex"), file = "huhu10_SI_1000r.tex")









### 0.25

fit_ampute_SI25 <- with(SI_ampute_25_1000r, geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
coef_ampute_SI25<-summary((fit_ampute_SI25) , exp=TRUE, conf.int=TRUE)



### Evaluation SI


fit<-fit_ampute_SI25

coef<-14
m<-1

a <- summary(fit)$estimate
datsss<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss[i]<-a[seq(i, length(a), coef)]
}
datsss<-data.frame(t(datsss))

b <- summary(fit)$std.error
datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_error[i]<-b[seq(i, length(b), coef)]
}
datsss_error<-data.frame(t(datsss_error))

true <- fit_CC$coefficients
ci1<-(datsss-(1.96*datsss_error))
ci2<-(datsss+(1.96*datsss_error))
rM<-rowMeans(datsss)
RB <- rM - true
PB <- 100 * abs((rM - true)/ true)
CR <- rowMeans(ci1< true & true < ci2)
AW <- rowMeans(ci2 - ci1)
RMSE <- sqrt(rowMeans((datsss - true)^2))
ci1<-rowMeans(ci1)
ci2<-rowMeans(ci2)
huhu25_SI_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2)

print(xtable(huhu25_SI_1000r, digits=4,type = "latex"), file = "huhu25_SI_1000r.tex")









### 0.5

fit_ampute_SI50 <- with(SI_ampute_50_1000r, geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
coef_ampute_SI50<-summary((fit_ampute_SI50) , exp=TRUE, conf.int=TRUE)



### Evaluation SI


fit<-fit_ampute_SI50

coef<-14
m<-1

a <- summary(fit)$estimate
datsss<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss[i]<-a[seq(i, length(a), coef)]
}
datsss<-data.frame(t(datsss))

b <- summary(fit)$std.error
datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_error[i]<-b[seq(i, length(b), coef)]
}
datsss_error<-data.frame(t(datsss_error))

true <- fit_CC$coefficients
ci1<-(datsss-(1.96*datsss_error))
ci2<-(datsss+(1.96*datsss_error))
rM<-rowMeans(datsss)
RB <- rM - true
PB <- 100 * abs((rM - true)/ true)
CR <- rowMeans(ci1< true & true < ci2)
AW <- rowMeans(ci2 - ci1)
RMSE <- sqrt(rowMeans((datsss - true)^2))
ci1<-rowMeans(ci1)
ci2<-rowMeans(ci2)
huhu50_SI_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2)

print(xtable(huhu50_SI_1000r, digits=4,type = "latex"), file = "huhu50_SI_1000r.tex")








### 0.75

fit_ampute_SI75 <- with(SI_ampute_75_1000r, geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
coef_ampute_SI75<-summary((fit_ampute_SI75) , exp=TRUE, conf.int=TRUE)



### Evaluation SI


fit<-fit_ampute_SI75

coef<-14
m<-1

a <- summary(fit)$estimate
datsss<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss[i]<-a[seq(i, length(a), coef)]
}
datsss<-data.frame(t(datsss))

b <- summary(fit)$std.error
datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_error[i]<-b[seq(i, length(b), coef)]
}
datsss_error<-data.frame(t(datsss_error))

true <- fit_CC$coefficients
ci1<-(datsss-(1.96*datsss_error))
ci2<-(datsss+(1.96*datsss_error))
rM<-rowMeans(datsss)
RB <- rM - true
PB <- 100 * abs((rM - true)/ true)
CR <- rowMeans(ci1< true & true < ci2)
AW <- rowMeans(ci2 - ci1)
RMSE <- sqrt(rowMeans((datsss - true)^2))
ci1<-rowMeans(ci1)
ci2<-rowMeans(ci2)
huhu75_SI_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2)

print(xtable(huhu75_SI_1000r, digits=4,type = "latex"), file = "huhu75_SI_1000r.tex")

















## CCA

# 0.05

gogn_fImpute_DNA<-na.omit(gogn_fImpute_1000r_0.05)
fitsDNA05<-geeglm(binary_trait==1 ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), data=gogn_fImpute_DNA,family=poisson, id=as.factor(pin))

CC05_1000r<-exp(fitsDNA05$coefficients)
CC05_1000r_tidy<-tidy(fitsDNA05, exp=TRUE, conf.int=TRUE)

fit<-fitsDNA05
true <- (fit_CC$coefficients)
ci1<-fit$coefficients-(1.96*summary(fit)$coefficients[,2])
ci2<-fit$coefficients+(1.96*summary(fit)$coefficients[,2])
RB <- fit$coefficients - true
PB <- 100 * abs((fit$coefficients - true)/ true)
CR <- ci1 < true & true < ci2
AW <- ci2 - ci1
RMSE <- sqrt(((fit$coefficients - true)^2))
huhu05_CC_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1, true, ci2)

print(xtable(huhu05_CC_1000r, digits=4,type = "latex"), file = "huhu05_CC_1000r.tex")



# 0.1

gogn_fImpute_DNA<-na.omit(gogn_fImpute_1000r_0.1)

fitsDNA10<-geeglm(binary_trait==1 ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), data=gogn_fImpute_DNA,family=poisson, id=as.factor(pin))

CC10_1000r<-exp(fitsDNA10$coefficients)
CC10_1000r_tidy<-tidy(fitsDNA10, exp=TRUE, conf.int=TRUE)

fit<-fitsDNA10
true <- (fit_CC$coefficients)
ci1<-fit$coefficients-(1.96*summary(fit)$coefficients[,2])
ci2<-fit$coefficients+(1.96*summary(fit)$coefficients[,2])
RB <- fit$coefficients - true
PB <- 100 * abs((fit$coefficients - true)/ true)
CR <- ci1 < true & true < ci2
AW <- ci2 - ci1
RMSE <- sqrt(((fit$coefficients - true)^2))
huhu10_CC_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1, true, ci2)

print(xtable(huhu10_CC_1000r, digits=4,type = "latex"), file = "huhu10_CC_1000r.tex")





# 0.25

gogn_fImpute_DNA<-na.omit(gogn_fImpute_1000r_0.25)
fitsDNA25<-geeglm(binary_trait==1 ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), data=gogn_fImpute_DNA,family=poisson, id=as.factor(pin))

CC25_1000r<-exp(fitsDNA05$coefficients)
CC25_1000r_tidy<-tidy(fitsDNA25, exp=TRUE, conf.int=TRUE)

fit<-fitsDNA25
true <- (fit_CC$coefficients)
ci1<-fit$coefficients-(1.96*summary(fit)$coefficients[,2])
ci2<-fit$coefficients+(1.96*summary(fit)$coefficients[,2])
RB <- fit$coefficients - true
PB <- 100 * abs((fit$coefficients - true)/ true)
CR <- ci1 < true & true < ci2
AW <- ci2 - ci1
RMSE <- sqrt(((fit$coefficients - true)^2))
huhu25_CC_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1, true, ci2)

print(xtable(huhu25_CC_1000r, digits=4,type = "latex"), file = "huhu25_CC_1000r.tex")






# 0.50

gogn_fImpute_DNA<-na.omit(gogn_fImpute_1000r_0.5)
fitsDNA50<-geeglm(binary_trait==1 ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), data=gogn_fImpute_DNA,family=poisson, id=as.factor(pin))

CC50_1000r<-exp(fitsDNA50$coefficients)
CC50_1000r_tidy<-tidy(fitsDNA50, exp=TRUE, conf.int=TRUE)

fit<-fitsDNA50
true <- (fit_CC$coefficients)
ci1<-fit$coefficients-(1.96*summary(fit)$coefficients[,2])
ci2<-fit$coefficients+(1.96*summary(fit)$coefficients[,2])
RB <- fit$coefficients - true
PB <- 100 * abs((fit$coefficients - true)/ true)
CR <- ci1 < true & true < ci2
AW <- ci2 - ci1
RMSE <- sqrt(((fit$coefficients - true)^2))
huhu50_CC_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1, true, ci2)

print(xtable(huhu50_CC_1000r, digits=4,type = "latex"), file = "huhu50_CC_1000r.tex")








# 0.75

gogn_fImpute_DNA<-na.omit(gogn_fImpute_1000r_0.75)
fitsDNA75<-geeglm(binary_trait==1 ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), data=gogn_fImpute_DNA,family=poisson, id=as.factor(pin))

CC75_1000r<-exp(fitsDNA75$coefficients)
CC75_1000r_tidy<-tidy(fitsDNA75, exp=TRUE, conf.int=TRUE)

fit<-fitsDNA75
true <- (fit_CC$coefficients)
ci1<-fit$coefficients-(1.96*summary(fit)$coefficients[,2])
ci2<-fit$coefficients+(1.96*summary(fit)$coefficients[,2])
RB <- fit$coefficients - true
PB <- 100 * abs((fit$coefficients - true)/ true)
CR <- ci1 < true & true < ci2
AW <- ci2 - ci1
RMSE <- sqrt(((fit$coefficients - true)^2))
huhu75_CC_1000r<-data.frame(RB, PB, CR, AW, RMSE, ci1, true, ci2)

print(xtable(huhu75_CC_1000r, digits=4,type = "latex"), file = "huhu75_CC_1000r.tex")






## To use for plots

## ORs

beraSamanRegCoef_PMM_1000r<-data.frame(coef_ampute_pmm05[2], coef_ampute_pmm10[2], coef_ampute_pmm25[2], coef_ampute_pmm50[2], coef_ampute_pmm75[2])
save(beraSamanRegCoef_PMM_1000r, file="beraSamanRegCoef_PMM_1000r")


beraSamanRegCoef_MICE_1000r<-data.frame(coef_ampute_MICE05[2], coef_ampute_MICE10[2], coef_ampute_MICE25[2], coef_ampute_MICE50[2], coef_ampute_MICE75[2])
save(beraSamanRegCoef_MICE_1000r, file="beraSamanRegCoef_MICE_1000r")


beraSamanRegCoef_SI_1000r<-data.frame(coef_ampute_SI05[2], coef_ampute_SI10[2], coef_ampute_SI25[2], coef_ampute_SI50[2], coef_ampute_SI75[2])
save(beraSamanRegCoef_SI_1000r, file="beraSamanRegCoefSI_1000r")


beraSamanRegCoef_CC_1000r<-data.frame(CC05_1000r, CC10_1000r, CC25_1000r, CC50_1000r, CC75_1000r)
save(beraSamanRegCoef_CC_1000r, file="beraSamanRegCoef_CC_1000r")



## SE

beraSamanSE_PMM_1000r<-data.frame(coef_ampute_pmm05[3], coef_ampute_pmm10[3], coef_ampute_pmm25[3], coef_ampute_pmm50[3], coef_ampute_pmm75[3])
save(beraSamanSE_PMM_1000r, file="beraSamanSE_PMM_1000r")


beraSamanSE_MICE_1000r<-data.frame(coef_ampute_MICE05[3], coef_ampute_MICE10[3], coef_ampute_MICE25[3], coef_ampute_MICE50[3], coef_ampute_MICE75[3])
save(beraSamanSE_MICE_1000r, file="beraSamanSE_MICE_1000r")


beraSamanSE_SI_1000r<-data.frame(coef_ampute_SI05[3], coef_ampute_SI10[3], coef_ampute_SI25[3], coef_ampute_SI50[3], coef_ampute_SI75[3])
save(beraSamanSE_SI_1000r, file="beraSamanSESI_1000r")


beraSamanSE_CC_1000r<-data.frame(CC05_1000r_tidy$std.error, CC10_1000r_tidy$std.error, CC25_1000r_tidy$std.error, CC50_1000r_tidy$std.error, CC75_1000r_tidy$std.error)
save(beraSamanSE_CC_1000r, file="beraSamanSE_CC_1000r")



## P_val

beraSamanP_val_PMM_1000r<-data.frame(coef_ampute_pmm05[6], coef_ampute_pmm10[6], coef_ampute_pmm25[6], coef_ampute_pmm50[6], coef_ampute_pmm75[6])
save(beraSamanP_val_PMM_1000r, file="beraSamanP_val_PMM_1000r")


beraSamanP_val_MICE_1000r<-data.frame(coef_ampute_MICE05[6], coef_ampute_MICE10[6], coef_ampute_MICE25[6], coef_ampute_MICE50[6], coef_ampute_MICE75[6])
save(beraSamanP_val_MICE_1000r, file="beraSamanP_val_MICE_1000r")


beraSamanP_val_SI_1000r<-data.frame(coef_ampute_SI05[5], coef_ampute_SI10[5], coef_ampute_SI25[5], coef_ampute_SI50[5], coef_ampute_SI75[5])
save(beraSamanP_val_SI_1000r, file="beraSamanP_valSI_1000r")


beraSamanP_val_CC_1000r<-data.frame(CC05_1000r_tidy$p.value, CC10_1000r_tidy$p.value, CC25_1000r_tidy$p.value, CC50_1000r_tidy$p.value, CC75_1000r_tidy$p.value)
save(beraSamanP_val_CC_1000r, file="beraSamanP_val_CC_1000r")



