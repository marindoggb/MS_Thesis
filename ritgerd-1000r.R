
library(mice)
library(geepack)
library(dplyr)
#Print for use in Latex
library(foreign)
library(xtable)
library(stargazer)

### Here the 1000 row sample is shown - for a different sample size it is only needed to Ctrl - F, and replace 1000r with 5000r, etc.

### PMM


load("./gogn_og_imp/gognDNA1000r")
load("./gogn_og_imp/imp_ampute_PMM_05_1000r")
load("./gogn_og_imp/imp_ampute_PMM_10_1000r")
load("./gogn_og_imp/imp_ampute_PMM_25_1000r")
load("./gogn_og_imp/imp_ampute_PMM_50_1000r")
load("./gogn_og_imp/imp_ampute_PMM_75_1000r")

# The correct results before missingness
fit_rett<-geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+
                   factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=pin, data=gognDNA1000r_rett)

tidy(fit_rett, exp=TRUE, conf.int=TRUE)


# The imputation and the fits for each level of missingness
gogn_listi<-list(PMM_ampute_05_1000r,PMM_ampute_10_1000r, PMM_ampute_25_1000r, PMM_ampute_50_1000r, PMM_ampute_75_1000r)

for (i in 1:length(gogn_listi)) {
  fit<-with(gogn_listi[[i]],geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+
                          factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=pin))
  summary1<-summary(pool(fit))
  summar<-summary(pool(fit), exp=TRUE, conf.int=TRUE)
  assign(paste0("coef_ampute_PMM", i),summar)

  #exp(summary1[,1])
  coef<-14 #number of coefficients in model
  m<-20 #number of imputations
  a <- summary1[,2] # Create a dataframe with all estimates from each fit to use for calculating coverage and PB and more
  datsss<-data.frame(matrix(ncol = coef, nrow = m))
  for (j in 1:coef) {
    datsss[j]<-a[seq(j, length(a), coef)]
  }
  datsss<-data.frame(t(datsss))
  b <- summary1[,3] # same thing as above except for confidence intervals
  datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
  for (j in 1:coef) {
    datsss_error[j]<-b[seq(j, length(b), coef)]
  }
  datsss_error<-data.frame(t(datsss_error))
  R2gildi_PMM_ItT<-c()
  for (j in 1:m) {
    dat<-complete(gogn_listi[[i]], j)
    glm1<-geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+
                   factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin), data=dat)
    Y_bar = mean(glm1$y, na.rm = T)
    R2gildi_PMM_ItT[j]<- 1-(sum(glm1$weights * (glm1$y - glm1$fitted.values)^2, na.rm = T)/sum(glm1$weights*(glm1$y - Y_bar)^2, na.rm = T))
  }
  mR2gildi_PMM_1000r<-mean(R2gildi_PMM_ItT)
  nam <- paste("mR2gildi_PMM_1000r", i, sep = "_")
  assign(paste0("mR2gildi_PMM_1000r_", i),mR2gildi_PMM_1000r) %>% saveRDS(,file=nam)
  true <- fit_rett$coefficients # mark the correct estimate from the data before missingness was created
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
  imp_est<-summary1$estimate
  true_exp<- exp(true)
  imp_exp<-exp(imp_est)
  PB_exp<-100 * abs((exp(rM) - true_exp)/ true_exp)
  huhu_PMM_1000r_<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2,imp_est, true_exp, imp_exp, PB_exp) # create data frame to print
  rownames(huhu_PMM_1000r_)<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+", "age", "deprivation_Yes", "menntun_Sec", "menntun_TA", "menntun_TB",
                                               "tekjur_301-500", "tekjur_501-700", "tekjur_701+", "fjolSamband_SingleWidow")
  nam <- paste("huhu_PMM_1000r", i, sep = "_")
  assign(paste0("huhu_PMM_1000r_", i), huhu_PMM_1000r_) %>% saveRDS(,file=nam)
}


##### MICE


load("./gogn_og_imp/imp_ampute_MICE_05_1000r")
load("./gogn_og_imp/imp_ampute_MICE_10_1000r")
load("./gogn_og_imp/imp_ampute_MICE_25_1000r")
load("./gogn_og_imp/imp_ampute_MICE_50_1000r")
load("./gogn_og_imp/imp_ampute_MICE_75_1000r")


# The imputation and the fits for each level of missingness
gogn_listi<-list(MICE_ampute_05_1000r,MICE_ampute_10_1000r, MICE_ampute_25_1000r, MICE_ampute_50_1000r, MICE_ampute_75_1000r)



for (i in 1:length(gogn_listi)) {
  fit<-with(gogn_listi[[i]],geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+
                          factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=pin))
  summary1<-summary(pool(fit))
  summar<-summary(pool(fit), exp=TRUE, conf.int=TRUE)
  assign(paste0("coef_ampute_MICE", i),summar)
  #exp(summary1[,1])
  coef<-14 #number of coefficients in model
  m<-20 #number of imputations
  a <- summary1[,2] # Create a dataframe with all estimates from each fit to use for calculating coverage and PB and more
  datsss<-data.frame(matrix(ncol = coef, nrow = m))
  for (j in 1:coef) {
    datsss[j]<-a[seq(j, length(a), coef)]
  }
  datsss<-data.frame(t(datsss))
  b <- summary1[,3] # same thing as above except for confidence intervals
  datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
  for (j in 1:coef) {
    datsss_error[j]<-b[seq(j, length(b), coef)]
  }
  datsss_error<-data.frame(t(datsss_error))
  R2gildi_MICE_ItT<-c()
  for (j in 1:m) {
    dat<-complete(gogn_listi[[i]], j)
    glm1<-geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+
                   factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin), data=dat)
    Y_bar = mean(glm1$y, na.rm = T)
    R2gildi_MICE_ItT[j]<- 1-(sum(glm1$weights * (glm1$y - glm1$fitted.values)^2, na.rm = T)/sum(glm1$weights*(glm1$y - Y_bar)^2, na.rm = T))
  }
  mR2gildi_MICE_1000r<-mean(R2gildi_MICE_ItT)
  nam <- paste("mR2gildi_MICE_1000r", i, sep = "_")
  assign(paste0("mR2gildi_MICE_1000r_", i),mR2gildi_MICE_1000r) %>% saveRDS(,file=nam)
  true <- fit_rett$coefficients # mark the correct estimate from the data before missingness was created
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
  imp_est<-summary1$estimate
  true_exp<- exp(true)
  imp_exp<-exp(imp_est)
  PB_exp<-100 * abs((exp(rM) - true_exp)/ true_exp)
  huhu_MICE_1000r_<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2,imp_est, true_exp, imp_exp, PB_exp) # create data frame to print
  rownames(huhu_MICE_1000r_)<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+", "age", "deprivation_Yes", "menntun_Sec", "menntun_TA", "menntun_TB",
                               "tekjur_301-500", "tekjur_501-700", "tekjur_701+", "fjolSamband_SingleWidow")
  nam <- paste("huhu_MICE_1000r", i, sep = "_")
  assign(paste0("huhu_MICE_1000r_", i), huhu_MICE_1000r_) %>% saveRDS(,file=nam)
}





#### SI-PMM


load("./gogn_og_imp/imp_ampute_SI_05_1000r")
load("./gogn_og_imp/imp_ampute_SI_10_1000r")
load("./gogn_og_imp/imp_ampute_SI_25_1000r")
load("./gogn_og_imp/imp_ampute_SI_50_1000r")
load("./gogn_og_imp/imp_ampute_SI_75_1000r")


# The imputation and the fits for each level of missingness
gogn_listi<-list(SI_ampute_05_1000r,SI_ampute_10_1000r, SI_ampute_25_1000r, SI_ampute_50_1000r, SI_ampute_75_1000r)



for (i in 1:length(gogn_listi)) {
  fit<-with(gogn_listi[[i]],geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+
                                     factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=pin))
  summary1<-summary((fit))
  summar<-summary(fit, exp=TRUE, conf.int=TRUE)
  assign(paste0("coef_ampute_SI", i),summar)
  #exp(summary1[,1])
  coef<-14 #number of coefficients in model
  m<-1 #number of imputations

  a <- summary(fit)$estimate # Create a dataframe with all estimates from each fit to use for calculating coverage and PB and more
  datsss<-data.frame(matrix(ncol = coef, nrow = m))
  for (j in 1:coef) {
    datsss[j]<-a[seq(j, length(a), coef)]
  }
  datsss<-data.frame(t(datsss))
  b <- summary(fit)$std.error # same thing as above except for confidence intervals
  datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
  for (j in 1:coef) {
    datsss_error[j]<-b[seq(j, length(b), coef)]
  }
  datsss_error<-data.frame(t(datsss_error))
  R2gildi_SI_ItT<-c()
  for (j in 1:m) {
    dat<-complete(gogn_listi[[i]], j)
    glm1<-geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+
                   factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin), data=dat)
    Y_bar = mean(glm1$y, na.rm = T)
    R2gildi_SI_ItT[j]<- 1-(sum(glm1$weights * (glm1$y - glm1$fitted.values)^2, na.rm = T)/sum(glm1$weights*(glm1$y - Y_bar)^2, na.rm = T))
  }
  mR2gildi_SI_1000r<-mean(R2gildi_SI_ItT)
  nam <- paste("mR2gildi_SI_1000r", i, sep = "_")
  assign(paste0("mR2gildi_SI_1000r_", i),mR2gildi_SI_1000r) %>% saveRDS(,file=nam)
  true <- fit_rett$coefficients # mark the correct estimate from the data before missingness was created
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
  imp_est<-summary1$estimate
  true_exp<- exp(true)
  imp_exp<-exp(imp_est)
  PB_exp<-100 * abs((exp(rM) - true_exp)/ true_exp)
  huhu_SI_1000r_<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2,imp_est, true_exp, imp_exp, PB_exp) # create data frame to print
  rownames(huhu_SI_1000r_)<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+", "age", "deprivation_Yes", "menntun_Sec", "menntun_TA", "menntun_TB",
                                "tekjur_301-500", "tekjur_501-700", "tekjur_701+", "fjolSamband_SingleWidow")
  nam <- paste("huhu_SI_1000r", i, sep = "_")
  assign(paste0("huhu_SI_1000r_", i), huhu_SI_1000r_) %>% saveRDS(,file=nam)
}




## CC



gogn_fImpute_1000r_0.05<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.05")
gogn_fImpute_1000r_0.05<- gogn_fImpute_1000r_0.05 %>% select(pin, binary_trait, ACE_f_total_cat, agecalculated, deprivation, menntun2, tekjurEigin, fjolSamband2) %>% na.omit(.)
gogn_fImpute_1000r_0.1<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.1")
gogn_fImpute_1000r_0.1<- gogn_fImpute_1000r_0.1 %>% select(pin, binary_trait, ACE_f_total_cat, agecalculated, deprivation, menntun2, tekjurEigin, fjolSamband2) %>% na.omit(.)
gogn_fImpute_1000r_0.25<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.25")
gogn_fImpute_1000r_0.25<- gogn_fImpute_1000r_0.25 %>% select(pin, binary_trait, ACE_f_total_cat, agecalculated, deprivation, menntun2, tekjurEigin, fjolSamband2) %>% na.omit(.)
gogn_fImpute_1000r_0.5<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.5")
gogn_fImpute_1000r_0.5<- gogn_fImpute_1000r_0.5 %>% select(pin, binary_trait, ACE_f_total_cat, agecalculated, deprivation, menntun2, tekjurEigin, fjolSamband2) %>% na.omit(.)
gogn_fImpute_1000r_0.75<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.75")
gogn_fImpute_1000r_0.75<- gogn_fImpute_1000r_0.75 %>% select(pin, binary_trait, ACE_f_total_cat, agecalculated, deprivation, menntun2, tekjurEigin, fjolSamband2) %>% na.omit(.)


# The imputation and the fits for each level of misCCngness
gogn_listi<-list(gogn_fImpute_1000r_0.05, gogn_fImpute_1000r_0.1, gogn_fImpute_1000r_0.25, gogn_fImpute_1000r_0.5, gogn_fImpute_1000r_0.75)



for (i in 1:length(gogn_listi)) {
  fit<-geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+
                                     factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=pin,data = gogn_listi[[i]])
  summary1<-summary((fit))
  summar<-tidy(fit, exp=TRUE, conf.int=TRUE)
  assign(paste0("coef_ampute_CC", i),summar)
  #exp(summary1[,1])
  coef<-14 #number of coefficients in model
  m<-1 #number of imputations
  a <- summary1$coefficients$Estimate# Create a dataframe with all estimates from each fit to use for calculating coverage and PB and more
  datsss<-data.frame(matrix(ncol = coef, nrow = m))
  for (j in 1:coef) {
    datsss[j]<-a[seq(j, length(a), coef)]
  }
  datsss<-data.frame(t(datsss))
  b <- summary1$coefficients$Std.err # same thing as above except for confidence intervals
  datsss_error<-data.frame(matrix(ncol = coef, nrow = m))
  for (j in 1:coef) {
    datsss_error[j]<-b[seq(j, length(b), coef)]
  }
  datsss_error<-data.frame(t(datsss_error))
  R2gildi_CC_ItT<-c()
  for (j in 1:m) {
    dat<-gogn_listi[[i]]
    glm1<-geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+
                   factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin), data=dat)
    Y_bar = mean(glm1$y, na.rm = T)
    R2gildi_CC_ItT[j]<- 1-(sum(glm1$weights * (glm1$y - glm1$fitted.values)^2, na.rm = T)/sum(glm1$weights*(glm1$y - Y_bar)^2, na.rm = T))
  }
  mR2gildi_CC_1000r<-mean(R2gildi_CC_ItT)
  nam <- paste("mR2gildi_CC_1000r", i, sep = "_")
  assign(paste0("mR2gildi_CC_1000r_", i),mR2gildi_CC_1000r) %>% saveRDS(,file=nam)
  true <- fit_rett$coefficients # mark the correct estimate from the data before missingness was created
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
  imp_est<-summary1$coefficients$Estimate
  true_exp<- exp(true)
  imp_exp<-exp(imp_est)
  PB_exp<-100 * abs((exp(rM) - true_exp)/ true_exp)
  huhu_CC_1000r_<-data.frame(RB, PB, CR, AW, RMSE, ci1,true ,ci2,imp_est, true_exp, imp_exp, PB_exp) # create data frame to print
  rownames(huhu_CC_1000r_)<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+", "age", "deprivation_Yes", "menntun_Sec", "menntun_TA", "menntun_TB",
                              "tekjur_301-500", "tekjur_501-700", "tekjur_701+", "fjolSamband_SingleWidow")
  nam <- paste("huhu_CC_1000r", i, sep = "_")
  assign(paste0("huhu_CC_1000r_", i), huhu_CC_1000r_) %>% saveRDS(,file=nam)
}





## To use for plots

## RRs

beraSamanRegCoef_PMM_1000r<-data.frame(coef_ampute_PMM1[2], coef_ampute_PMM2[2], coef_ampute_PMM3[2], coef_ampute_PMM4[2], coef_ampute_PMM5[2])
save(beraSamanRegCoef_PMM_1000r, file="beraSamanRegCoef_PMM_1000r")


beraSamanRegCoef_MICE_1000r<-data.frame(coef_ampute_MICE1[2], coef_ampute_MICE2[2], coef_ampute_MICE3[2], coef_ampute_MICE4[2], coef_ampute_MICE5[2])
save(beraSamanRegCoef_MICE_1000r, file="beraSamanRegCoef_MICE_1000r")


beraSamanRegCoef_SI_1000r<-data.frame(coef_ampute_SI1[2], coef_ampute_SI2[2], coef_ampute_SI3[2], coef_ampute_SI4[2], coef_ampute_SI5[2])
save(beraSamanRegCoef_SI_1000r, file="beraSamanRegCoefSI_1000r")


beraSamanRegCoef_CC_1000r<-data.frame(coef_ampute_CC1[2], coef_ampute_CC2[2], coef_ampute_CC3[2], coef_ampute_CC4[2], coef_ampute_CC5[2])
save(beraSamanRegCoef_CC_1000r, file="beraSamanRegCoef_CC_1000r")



## SE

beraSamanSE_PMM_1000r<-data.frame(coef_ampute_PMM1[3], coef_ampute_PMM2[3], coef_ampute_PMM3[3], coef_ampute_PMM4[3], coef_ampute_PMM5[3])
save(beraSamanSE_PMM_1000r, file="beraSamanSE_PMM_1000r")


beraSamanSE_MICE_1000r<-data.frame(coef_ampute_MICE1[3], coef_ampute_MICE2[3], coef_ampute_MICE3[3], coef_ampute_MICE4[3], coef_ampute_MICE5[3])
save(beraSamanSE_MICE_1000r, file="beraSamanSE_MICE_1000r")


beraSamanSE_SI_1000r<-data.frame(coef_ampute_SI1[3], coef_ampute_SI2[3], coef_ampute_SI3[3], coef_ampute_SI4[3], coef_ampute_SI5[3])
save(beraSamanSE_SI_1000r, file="beraSamanSESI_1000r")


beraSamanSE_CC_1000r<-data.frame(coef_ampute_CC1$std.error, coef_ampute_CC2$std.error, coef_ampute_CC3$std.error, coef_ampute_CC4$std.error, coef_ampute_CC5$std.error)
save(beraSamanSE_CC_1000r, file="beraSamanSE_CC_1000r")



## P_val

beraSamanP_val_PMM_1000r<-data.frame(coef_ampute_PMM1[6], coef_ampute_PMM2[6], coef_ampute_PMM3[6], coef_ampute_PMM4[6], coef_ampute_PMM5[6])
save(beraSamanP_val_PMM_1000r, file="beraSamanP_val_PMM_1000r")


beraSamanP_val_MICE_1000r<-data.frame(coef_ampute_MICE1[6], coef_ampute_MICE2[6], coef_ampute_MICE3[6], coef_ampute_MICE4[6], coef_ampute_MICE5[6])
save(beraSamanP_val_MICE_1000r, file="beraSamanP_val_MICE_1000r")


beraSamanP_val_SI_1000r<-data.frame(coef_ampute_SI1[5], coef_ampute_SI2[5], coef_ampute_SI3[5], coef_ampute_SI4[5], coef_ampute_SI5[5])
save(beraSamanP_val_SI_1000r, file="beraSamanP_valSI_1000r")


beraSamanP_val_CC_1000r<-data.frame(coef_ampute_CC1$p.value, coef_ampute_CC2$p.value, coef_ampute_CC3$p.value, coef_ampute_CC4$p.value, coef_ampute_CC5$p.value)
save(beraSamanP_val_CC_1000r, file="beraSamanP_val_CC_1000r")




## R2



beraSamanR2_PMM_1000r<-data.frame(mean(mR2gildi_PMM_1000r_1), mean(mR2gildi_PMM_1000r_2), mean(mR2gildi_PMM_1000r_3), mean(mR2gildi_PMM_1000r_4), mean(mR2gildi_PMM_1000r_5))
save(beraSamanR2_PMM_1000r, file="beraSamanR2_PMM_1000r")


beraSamanR2_MICE_1000r<-data.frame(mean(mR2gildi_MICE_1000r_1), mean(mR2gildi_MICE_1000r_2), mean(mR2gildi_MICE_1000r_3), mean(mR2gildi_MICE_1000r_4), mean(mR2gildi_MICE_1000r_5))
save(beraSamanR2_MICE_1000r, file="beraSamanR2_MICE_1000r")


beraSamanR2_SI_1000r<-data.frame(mean(mR2gildi_SI_1000r_1), mean(mR2gildi_SI_1000r_2), mean(mR2gildi_SI_1000r_3), mean(mR2gildi_SI_1000r_4), mean(mR2gildi_SI_1000r_5))
save(beraSamanR2_SI_1000r, file="beraSamanR2_SI_1000r")

beraSamanR2_CC_1000r<-data.frame(mean(mR2gildi_CC_1000r_1), mean(mR2gildi_CC_1000r_2), mean(mR2gildi_CC_1000r_3), mean(mR2gildi_CC_1000r_4), mean(mR2gildi_CC_1000r_5))
save(beraSamanR2_CC_1000r, file="beraSamanR2_CC_1000r")




