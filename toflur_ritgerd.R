library(extrafont) 
library(tidyverse)
loadfonts()
windowsFonts(
  LLDR=windowsFont("lmroman10-regular")
)
windowsFonts(
  LLB=windowsFont("lmroman10-bold")
)
library(tableone)

tafla1<-CreateTableOne(data=gognDNA20000r_rett[,c(2,5,6,7,10,24,25)], factorVars=c( "fjolSamband2", "menntun2", "tekjurEigin",  "deprivation",  "ACE_f_total_cat", "binary_trait"))

library(xtable)
library(stargazer)
print(xtable(as.table(tafla1), digits=4,type = "latex"), file = "bussi.tex")



load("./gogn_og_imp/gognDNA1000r")

load("./gogn_og_imp/gognDNA5000r")

load("./gogn_og_imp/gognDNA10000r")

load("./gogn_og_imp/gognDNA15000r")

load("./gogn_og_imp/gognDNA20000r")



tafla1<-CreateTableOne(data=gognDNA1000_rett[2:25], factorVars=c("innflytNordurlond", "husHeilbrigdisumd", "fjolSamband2", "menntun2", "tekjurEigin", "stadaNuverandi2", "vinnutimi", "deprivation", "reyk", "audit3", "worstTrauma_freq", "ACE_f_total_cat", "binary_trait"),includeNA = TRUE)


gogn_fImpute_1000r_0.05<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.05")
gogn_fImpute_1000r_0.1<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.1")
gogn_fImpute_1000r_0.25<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.25")
gogn_fImpute_1000r_0.5<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.5")
gogn_fImpute_1000r_0.75<-readRDS("./gogn_og_imp/gogn_fImpute_1000r_0.75")

gogn_fImpute_5000r_0.05<-readRDS("./gogn_og_imp/gogn_fImpute_5000r_0.05")
gogn_fImpute_5000r_0.1<-readRDS("./gogn_og_imp/gogn_fImpute_5000r_0.1")
gogn_fImpute_5000r_0.25<-readRDS("./gogn_og_imp/gogn_fImpute_5000r_0.25")
gogn_fImpute_5000r_0.5<-readRDS("./gogn_og_imp/gogn_fImpute_5000r_0.5")
gogn_fImpute_5000r_0.75<-readRDS("./gogn_og_imp/gogn_fImpute_5000r_0.75")

gogn_fImpute_10000r_0.05<-readRDS("./gogn_og_imp/gogn_fImpute_10000r_0.05")
gogn_fImpute_10000r_0.1<-readRDS("./gogn_og_imp/gogn_fImpute_10000r_0.1")
gogn_fImpute_10000r_0.25<-readRDS("./gogn_og_imp/gogn_fImpute_10000r_0.25")
gogn_fImpute_10000r_0.5<-readRDS("./gogn_og_imp/gogn_fImpute_10000r_0.5")
gogn_fImpute_10000r_0.75<-readRDS("./gogn_og_imp/gogn_fImpute_10000r_0.75")


gogn_fImpute_15000r_0.05<-readRDS("./gogn_og_imp/gogn_fImpute_15000r_0.05")
gogn_fImpute_15000r_0.1<-readRDS("./gogn_og_imp/gogn_fImpute_15000r_0.1")
gogn_fImpute_15000r_0.25<-readRDS("./gogn_og_imp/gogn_fImpute_15000r_0.25")
gogn_fImpute_15000r_0.5<-readRDS("./gogn_og_imp/gogn_fImpute_15000r_0.5")
gogn_fImpute_15000r_0.75<-readRDS("./gogn_og_imp/gogn_fImpute_15000r_0.75")


gogn_fImpute_20000r_0.05<-readRDS("./gogn_og_imp/gogn_fImpute_20000r_0.05")
gogn_fImpute_20000r_0.1<-readRDS("./gogn_og_imp/gogn_fImpute_20000r_0.1")
gogn_fImpute_20000r_0.25<-readRDS("./gogn_og_imp/gogn_fImpute_20000r_0.25")
gogn_fImpute_20000r_0.5<-readRDS("./gogn_og_imp/gogn_fImpute_20000r_0.5")
gogn_fImpute_20000r_0.75<-readRDS("./gogn_og_imp/gogn_fImpute_20000r_0.75")




tafla1<-CreateTableOne(data=gogn_fImpute_20000r_0.75[,c(2,5,6,7,10,24,25)], factorVars=c( "fjolSamband2", "menntun2", "tekjurEigin",  "deprivation",  "ACE_f_total_cat", "binary_trait"), includeNA = TRUE)





## The print method returns a matrix.
tabAsStringMatrix <- print(tafla1, printToggle = FALSE, noSpaces = TRUE)

## Very crude, but it can work.
print(xtable(tabAsStringMatrix), digits=4, type="latex", file="tafla1.tex")

#mis pattern
library(tibble)
df <- tibble::rownames_to_column(kuku, "VALUE")
kuku<-data.frame(md.pattern(gognmNA))
df$VALUE<-gsub("X","",as.character(df$VALUE))
df$VALUE<-as.numeric(df$VALUE)
df2<-df[which(df$VALUE>50),]




susu<-data.frame(md.pattern(gognmNA[,c(5,6,7,10,18)]))
df2 <- tibble::rownames_to_column(susu, "VALUE")
df2$VALUE<-gsub("X","",as.character(df2$VALUE))
df2$VALUE<-as.numeric(df2$VALUE)
df2<-df2[which(df2$VALUE>5),]



m<-1
summary_gogn<-
summary_gogn<-data.frame(matrix(nrow = 5, ncol = 3))
for (i in 1:m) {
  dat<-complete(imp_MICE_m25_WD, i)
  dat<-dat[,c(5,6,7,10,24,25)]
  dat$binary_trait<-as.factor(dat$binary_trait)
  nam<-paste0(dat,i)
  assign(paste0("gogn_comp",i), dat)


  summ<-summary(dat)
  assign(paste0("summary",i),summ)
  summary_gogn[i]<-(summ)
}
sumgogn<-t(data.frame(summary1,summary2,summary3,summary4,summary5,summary6,summary7,summary8,summary9,summary10,summary11,summary12,summary13,summary14,summary15,summary16,summary17,summary18,summary19,summary20,summary21,summary22,summary23,summary24,summary25))
sumgogn<-sumgogn[,-c(3,4,5,10,15,18,19,20,28,29,30)]
sumgogn<-sumgogn[-c(seq(1,100,3)),]
sumgogn<-sumgogn[-c(seq(1,100,2)),]
sumgogn<-data.frame(rbind(sumgogn, c("Married or in a relationship", "Single or widowed", "Primary", "Secondary", "TertiaryA", "TertiaryB", "<300", "301-500", "501-700", "701+", "No", "Yes", "0", "1", "2", "3-4", "5+", "0", "1")))
sumgogn<-sumgogn[c(27,1:26),]
sumgogn$X1<-gsub("Married or in a relationship:","",as.character(sumgogn$X1)) %>% as.numeric()
sumgogn$X2<-gsub("Single or widowed           :","",as.character(sumgogn$X2)) %>% as.numeric()
sumgogn$X3<-gsub("Primary  :","",as.character(sumgogn$X3)) %>% as.numeric()
sumgogn$X4<-gsub("Secondary:","",as.character(sumgogn$X4)) %>% as.numeric()
sumgogn$X5<-gsub("TeriaryA :","",as.character(sumgogn$X5)) %>% as.numeric()
sumgogn$X6<-gsub("TertiaryB:","",as.character(sumgogn$X6)) %>% as.numeric()
sumgogn$X7<-gsub("<300   :","",as.character(sumgogn$X7)) %>% as.numeric()
sumgogn$X8<-gsub("301-500:","",as.character(sumgogn$X8)) %>% as.numeric()
sumgogn$X9<-gsub("501-700:","",as.character(sumgogn$X9)) %>% as.numeric() 
sumgogn$X10<-gsub("701.   :","",as.character(sumgogn$X10))  %>% as.numeric()
sumgogn$X11<-gsub("No :","",as.character(sumgogn$X11)) %>% as.numeric()
sumgogn$X12<-gsub("Yes: ","",as.character(sumgogn$X12)) %>% as.numeric()
sumgogn$X13<-gsub("0  :","",as.character(sumgogn$X13)) %>% as.numeric()
sumgogn$X14<-gsub("1  :","",as.character(sumgogn$X14)) %>% as.numeric()
sumgogn$X15<-gsub("2  :","",as.character(sumgogn$X15)) %>% as.numeric()
sumgogn$X16<-gsub("3-4:","",as.character(sumgogn$X16)) %>% as.numeric()
sumgogn$X17<-gsub("5. :","",as.character(sumgogn$X17)) %>% as.numeric()
sumgogn$X18<-gsub("0:","",as.character(sumgogn$X18)) %>% as.numeric()
sumgogn$X19<-gsub("1: ","",as.character(sumgogn$X19)) %>% as.numeric()

colnames(sumgogn)<-c("Married or in a relationship", "Single or widowed", "Primary", "Secondary", "TertiaryA", "TertiaryB", "<300", "301-500", "501-700", "701+", "No", "Yes", "0", "1", "2", "3-4", "5+", "No", "Yes")
#sumgogn<-sumgogn[c(3:27),]

sumgogn<-sumgogn[2:26,]
rownames(sumgogn)<-1:nrow(sumgogn)

print(xtable(t(hihi), digits=4,type = "latex"), file = "impdatasummary.tex")


lines(gogn_comp10$BMI)



hihi<-summary(sumgogn)



fit_mice<-with(imp_MICE_m25_WD,geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))
m=25
fit_gogn_mice<-data.frame(matrix(nrow=14, ncol=25))
for (i in 1:m) {
  fit_gogn_mice[,i]<-data.frame(fit_mice$analyses[[i]][1])
}

jiji<-summary(t(fit_gogn_mice))
colnames(jiji)<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+", "agecalculated", "deprivation_Yes", "menntun_Secondary", "menntun_TertiaryA", "TertiaryB", "TekjurEigin_301-500", "TekjurEigin_501-700", "TekjurEigin_701+", "fjolsamband_single")


print(xtable(t(jiji), digits=4,type = "latex"), file = "fit_gogn_mice.tex")

gigi<-summary(t(exp(fit_gogn_mice)))
colnames(gigi)<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+", "agecalculated", "deprivation_Yes", "menntun_Secondary", "menntun_TertiaryA", "TertiaryB", "TekjurEigin_301-500", "TekjurEigin_501-700", "TekjurEigin_701+", "fjolsamband_single")



print(xtable((t(gigi)), digits=4,type = "latex"), file = "fit_gogn_mice_exp.tex")

pool_mice<-(pool(fit_mice))
print(xtable((pool_mice), digits=4,type = "latex"), file = "pool_mice.tex")

pool_mice_exp<-summary(pool(fit_mice), exp=TRUE, conf.int=TRUE)

print(xtable((pool_mice_exp), digits=4,type = "latex"), file = "pool_mice_exp.tex")



bibi<-data.frame(pool_mice$pooled)

print(xtable(bibi, digits=4,type = "latex"), file = "pool_mice_fmi.tex")







fit_pmm<-with(imp_PMM_m25_WD,geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))

pool_pmm_exp<-summary(pool(fit_pmm), exp=TRUE, conf.int=TRUE)


print(xtable((pool_pmm_exp), digits=4,type = "latex"), file = "pool_pmm_exp.tex")





fit_SI<-with(imp_SI_m25_WD,geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)+factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin)))

pool_SI_exp<-summary((fit_SI), exp=TRUE, conf.int=TRUE)


print(xtable((pool_SI_exp), digits=4,type = "latex"), file = "pool_SI_exp.tex")


fit_CC<-geeglm((binary_trait==1) ~ factor(ACE_f_total_cat, order=FALSE)+ 
                 agecalculated + factor(deprivation, order=FALSE)+factor(menntun2, order=FALSE)
               +factor(tekjurEigin, order=FALSE)+ factor(fjolSamband2, order=FALSE), family=poisson, id=as.factor(pin), data=gognDNA)
fit_CC_tidy<-tidy(fit_CC, exp=TRUE, conf.int = TRUE)


data_OR<-data.frame(pool_mice_exp$estimate, pool_pmm_exp$estimate, pool_SI_exp$estimate, exp(fit_CC$coefficients))
colnames(data_OR)<-c("MICE", "PMM", "SI", "CCA")
print(xtable((data_OR), digits=3,type = "latex"), file = "Data_OR.tex")




data_CI<-data.frame(pool_mice_exp$`2.5 %`,pool_pmm_exp$`97.5 %`, pool_pmm_exp$`2.5 %`,pool_pmm_exp$`97.5 %`, pool_SI_exp$conf.low, pool_SI_exp$conf.high, fit_CC_tidy$conf.low, fit_CC_tidy$conf.high)
colnames(data_CI)<-c("MICE_low", "MICE_high", "PMM_low", "PMM_high", "SI_low", "SI_high", "CCA_low", "CCA_high")
print(xtable((data_CI), digits=3,type = "latex"), file = "Data_CI.tex")











### 1000r



### OR


momo05<-summary(fit_ampute_pmm05,  conf.int=TRUE, exp=TRUE)
m=20
coef=14
c05 <- momo05$estimate
datsss_05<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_05[i]<-c05[seq(i, length(c05), coef)]
}
datsss_05<-data.frame(t(datsss_05))
#colnames(datsss_c)<-names(fit_CC$coefficients)

# gigi<-summary(datsss_c)
# print(xtable(((gigi)), digits=4,type = "latex"), file = "coverage_PMM_1000r_0.05.tex")
# 
# 



library(reshape)
#datsss_c<- t(datsss_c)
datsss_05$nafn<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin")
datsss_05$CI1<-fit_CC_tidy$conf.low
datsss_05$CI2<-fit_CC_tidy$conf.high
datsss_05$True<-fit_CC_tidy$estimate

data_long <- melt((dataa), id = "nafn")




momo10<-summary(fit_ampute_pmm10,  conf.int=TRUE, exp=TRUE)

c10 <- momo10$estimate
datsss_10<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_10[i]<-c10[seq(i, length(c10), coef)]
}
datsss_10<-data.frame(t(datsss_10))

datsss_10$nafn<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin")
datsss_10$CI1<-fit_CC_tidy$conf.low
datsss_10$CI2<-fit_CC_tidy$conf.high
datsss_10$True<-fit_CC_tidy$estimate




momo25<-summary(fit_ampute_pmm25,  conf.int=TRUE, exp=TRUE)

c25 <- momo25$estimate
datsss_25<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_25[i]<-c25[seq(i, length(c25), coef)]
}
datsss_25<-data.frame(t(datsss_25))

datsss_25$nafn<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin")
datsss_25$CI1<-fit_CC_tidy$conf.low
datsss_25$CI2<-fit_CC_tidy$conf.high
datsss_25$True<-fit_CC_tidy$estimate


momo50<-summary(fit_ampute_pmm50,  conf.int=TRUE, exp=TRUE)

c50 <- momo50$estimate
datsss_50<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_50[i]<-c50[seq(i, length(c50), coef)]
}
datsss_50<-data.frame(t(datsss_50))

datsss_50$nafn<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin")
datsss_50$CI1<-fit_CC_tidy$conf.low
datsss_50$CI2<-fit_CC_tidy$conf.high
datsss_50$True<-fit_CC_tidy$estimate



momo75<-summary(fit_ampute_pmm75,  conf.int=TRUE, exp=TRUE)

c75 <- momo75$estimate
datsss_75<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_75[i]<-c75[seq(i, length(c75), coef)]
}
datsss_75<-data.frame(t(datsss_75))

datsss_75$nafn<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin")
datsss_75$CI1<-fit_CC_tidy$conf.low
datsss_75$CI2<-fit_CC_tidy$conf.high
datsss_75$True<-fit_CC_tidy$estimate


dataa<-rbind(datsss_05, datsss_10, datsss_25, datsss_50, datsss_75)
dataa$Mis_Level<-as.factor(c(rep("05",14), rep("10",14), rep("25", 14), rep("50", 14), rep("75", 14)))
rownames(dataa)<-1:nrow(dataa)
jpeg("1000r_PMM_regcoef.jpeg", quality = 100,
     width = 1000, height = 650, res = 100)
i <- ggplot(dataa[c(2:14, 16:28, 30:42, 44:56, 58:70),], aes(y=nafn, x=NULL ,color=Mis_Level))
i + xlab("")+ylab("")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,  family="LM Roman 10"))+ theme(text = element_text(size = 17,  family="LM Roman 10"))+ 
  geom_point(aes(x =X1), shape=1, size=4)+ geom_point(aes(x =X2), shape=1, size=4)+ geom_point(aes(x =X3), shape=1, size=4)+
  theme_bw()+ggtitle("RR - MI-PMM - 1000r")+
  geom_point(aes(x =X4), shape=1, size=4)+ geom_point(aes(x =X5), shape=1, size=4)+ geom_point(aes(x =X6), shape=1, size=4)+
  geom_point(aes(x =X7), shape=1, size=4)+geom_point(aes(x =X8), shape=1, size=4)+geom_point(aes(x =X9), shape=1, size=4)+
  geom_point(aes(x =X10), shape=1, size=4)+ geom_point(aes(x =X11), shape=1, size=4)+ geom_point(aes(x =X12), shape=1, size=4)+ geom_point(aes(x =X13), shape=1, size=4)+
  geom_point(aes(x =X14), shape=1, size=4)+ geom_point(aes(x =X15), shape=1, size=4)+ geom_point(aes(x =X16), shape=1, size=4)+
  geom_point(aes(x =X17),shape=1, size=4)+geom_point(aes(x =X18), shape=1, size=4)+geom_point(aes(x =X19), shape=1, size=4)+
  geom_point(aes(x =X20),shape=1, size=4)+geom_point(aes(x=True), color="black", shape=8, size=6)+ 
  theme(axis.text.x = element_text(angle=0, vjust = 1, hjust=1,  family="LLB"))+ theme(text = element_text(size = 17,  family="LLB")) +
  scale_color_manual(values = c("dodgerblue", "dodgerblue2", "darkmagenta", "firebrick1", "firebrick3"))+theme(legend.position = "none")
dev.off()









### 10000r



### OR


momo05<-summary(fit_ampute_pmm05,  conf.int=TRUE, exp=TRUE)
m=20
coef=14
c05 <- momo05$estimate
datsss_05<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_05[i]<-c05[seq(i, length(c05), coef)]
}
datsss_05<-data.frame(t(datsss_05))
#colnames(datsss_c)<-names(fit_CC$coefficients)

# gigi<-summary(datsss_c)
# print(xtable(((gigi)), digits=4,type = "latex"), file = "coverage_PMM_10000r_0.05.tex")
# 
# 



library(reshape)
#datsss_c<- t(datsss_c)
datsss_05$nafn<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin")
datsss_05$CI1<-fit_CC_tidy$conf.low
datsss_05$CI2<-fit_CC_tidy$conf.high
datsss_05$True<-fit_CC_tidy$estimate

data_long <- melt((dataa), id = "nafn")




momo10<-summary(fit_ampute_pmm10,  conf.int=TRUE, exp=TRUE)

c10 <- momo10$estimate
datsss_10<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_10[i]<-c10[seq(i, length(c10), coef)]
}
datsss_10<-data.frame(t(datsss_10))

datsss_10$nafn<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin")
datsss_10$CI1<-fit_CC_tidy$conf.low
datsss_10$CI2<-fit_CC_tidy$conf.high
datsss_10$True<-fit_CC_tidy$estimate




momo25<-summary(fit_ampute_pmm25,  conf.int=TRUE, exp=TRUE)

c25 <- momo25$estimate
datsss_25<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_25[i]<-c25[seq(i, length(c25), coef)]
}
datsss_25<-data.frame(t(datsss_25))

datsss_25$nafn<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin")
datsss_25$CI1<-fit_CC_tidy$conf.low
datsss_25$CI2<-fit_CC_tidy$conf.high
datsss_25$True<-fit_CC_tidy$estimate


momo50<-summary(fit_ampute_pmm50,  conf.int=TRUE, exp=TRUE)

c50 <- momo50$estimate
datsss_50<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_50[i]<-c50[seq(i, length(c50), coef)]
}
datsss_50<-data.frame(t(datsss_50))

datsss_50$nafn<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin")
datsss_50$CI1<-fit_CC_tidy$conf.low
datsss_50$CI2<-fit_CC_tidy$conf.high
datsss_50$True<-fit_CC_tidy$estimate



momo75<-summary(fit_ampute_pmm75,  conf.int=TRUE, exp=TRUE)

c75 <- momo75$estimate
datsss_75<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_75[i]<-c75[seq(i, length(c75), coef)]
}
datsss_75<-data.frame(t(datsss_75))

datsss_75$nafn<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin")
datsss_75$CI1<-fit_CC_tidy$conf.low
datsss_75$CI2<-fit_CC_tidy$conf.high
datsss_75$True<-fit_CC_tidy$estimate


dataa<-rbind(datsss_05, datsss_10, datsss_25, datsss_50, datsss_75)
dataa$Mis_Level<-as.factor(c(rep("05",14), rep("10",14), rep("25", 14), rep("50", 14), rep("75", 14)))
rownames(dataa)<-1:nrow(dataa)

jpeg("10000r_PMM_regcoef.jpeg", quality = 100,
     width = 1000, height = 650, res = 100)
i <- ggplot(dataa[c(2:14, 16:28, 30:42, 44:56, 58:70),], aes(y=nafn, x=NULL ,color=Mis_Level))
i + xlab("")+ylab("")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,  family="LM Roman 10"))+ theme(text = element_text(size = 17,  family="LM Roman 10"))+ 
  geom_point(aes(x =X1), shape=1, size=4)+ geom_point(aes(x =X2), shape=1, size=4)+ geom_point(aes(x =X3), shape=1, size=4)+
  theme_bw()+ggtitle("RR - MI-PMM - 10000r")+
  geom_point(aes(x =X4), shape=1, size=4)+ geom_point(aes(x =X5), shape=1, size=4)+ geom_point(aes(x =X6), shape=1, size=4)+
  geom_point(aes(x =X7), shape=1, size=4)+geom_point(aes(x =X8), shape=1, size=4)+geom_point(aes(x =X9), shape=1, size=4)+
  geom_point(aes(x =X10), shape=1, size=4)+ geom_point(aes(x =X11), shape=1, size=4)+ geom_point(aes(x =X12), shape=1, size=4)+ geom_point(aes(x =X13), shape=1, size=4)+
  geom_point(aes(x =X14), shape=1, size=4)+ geom_point(aes(x =X15), shape=1, size=4)+ geom_point(aes(x =X16), shape=1, size=4)+
  geom_point(aes(x =X17),shape=1, size=4)+geom_point(aes(x =X18), shape=1, size=4)+geom_point(aes(x =X19), shape=1, size=4)+
  geom_point(aes(x =X20),shape=1, size=4)+geom_point(aes(x=True), color="black", shape=8, size=6)+ 
  theme(axis.text.x = element_text(angle=0, vjust = 1, hjust=1,  family="LLB"))+ theme(text = element_text(size = 17,  family="LLB")) +
  scale_color_manual(values = c("dodgerblue", "dodgerblue2", "darkmagenta", "firebrick1", "firebrick3"))+theme(legend.position = "none")
dev.off()







### 20000r



### OR


momo05<-summary(fit_ampute_pmm05,  conf.int=TRUE, exp=TRUE)
m=20
coef=14
c05 <- momo05$estimate
datsss_05<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_05[i]<-c05[seq(i, length(c05), coef)]
}
datsss_05<-data.frame(t(datsss_05))
#colnames(datsss_c)<-names(fit_CC$coefficients)

# gigi<-summary(datsss_c)
# print(xtable(((gigi)), digits=4,type = "latex"), file = "coverage_PMM_20000r_0.05.tex")
# 
# 



library(reshape)
#datsss_c<- t(datsss_c)
datsss_05$nafn<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin")
datsss_05$CI1<-fit_CC_tidy$conf.low
datsss_05$CI2<-fit_CC_tidy$conf.high
datsss_05$True<-fit_CC_tidy$estimate

data_long <- melt((dataa), id = "nafn")




momo10<-summary(fit_ampute_pmm10,  conf.int=TRUE, exp=TRUE)

c10 <- momo10$estimate
datsss_10<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_10[i]<-c10[seq(i, length(c10), coef)]
}
datsss_10<-data.frame(t(datsss_10))

datsss_10$nafn<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin")
datsss_10$CI1<-fit_CC_tidy$conf.low
datsss_10$CI2<-fit_CC_tidy$conf.high
datsss_10$True<-fit_CC_tidy$estimate




momo25<-summary(fit_ampute_pmm25,  conf.int=TRUE, exp=TRUE)

c25 <- momo25$estimate
datsss_25<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_25[i]<-c25[seq(i, length(c25), coef)]
}
datsss_25<-data.frame(t(datsss_25))

datsss_25$nafn<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin")
datsss_25$CI1<-fit_CC_tidy$conf.low
datsss_25$CI2<-fit_CC_tidy$conf.high
datsss_25$True<-fit_CC_tidy$estimate


momo50<-summary(fit_ampute_pmm50,  conf.int=TRUE, exp=TRUE)

c50 <- momo50$estimate
datsss_50<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_50[i]<-c50[seq(i, length(c50), coef)]
}
datsss_50<-data.frame(t(datsss_50))

datsss_50$nafn<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin")
datsss_50$CI1<-fit_CC_tidy$conf.low
datsss_50$CI2<-fit_CC_tidy$conf.high
datsss_50$True<-fit_CC_tidy$estimate



momo75<-summary(fit_ampute_pmm75,  conf.int=TRUE, exp=TRUE)

c75 <- momo75$estimate
datsss_75<-data.frame(matrix(ncol = coef, nrow = m))
for (i in 1:coef) {
  datsss_75[i]<-c75[seq(i, length(c75), coef)]
}
datsss_75<-data.frame(t(datsss_75))

datsss_75$nafn<-c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin")
datsss_75$CI1<-fit_CC_tidy$conf.low
datsss_75$CI2<-fit_CC_tidy$conf.high
datsss_75$True<-fit_CC_tidy$estimate


dataa<-rbind(datsss_05, datsss_10, datsss_25, datsss_50, datsss_75)
dataa$Mis_Level<-as.factor(c(rep("05",14), rep("10",14), rep("25", 14), rep("50", 14), rep("75", 14)))
rownames(dataa)<-1:nrow(dataa)
jpeg("20000r_PMM_regcoef.jpeg", quality = 100,
     width = 1000, height = 650, res = 100)
i <- ggplot(dataa[c(2:14, 16:28, 30:42, 44:56, 58:70),], aes(y=nafn, x=NULL ,color=Mis_Level))
i + xlab("")+ylab("")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,  family="LM Roman 10"))+ theme(text = element_text(size = 17,  family="LM Roman 10"))+ 
  geom_point(aes(x =X1), shape=1, size=4)+ geom_point(aes(x =X2), shape=1, size=4)+ geom_point(aes(x =X3), shape=1, size=4)+
  theme_bw()+ggtitle("RR - MI-PMM - 20000r")+
  geom_point(aes(x =X4), shape=1, size=4)+ geom_point(aes(x =X5), shape=1, size=4)+ geom_point(aes(x =X6), shape=1, size=4)+
  geom_point(aes(x =X7), shape=1, size=4)+geom_point(aes(x =X8), shape=1, size=4)+geom_point(aes(x =X9), shape=1, size=4)+
  geom_point(aes(x =X10), shape=1, size=4)+ geom_point(aes(x =X11), shape=1, size=4)+ geom_point(aes(x =X12), shape=1, size=4)+ geom_point(aes(x =X13), shape=1, size=4)+
  geom_point(aes(x =X14), shape=1, size=4)+ geom_point(aes(x =X15), shape=1, size=4)+ geom_point(aes(x =X16), shape=1, size=4)+
  geom_point(aes(x =X17),shape=1, size=4)+geom_point(aes(x =X18), shape=1, size=4)+geom_point(aes(x =X19), shape=1, size=4)+
  geom_point(aes(x =X20),shape=1, size=4)+geom_point(aes(x=True), color="black", shape=8, size=6)+ 
  theme(axis.text.x = element_text(angle=0, vjust = 1, hjust=1,  family="LLB"))+ theme(text = element_text(size = 17,  family="LLB")) +
  scale_color_manual(values = c("dodgerblue", "dodgerblue2", "darkmagenta", "firebrick1", "firebrick3"))+theme(legend.position = "none")
dev.off()






########################## Coverage rett ----------------------------------------------------


# 1000r

bu<-tidy(fit_CC, conf.int=TRUE)

fit<-fit_ampute_pmm05
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
ci1_05<-(datsss-(1.96*datsss_error))
ci2_05<-(datsss+(1.96*datsss_error))



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
ci1_10<-(datsss-(1.96*datsss_error))
ci2_10<-(datsss+(1.96*datsss_error))





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
ci1_25<-(datsss-(1.96*datsss_error))
ci2_25<-(datsss+(1.96*datsss_error))




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
ci1_50<-(datsss-(1.96*datsss_error))
ci2_50<-(datsss+(1.96*datsss_error))


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
ci1_75<-(datsss-(1.96*datsss_error))
ci2_75<-(datsss+(1.96*datsss_error))
nofn<-c(rep(c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin"),5))
rett<-c(rep(fit_CC$coefficients,5))

ci1_allt<-rbind(ci1_05, ci1_10, ci1_25, ci1_50, ci1_75)
ci1_allt$nofn<-nofn
ci1_allt$true<-rett
rownames(ci1_allt)<-1:nrow(ci1_allt)

ci2_allt<-rbind(ci2_05, ci2_10, ci2_25, ci2_50, ci2_75)
ci2_allt$nofn<-nofn
ci2_allt$true<-rett
rownames(ci2_allt)<-1:nrow(ci2_allt)
colnames(ci2_allt)<- c("X1.2","X2.2","X3.2","X4.2","X5.2","X6.2","X7.2","X8.2","X9.2","X10.2",
                              "X11.2","X12.2","X13.2","X14.2","X15.2","X16.2","X17.2","X18.2","X19.2","X20.2",
                              "nofn.2", "true.2")



gaga<-cbind(ci1_allt, ci2_allt)
gaga$Mis_Level<-as.factor(c(rep("05",14), rep("10",14), rep("25", 14), rep("50", 14), rep("75", 14)))
gaga$CI1<-rep(bu$conf.low,5)
gaga$CI2<-rep(bu$conf.high,5)



jpeg("1000r_PMM_Coverage_rett.jpeg", quality = 100,
     width = 1000, height = 650, res = 100)
i <- ggplot(gaga[c(2:14, 16:28, 30:42, 44:56, 58:70),], aes(y=nofn, color=Mis_Level ))
i + xlab("")+ylab("")+ geom_point(aes(x = X1), shape=1, size=4)+ geom_point(aes(x = X2), shape=1, size=4)+ geom_point(aes(x = X3), shape=1, size=4)+theme_bw()+ggtitle("Coverage - MI-PMM - 1000r")+
  geom_point(aes(x =X4), shape=1, size=4)+ geom_point(aes(x =X5), shape=1, size=4)+ geom_point(aes(x =X6), shape=1, size=4)+
  geom_point(aes(x =X7), shape=1, size=4)+geom_point(aes(x =X8), shape=1, size=4)+geom_point(aes(x =X9), shape=1, size=4)+
  geom_point(aes(x =X10), shape=1, size=4)+ geom_point(aes(x =X11), shape=1, size=4)+ geom_point(aes(x =X12), shape=1, size=4)+ geom_point(aes(x =X13), shape=1, size=4)+
  geom_point(aes(x =X14), shape=1, size=4)+ geom_point(aes(x =X15), shape=1, size=4)+ geom_point(aes(x =X16), shape=1, size=4)+
  geom_point(aes(x =X17), shape=1, size=4)+geom_point(aes(x =X18), shape=1, size=4)+geom_point(aes(x =X19), shape=1, size=4)+
  geom_point(aes(x =X20), shape=1, size=4)+geom_point(aes(x=true), color="black", shape=18, size=4) + geom_point(aes(x =X1.2), shape=1, size=4)+ geom_point(aes(x =X2.2), shape=1, size=4)+ geom_point(aes(x =X3.2), shape=1, size=4)+theme_bw()+
  geom_point(aes(x =X4.2), shape=1, size=4)+ geom_point(aes(x =X5.2), shape=1, size=4)+ geom_point(aes(x =X6.2), shape=1, size=4)+
  geom_point(aes(x =X7.2), shape=1, size=4)+geom_point(aes(x =X8.2), shape=1, size=4)+geom_point(aes(x =X9.2), shape=1, size=4)+
  geom_point(aes(x =X10.2), shape=1, size=4)+ geom_point(aes(x =X11.2), shape=1, size=4)+ geom_point(aes(x =X12.2), shape=1, size=4)+ geom_point(aes(x =X13), shape=1, size=4)+
  geom_point(aes(x =X14.2), shape=1, size=4)+ geom_point(aes(x =X15.2), shape=1, size=4)+ geom_point(aes(x =X16.2), shape=1, size=4)+
  geom_point(aes(x =X17.2), shape=1, size=4)+geom_point(aes(x =X18.2), shape=1, size=4)+geom_point(aes(x =X19.2), shape=1, size=4)+
  geom_point(aes(x =X20.2), shape=1, size=4)+theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1,  family="LLB"))+ theme(text = element_text(size = 17,  family="LLB"))+
  scale_color_manual(values = c("dodgerblue", "dodgerblue2", "darkmagenta", "firebrick1", "firebrick3"))+theme(legend.position = "none") + geom_point(aes(x =CI1), shape=8, size=6, color="black")+ geom_point(aes(x =CI2), shape=8, size=6, color="black")

dev.off()

















# 10000r



bu<-tidy(fit_CC, conf.int=TRUE)


fit<-fit_ampute_pmm05
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
ci1_05<-(datsss-(1.96*datsss_error))
ci2_05<-(datsss+(1.96*datsss_error))



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
ci1_10<-(datsss-(1.96*datsss_error))
ci2_10<-(datsss+(1.96*datsss_error))





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
ci1_25<-(datsss-(1.96*datsss_error))
ci2_25<-(datsss+(1.96*datsss_error))




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
ci1_50<-(datsss-(1.96*datsss_error))
ci2_50<-(datsss+(1.96*datsss_error))


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
ci1_75<-(datsss-(1.96*datsss_error))
ci2_75<-(datsss+(1.96*datsss_error))
nofn<-c(rep(c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin"),5))
rett<-c(rep(fit_CC$coefficients,5))

ci1_allt<-rbind(ci1_05, ci1_10, ci1_25, ci1_50, ci1_75)
ci1_allt$nofn<-nofn
ci1_allt$true<-rett
rownames(ci1_allt)<-1:nrow(ci1_allt)

ci2_allt<-rbind(ci2_05, ci2_10, ci2_25, ci2_50, ci2_75)
ci2_allt$nofn<-nofn
ci2_allt$true<-rett
rownames(ci2_allt)<-1:nrow(ci2_allt)
colnames(ci2_allt)<- c("X1.2","X2.2","X3.2","X4.2","X5.2","X6.2","X7.2","X8.2","X9.2","X10.2",
                       "X11.2","X12.2","X13.2","X14.2","X15.2","X16.2","X17.2","X18.2","X19.2","X20.2",
                       "nofn.2", "true.2")



gaga<-cbind(ci1_allt, ci2_allt)
gaga$Mis_Level<-as.factor(c(rep("05",14), rep("10",14), rep("25", 14), rep("50", 14), rep("75", 14)))
gaga$CI1<-rep(bu$conf.low,5)
gaga$CI2<-rep(bu$conf.high,5)






jpeg("10000r_PMM_Coverage_rett.jpeg", quality = 100,
     width = 1000, height = 650, res = 100)
i <- ggplot(gaga[c(2:14, 16:28, 30:42, 44:56, 58:70),], aes(y=nofn, color=Mis_Level ))
i + xlab("")+ylab("")+ geom_point(aes(x = X1), shape=1, size=4)+ geom_point(aes(x = X2), shape=1, size=4)+ geom_point(aes(x = X3), shape=1, size=4)+theme_bw()+ggtitle("Coverage - MI-PMM - 10000r")+
  geom_point(aes(x =X4), shape=1, size=4)+ geom_point(aes(x =X5), shape=1, size=4)+ geom_point(aes(x =X6), shape=1, size=4)+
  geom_point(aes(x =X7), shape=1, size=4)+geom_point(aes(x =X8), shape=1, size=4)+geom_point(aes(x =X9), shape=1, size=4)+
  geom_point(aes(x =X10), shape=1, size=4)+ geom_point(aes(x =X11), shape=1, size=4)+ geom_point(aes(x =X12), shape=1, size=4)+ geom_point(aes(x =X13), shape=1, size=4)+
  geom_point(aes(x =X14), shape=1, size=4)+ geom_point(aes(x =X15), shape=1, size=4)+ geom_point(aes(x =X16), shape=1, size=4)+
  geom_point(aes(x =X17), shape=1, size=4)+geom_point(aes(x =X18), shape=1, size=4)+geom_point(aes(x =X19), shape=1, size=4)+
  geom_point(aes(x =X20), shape=1, size=4)+geom_point(aes(x=true), color="black", shape=18, size=4) + geom_point(aes(x =X1.2), shape=1, size=4)+ geom_point(aes(x =X2.2), shape=1, size=4)+ geom_point(aes(x =X3.2), shape=1, size=4)+theme_bw()+
  geom_point(aes(x =X4.2), shape=1, size=4)+ geom_point(aes(x =X5.2), shape=1, size=4)+ geom_point(aes(x =X6.2), shape=1, size=4)+
  geom_point(aes(x =X7.2), shape=1, size=4)+geom_point(aes(x =X8.2), shape=1, size=4)+geom_point(aes(x =X9.2), shape=1, size=4)+
  geom_point(aes(x =X10.2), shape=1, size=4)+ geom_point(aes(x =X11.2), shape=1, size=4)+ geom_point(aes(x =X12.2), shape=1, size=4)+ geom_point(aes(x =X13), shape=1, size=4)+
  geom_point(aes(x =X14.2), shape=1, size=4)+ geom_point(aes(x =X15.2), shape=1, size=4)+ geom_point(aes(x =X16.2), shape=1, size=4)+
  geom_point(aes(x =X17.2), shape=1, size=4)+geom_point(aes(x =X18.2), shape=1, size=4)+geom_point(aes(x =X19.2), shape=1, size=4)+
  geom_point(aes(x =X20.2), shape=1, size=4)+theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1,  family="LLB"))+ theme(text = element_text(size = 17,  family="LLB"))+
  scale_color_manual(values = c("dodgerblue", "dodgerblue2", "darkmagenta", "firebrick1", "firebrick3"))+theme(legend.position = "none") + geom_point(aes(x =CI1), shape=8, size=6, color="black")+ geom_point(aes(x =CI2), shape=8, size=6, color="black")

dev.off()























# 20000r




bu<-tidy(fit_CC, conf.int=TRUE)

fit<-fit_ampute_pmm05
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
ci1_05<-(datsss-(1.96*datsss_error))
ci2_05<-(datsss+(1.96*datsss_error))



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
ci1_10<-(datsss-(1.96*datsss_error))
ci2_10<-(datsss+(1.96*datsss_error))





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
ci1_25<-(datsss-(1.96*datsss_error))
ci2_25<-(datsss+(1.96*datsss_error))




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
ci1_50<-(datsss-(1.96*datsss_error))
ci2_50<-(datsss+(1.96*datsss_error))


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
ci1_75<-(datsss-(1.96*datsss_error))
ci2_75<-(datsss+(1.96*datsss_error))

nofn<-c(rep(c("Intercept", "ACE_1", "ACE_2", "ACE_3-4", "ACE_5+","Age", "Deprivation_Y", "Education_Sec", "Education_TA", "Education_TB", "Income_lowM", "Income_highM", "Income_high", "Cohabitation_Sin"),5))
rett<-c(rep(fit_CC$coefficients,5))
ci_true<-tidy(fit_CC, conf.int=TRUE)
ci1_true<-rep(ci_true$conf.low,5)
ci2_true<-rep(ci_true$conf.high, 5)



ci1_allt<-rbind(ci1_05, ci1_10, ci1_25, ci1_50, ci1_75)
ci1_allt$nofn<-nofn
ci1_allt$true<-rett
ci1_allt$ci_true<-ci1_true
rownames(ci1_allt)<-1:nrow(ci1_allt)

ci2_allt<-rbind(ci2_05, ci2_10, ci2_25, ci2_50, ci2_75)
ci2_allt$nofn<-nofn
ci2_allt$true<-rett
ci2_allt$ci_true.2<-ci2_true
rownames(ci2_allt)<-1:nrow(ci2_allt)
colnames(ci2_allt)<- c("X1.2","X2.2","X3.2","X4.2","X5.2","X6.2","X7.2","X8.2","X9.2","X10.2",
                       "X11.2","X12.2","X13.2","X14.2","X15.2","X16.2","X17.2","X18.2","X19.2","X20.2",
                       "nofn.2", "true.2", "ci_true.2")



gaga<-cbind(ci1_allt, ci2_allt)
gaga$Mis_Level<-as.factor(c(rep("05",14), rep("10",14), rep("25", 14), rep("50", 14), rep("75", 14)))
gaga$CI1<-rep(bu$conf.low,5)
gaga$CI2<-rep(bu$conf.high,5)




jpeg("20000r_PMM_Coverage_rett.jpeg", quality = 100,
     width = 1000, height = 650, res = 100)
i <- ggplot(gaga[c(2:14, 16:28, 30:42, 44:56, 58:70),], aes(y=nofn, color=Mis_Level ))
i + xlab("")+ylab("")+ geom_point(aes(x = X1), shape=1, size=4)+ geom_point(aes(x = X2), shape=1, size=4)+ geom_point(aes(x = X3), shape=1, size=4)+theme_bw()+ggtitle("Coverage - MI-PMM - 20000r")+
  geom_point(aes(x =X4), shape=1, size=4)+ geom_point(aes(x =X5), shape=1, size=4)+ geom_point(aes(x =X6), shape=1, size=4)+
  geom_point(aes(x =X7), shape=1, size=4)+geom_point(aes(x =X8), shape=1, size=4)+geom_point(aes(x =X9), shape=1, size=4)+
  geom_point(aes(x =X10), shape=1, size=4)+ geom_point(aes(x =X11), shape=1, size=4)+ geom_point(aes(x =X12), shape=1, size=4)+ geom_point(aes(x =X13), shape=1, size=4)+
  geom_point(aes(x =X14), shape=1, size=4)+ geom_point(aes(x =X15), shape=1, size=4)+ geom_point(aes(x =X16), shape=1, size=4)+
  geom_point(aes(x =X17), shape=1, size=4)+geom_point(aes(x =X18), shape=1, size=4)+geom_point(aes(x =X19), shape=1, size=4)+
  geom_point(aes(x =X20), shape=1, size=4)+geom_point(aes(x=true), color="black", shape=18, size=4) + geom_point(aes(x =X1.2), shape=1, size=4)+ geom_point(aes(x =X2.2), shape=1, size=4)+ geom_point(aes(x =X3.2), shape=1, size=4)+theme_bw()+
  geom_point(aes(x =X4.2), shape=1, size=4)+ geom_point(aes(x =X5.2), shape=1, size=4)+ geom_point(aes(x =X6.2), shape=1, size=4)+
  geom_point(aes(x =X7.2), shape=1, size=4)+geom_point(aes(x =X8.2), shape=1, size=4)+geom_point(aes(x =X9.2), shape=1, size=4)+
  geom_point(aes(x =X10.2), shape=1, size=4)+ geom_point(aes(x =X11.2), shape=1, size=4)+ geom_point(aes(x =X12.2), shape=1, size=4)+ geom_point(aes(x =X13), shape=1, size=4)+
  geom_point(aes(x =X14.2), shape=1, size=4)+ geom_point(aes(x =X15.2), shape=1, size=4)+ geom_point(aes(x =X16.2), shape=1, size=4)+
  geom_point(aes(x =X17.2), shape=1, size=4)+geom_point(aes(x =X18.2), shape=1, size=4)+geom_point(aes(x =X19.2), shape=1, size=4)+
  geom_point(aes(x =X20.2), shape=1, size=4)+theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust=1,  family="LLB"))+ theme(text = element_text(size = 17,  family="LLB"))+
  scale_color_manual(values = c("dodgerblue", "dodgerblue2", "darkmagenta", "firebrick1", "firebrick3"))+theme(legend.position = "none") + geom_point(aes(x =CI1), shape=8, size=6, color="black")+ geom_point(aes(x =CI2), shape=8, size=6, color="black")

dev.off()



























