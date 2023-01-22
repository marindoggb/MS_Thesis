
source("gogn_preprocess.R")
source("patterns.R")


#1000 row sample from the completely observed data
set.seed(7231)
gognDNA1000r_rett<-gognDNA[sample(nrow(gognDNA), 1000), ]  #Random sample
gognDNA1000r<-sapply(gognDNA1000r_rett[,c(5:7,10,18)], as.numeric) #Make factors into numeric to use the ampute function
save(gognDNA1000r_rett, file="gognDNA1000r")


Lmis<-c(0.05,0.10,0.25,0.50,0.75) # vector of missing levels
gogn_ampute<-c() # create empty vector

#Loop that creates five missing datasets with the Lmis levels of missing
#In the loop the factors are also made factors again
for (i in Lmis) {
  set.seed(231)
  gogn_ampute<- ampute(data = gognDNA1000r,  patterns = patt, freq = freqqq, prop=i) 
  gagna<-gogn_ampute$amp
  gagna$menntun2<- as.factor(gagna$menntun2)
  levels(gagna$menntun2)=c("Primary", "Secondary", "TertiaryA", "TertiaryB")
  gagna$menntun2 <- factor(gagna$menntun2, ordered=TRUE,levels=c("Primary", "Secondary", "TertiaryA", "TertiaryB"))
  gagna$tekjurEigin<- as.factor(gagna$tekjurEigin)
  levels(gagna$tekjurEigin)=c("<300", "301-500", "501-700","701+")
  gagna$tekjurEigin <- factor(gagna$tekjurEigin, ordered=TRUE,levels=c("<300", "301-500", "501-700","701+"))
  gagna$fjolSamband2<- as.factor(gagna$fjolSamband2)
  levels(gagna$fjolSamband2) <- c("Married or in a relationship", "Single or Widowed")
  gagna$deprivation<- factor(ifelse(gagna$deprivation==1, "No","Yes"), ordered= TRUE, levels=c("No", "Yes"))
  gogn_fImpute<-data.frame(gognDNA1000_rett)
  gogn_fImpute$fjolSamband2<-gagna$fjolSamband2
  gogn_fImpute$menntun2<- gagna$menntun2
  gogn_fImpute$tekjurEigin<- gagna$tekjurEigin
  gogn_fImpute$deprivation<- gagna$deprivation
  gogn_fImpute$PHQ9_row_imputed_total<- gagna$PHQ9_row_imputed_total
  gogn_fImpute$pin<- as.factor(gogn_fImpute$pin)
  gogn_fImpute_1000r<-gogn_fImpute
  #print(summary(gogn_fImpute_1000r)) To check if everything looks good
  nam <- paste("gogn_fImpute_1000r", i, sep = "_")
  assign(paste0("gogn_fImpute_1000r_", i), gogn_fImpute_1000r) %>% saveRDS(,file=nam)
  }

## Do exactly the same for the other sample sizes

# 5000r


set.seed(231)
gognDNA5000r_rett<-gognDNA[sample(nrow(gognDNA), 5000), ]
gognDNA5000r<-sapply(gognDNA5000r_rett[,c(5:7,10,18)], as.numeric)
save(gognDNA5000r_rett, file="gognDNA5000r")

Lmis<-c(0.05,0.10,0.25,0.50,0.75)
gogn_ampute<-c()

for (i in Lmis) {
  set.seed(231)
  gogn_ampute<- ampute(data = gognDNA5000r,  patterns = patt, freq = freqqq, prop=i)
  gagna<-gogn_ampute$amp
  gagna$menntun2<- as.factor(gagna$menntun2)
  levels(gagna$menntun2)=c("Primary", "Secondary", "TertiaryA", "TertiaryB")
  gagna$menntun2 <- factor(gagna$menntun2, ordered=TRUE,levels=c("Primary", "Secondary", "TertiaryA", "TertiaryB"))
  gagna$tekjurEigin<- as.factor(gagna$tekjurEigin)
  levels(gagna$tekjurEigin)=c("<300", "301-500", "501-700","701+")
  gagna$tekjurEigin <- factor(gagna$tekjurEigin, ordered=TRUE,levels=c("<300", "301-500", "501-700","701+"))
  gagna$fjolSamband2<- as.factor(gagna$fjolSamband2)
  levels(gagna$fjolSamband2) <- c("Married or in a relationship", "Single or Widowed")
  gagna$deprivation<- factor(ifelse(gagna$deprivation==1, "No","Yes"), ordered= TRUE, levels=c("No", "Yes"))
  gogn_fImpute<-data.frame(gognDNA5000r_rett)
  gogn_fImpute$fjolSamband2<-gagna$fjolSamband2
  gogn_fImpute$menntun2<- gagna$menntun2
  gogn_fImpute$tekjurEigin<- gagna$tekjurEigin
  gogn_fImpute$deprivation<- gagna$deprivation
  gogn_fImpute$PHQ9_row_imputed_total<- gagna$PHQ9_row_imputed_total
  gogn_fImpute$pin<- as.factor(gogn_fImpute$pin)
  gogn_fImpute_5000r<-gogn_fImpute
 # print(summary(gogn_fImpute_5000r))
  nam <- paste("gogn_fImpute_5000r", i, sep = "_")
  assign(paste0("gogn_fImpute_5000r_", i), gogn_fImpute_5000r) %>% saveRDS(,file=nam)
}


# 10000r

set.seed(231)
gognDNA10000r_rett<-gognDNA[sample(nrow(gognDNA), 10000), ]
gognDNA10000r<-sapply(gognDNA10000r_rett[,c(5:7,10,18)], as.numeric)
save(gognDNA10000r_rett, file="gognDNA10000r")

Lmis<-c(0.05,0.10,0.25,0.50,0.75)
gogn_ampute<-c()

for (i in Lmis) {
  set.seed(231)
  gogn_ampute<- ampute(data = gognDNA10000r,  patterns = patt, freq = freqqq, prop=i)
  gagna<-gogn_ampute$amp
  gagna$menntun2<- as.factor(gagna$menntun2)
  levels(gagna$menntun2)=c("Primary", "Secondary", "TertiaryA", "TertiaryB")
  gagna$menntun2 <- factor(gagna$menntun2, ordered=TRUE,levels=c("Primary", "Secondary", "TertiaryA", "TertiaryB"))
  gagna$tekjurEigin<- as.factor(gagna$tekjurEigin)
  levels(gagna$tekjurEigin)=c("<300", "301-500", "501-700","701+")
  gagna$tekjurEigin <- factor(gagna$tekjurEigin, ordered=TRUE,levels=c("<300", "301-500", "501-700","701+"))
  gagna$fjolSamband2<- as.factor(gagna$fjolSamband2)
  levels(gagna$fjolSamband2) <- c("Married or in a relationship", "Single or Widowed")
  gagna$deprivation<- factor(ifelse(gagna$deprivation==1, "No","Yes"), ordered= TRUE, levels=c("No", "Yes"))
  gogn_fImpute<-data.frame(gognDNA10000r_rett)
  gogn_fImpute$fjolSamband2<-gagna$fjolSamband2
  gogn_fImpute$menntun2<- gagna$menntun2
  gogn_fImpute$tekjurEigin<- gagna$tekjurEigin
  gogn_fImpute$deprivation<- gagna$deprivation
  gogn_fImpute$PHQ9_row_imputed_total<- gagna$PHQ9_row_imputed_total
  gogn_fImpute$pin<- as.factor(gogn_fImpute$pin)
  gogn_fImpute_10000r<-gogn_fImpute
  #print(summary(gogn_fImpute_10000r))
  nam <- paste("gogn_fImpute_10000r", i, sep = "_")
  assign(paste0("gogn_fImpute_10000r_", i), gogn_fImpute_10000r) %>% saveRDS(,file=nam)
}





# 15000r

set.seed(231)
gognDNA15000r_rett<-gognDNA[sample(nrow(gognDNA), 15000), ]
gognDNA15000r<-sapply(gognDNA15000r_rett[,c(5:7,10,18)], as.numeric)
save(gognDNA15000r_rett, file="gognDNA15000r")

Lmis<-c(0.05,0.10,0.25,0.50,0.75)
gogn_ampute<-c()

for (i in Lmis) {
  set.seed(231)
  gogn_ampute<- ampute(data = gognDNA15000r,  patterns = patt, freq = freqqq, prop=i)
  gagna<-gogn_ampute$amp
  gagna$menntun2<- as.factor(gagna$menntun2)
  levels(gagna$menntun2)=c("Primary", "Secondary", "TertiaryA", "TertiaryB")
  gagna$menntun2 <- factor(gagna$menntun2, ordered=TRUE,levels=c("Primary", "Secondary", "TertiaryA", "TertiaryB"))
  gagna$tekjurEigin<- as.factor(gagna$tekjurEigin)
  levels(gagna$tekjurEigin)=c("<300", "301-500", "501-700","701+")
  gagna$tekjurEigin <- factor(gagna$tekjurEigin, ordered=TRUE,levels=c("<300", "301-500", "501-700","701+"))
  gagna$fjolSamband2<- as.factor(gagna$fjolSamband2)
  levels(gagna$fjolSamband2) <- c("Married or in a relationship", "Single or Widowed")
  gagna$deprivation<- factor(ifelse(gagna$deprivation==1, "No","Yes"), ordered= TRUE, levels=c("No", "Yes"))
  gogn_fImpute<-data.frame(gognDNA15000r_rett)
  gogn_fImpute$fjolSamband2<-gagna$fjolSamband2
  gogn_fImpute$menntun2<- gagna$menntun2
  gogn_fImpute$tekjurEigin<- gagna$tekjurEigin
  gogn_fImpute$deprivation<- gagna$deprivation
  gogn_fImpute$PHQ9_row_imputed_total<- gagna$PHQ9_row_imputed_total
  gogn_fImpute$pin<- as.factor(gogn_fImpute$pin)
  gogn_fImpute_15000r<-gogn_fImpute
  print(summary(gogn_fImpute_15000r))
  nam <- paste("gogn_fImpute_15000r", i, sep = "_")
  assign(paste0("gogn_fImpute_15000r_", i), gogn_fImpute_15000r) %>% saveRDS(,file=nam)
}




# 20000r

set.seed(231)
gognDNA20000r_rett<-gognDNA[sample(nrow(gognDNA), 20000), ]
gognDNA20000r<-sapply(gognDNA20000r_rett[,c(5:7,10,18)], as.numeric)
save(gognDNA20000r_rett, file="gognDNA20000r")

Lmis<-c(0.05,0.10,0.25,0.50,0.75)
gogn_ampute<-c()

for (i in Lmis) {
  set.seed(231)
  gogn_ampute<- ampute(data = gognDNA20000r,  patterns = patt, freq = freqqq, prop=i)
  gagna<-gogn_ampute$amp
  gagna$menntun2<- as.factor(gagna$menntun2)
  levels(gagna$menntun2)=c("Primary", "Secondary", "TertiaryA", "TertiaryB")
  gagna$menntun2 <- factor(gagna$menntun2, ordered=TRUE,levels=c("Primary", "Secondary", "TertiaryA", "TertiaryB"))
  gagna$tekjurEigin<- as.factor(gagna$tekjurEigin)
  levels(gagna$tekjurEigin)=c("<300", "301-500", "501-700","701+")
  gagna$tekjurEigin <- factor(gagna$tekjurEigin, ordered=TRUE,levels=c("<300", "301-500", "501-700","701+"))
  gagna$fjolSamband2<- as.factor(gagna$fjolSamband2)
  levels(gagna$fjolSamband2) <- c("Married or in a relationship", "Single or Widowed")
  gagna$deprivation<- factor(ifelse(gagna$deprivation==1, "No","Yes"), ordered= TRUE, levels=c("No", "Yes"))
  gogn_fImpute<-data.frame(gognDNA20000r_rett)
  gogn_fImpute$fjolSamband2<-gagna$fjolSamband2
  gogn_fImpute$menntun2<- gagna$menntun2
  gogn_fImpute$tekjurEigin<- gagna$tekjurEigin
  gogn_fImpute$deprivation<- gagna$deprivation
  gogn_fImpute$PHQ9_row_imputed_total<- gagna$PHQ9_row_imputed_total
  gogn_fImpute$pin<- as.factor(gogn_fImpute$pin)
  gogn_fImpute_20000r<-gogn_fImpute
  print(summary(gogn_fImpute_20000r))
  nam <- paste("gogn_fImpute_20000r", i, sep = "_")
  assign(paste0("gogn_fImpute_20000r_", i), gogn_fImpute_20000r) %>% saveRDS(,file=nam)
}









### MNAR




source("gogn_preprocess.R")
source("patterns.R")


#Take 1000r sample from the complete cases data
#Exactly the same as before except now the mechanism
#is MNAR and the sample sizes are 1000r,10000r,20000r


set.seed(7231)
gognDNA1000r_MNAR_rett<-gognDNA[sample(nrow(gognDNA), 1000), ]
gognDNA1000r_MNAR<-sapply(gognDNA1000r_MNAR_rett[,c(5:7,10,18)], as.numeric)
save(gognDNA1000r_MNAR_rett, file="gognDNA1000r_MNAR")


Lmis<-c(0.05,0.10,0.25,0.50,0.75)
gogn_ampute<-c()

for (i in Lmis) {
  set.seed(231)
  gogn_ampute<- ampute(data = gognDNA1000r_MNAR,  patterns = patt, freq = freqqq, prop=i,mech="MNAR")
  gagna<-gogn_ampute$amp
  gagna$menntun2<- as.factor(gagna$menntun2)
  levels(gagna$menntun2)=c("Primary", "Secondary", "TertiaryA", "TertiaryB")
  gagna$menntun2 <- factor(gagna$menntun2, ordered=TRUE,levels=c("Primary", "Secondary", "TertiaryA", "TertiaryB"))
  gagna$tekjurEigin<- as.factor(gagna$tekjurEigin)
  levels(gagna$tekjurEigin)=c("<300", "301-500", "501-700","701+")
  gagna$tekjurEigin <- factor(gagna$tekjurEigin, ordered=TRUE,levels=c("<300", "301-500", "501-700","701+"))
  gagna$fjolSamband2<- as.factor(gagna$fjolSamband2)
  levels(gagna$fjolSamband2) <- c("Married or in a relationship", "Single or Widowed")
  gagna$deprivation<- factor(ifelse(gagna$deprivation==1, "No","Yes"), ordered= TRUE, levels=c("No", "Yes"))
  gogn_fImpute<-data.frame(gognDNA1000_rett)
  gogn_fImpute$fjolSamband2<-gagna$fjolSamband2
  gogn_fImpute$menntun2<- gagna$menntun2
  gogn_fImpute$tekjurEigin<- gagna$tekjurEigin
  gogn_fImpute$deprivation<- gagna$deprivation
  gogn_fImpute$PHQ9_row_imputed_total<- gagna$PHQ9_row_imputed_total
  gogn_fImpute$pin<- as.factor(gogn_fImpute$pin)
  gogn_fImpute_1000r_MNAR<-gogn_fImpute
  #print(summary(gogn_fImpute_1000r_MNAR))
  nam <- paste("gogn_fImpute_1000r_MNAR", i, sep = "_")
  assign(paste0("gogn_fImpute_1000r_MNAR_", i), gogn_fImpute_1000r_MNAR) %>% saveRDS(,file=nam)
}



# 10000r_MNAR

set.seed(231)
gognDNA10000r_MNAR_rett<-gognDNA[sample(nrow(gognDNA), 10000), ]
gognDNA10000r_MNAR<-sapply(gognDNA10000r_MNAR_rett[,c(5:7,10,18)], as.numeric)
save(gognDNA10000r_MNAR_rett, file="gognDNA10000r_MNAR")


Lmis<-c(0.05,0.10,0.25,0.50,0.75)
gogn_ampute<-c()

for (i in Lmis) {
  set.seed(231)
  gogn_ampute<- ampute(data = gognDNA10000r_MNAR,  patterns = patt, freq = freqqq, prop=i,mech="MNAR")
  gagna<-gogn_ampute$amp
  gagna$menntun2<- as.factor(gagna$menntun2)
  levels(gagna$menntun2)=c("Primary", "Secondary", "TertiaryA", "TertiaryB")
  gagna$menntun2 <- factor(gagna$menntun2, ordered=TRUE,levels=c("Primary", "Secondary", "TertiaryA", "TertiaryB"))
  gagna$tekjurEigin<- as.factor(gagna$tekjurEigin)
  levels(gagna$tekjurEigin)=c("<300", "301-500", "501-700","701+")
  gagna$tekjurEigin <- factor(gagna$tekjurEigin, ordered=TRUE,levels=c("<300", "301-500", "501-700","701+"))
  gagna$fjolSamband2<- as.factor(gagna$fjolSamband2)
  levels(gagna$fjolSamband2) <- c("Married or in a relationship", "Single or Widowed")
  gagna$deprivation<- factor(ifelse(gagna$deprivation==1, "No","Yes"), ordered= TRUE, levels=c("No", "Yes"))
  gogn_fImpute<-data.frame(gognDNA10000r_MNAR_rett)
  gogn_fImpute$fjolSamband2<-gagna$fjolSamband2
  gogn_fImpute$menntun2<- gagna$menntun2
  gogn_fImpute$tekjurEigin<- gagna$tekjurEigin
  gogn_fImpute$deprivation<- gagna$deprivation
  gogn_fImpute$PHQ9_row_imputed_total<- gagna$PHQ9_row_imputed_total
  gogn_fImpute$pin<- as.factor(gogn_fImpute$pin)
  gogn_fImpute_10000r_MNAR<-gogn_fImpute
 # print(summary(gogn_fImpute_10000r_MNAR))
  nam <- paste("gogn_fImpute_10000r_MNAR", i, sep = "_")
  assign(paste0("gogn_fImpute_10000r_MNAR_", i), gogn_fImpute_10000r_MNAR) %>% saveRDS(,file=nam)
}






# 20000r_MNAR

set.seed(231)
gognDNA20000r_MNAR_rett<-gognDNA[sample(nrow(gognDNA), 20000), ]
gognDNA20000r_MNAR<-sapply(gognDNA20000r_MNAR_rett[,c(5:7,10,18)], as.numeric)
save(gognDNA20000r_MNAR_rett, file="gognDNA20000r_MNAR")

Lmis<-c(0.05,0.10,0.25,0.50,0.75)
gogn_ampute<-c()

for (i in Lmis) {
  set.seed(231)
  gogn_ampute<- ampute(data = gognDNA20000r_MNAR,  patterns = patt, freq = freqqq, prop=i,mech="MNAR")
  gagna<-gogn_ampute$amp
  gagna$menntun2<- as.factor(gagna$menntun2)
  levels(gagna$menntun2)=c("Primary", "Secondary", "TertiaryA", "TertiaryB")
  gagna$menntun2 <- factor(gagna$menntun2, ordered=TRUE,levels=c("Primary", "Secondary", "TertiaryA", "TertiaryB"))
  gagna$tekjurEigin<- as.factor(gagna$tekjurEigin)
  levels(gagna$tekjurEigin)=c("<300", "301-500", "501-700","701+")
  gagna$tekjurEigin <- factor(gagna$tekjurEigin, ordered=TRUE,levels=c("<300", "301-500", "501-700","701+"))
  gagna$fjolSamband2<- as.factor(gagna$fjolSamband2)
  levels(gagna$fjolSamband2) <- c("Married or in a relationship", "Single or Widowed")
  gagna$deprivation<- factor(ifelse(gagna$deprivation==1, "No","Yes"), ordered= TRUE, levels=c("No", "Yes"))
  gogn_fImpute<-data.frame(gognDNA20000r_MNAR_rett)
  gogn_fImpute$fjolSamband2<-gagna$fjolSamband2
  gogn_fImpute$menntun2<- gagna$menntun2
  gogn_fImpute$tekjurEigin<- gagna$tekjurEigin
  gogn_fImpute$deprivation<- gagna$deprivation
  gogn_fImpute$PHQ9_row_imputed_total<- gagna$PHQ9_row_imputed_total
  gogn_fImpute$pin<- as.factor(gogn_fImpute$pin)
  gogn_fImpute_20000r_MNAR<-gogn_fImpute
 # print(summary(gogn_fImpute_20000r_MNAR))
  nam <- paste("gogn_fImpute_20000r_MNAR", i, sep = "_")
  assign(paste0("gogn_fImpute_20000r_MNAR_", i), gogn_fImpute_20000r_MNAR) %>% saveRDS(,file=nam)
}



