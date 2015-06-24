### Load Data

setwd("~/Documents/## Github Repo/Dospert-Risk-Domain-Analysis-/shiny")

library(xlsx)
#Load Dospert Original Survey Result (RISK part)
dospertRisk<-read.xlsx("./Dospert_Score_Risk_Association_Survey.xls",
                       sheetIndex=4,header=TRUE,colIndex=2:42,rowIndex=3:87)
names<-paste("Q",1:40,sep="")
colnames(dospertRisk)<-c("participants",names)

#Load Dospert Original Survey Result (BENEFIT part)
dospertBenefit<-read.xlsx("./Dospert_Score_Risk_Association_Survey.xls",
                          sheetIndex=5,header=TRUE,colIndex=2:42,rowIndex=3:87)
colnames(dospertBenefit)<-c("participants",names)

#Load Dospert coefficient
coef<-read.xlsx("./Dospert_Score_Risk_Association_Survey.xls",
                sheetIndex=1,header=TRUE,colIndex=4:5,rowIndex=4:44)
colnames(coef)<-c("benefit","risk")
#change data type from factor to numeric
coef$benefit<-sub("\\,",".",coef$benefit)
coef$risk<-sub("\\,",".",coef$risk)
coef<-apply(coef,2,as.numeric)

#Reset coef to 1, since coefficient computation is wrong. 
coef<-data.frame(matrix(rep(1,80),ncol=2))
colnames(coef)<-c("benefit","risk")

#Read Free Association
freeAssoc<-read.csv("./freeAssociation.csv",na.strings="NA",colClasses="character")
# set NULL to NA
freeAssoc<-freeAssoc[-1,]
# set NULL to NA
for (i in 1:nrow(freeAssoc)){
  index<-nchar(freeAssoc[i,])==0 # the subjec of nchar() must be a vector, not data.frame
  freeAssoc[i,index]<-NA
}
name<-1:160
colnames(freeAssoc)<-c("participant",name)


###Calculate coefficient *Score 

# Benefit calculation
benefit<-matrix(coef[,"benefit"])
risk<-matrix(coef[,"risk"])

benefit[is.na(benefit)]<-0
risk[is.na(risk)]<-0

dosBenefit<-as.matrix(dospertBenefit[,-1])
dosBenefit[is.na(dosBenefit)]<-0

dim(dosBenefit);dim(benefit)
weightedBenefit<-dosBenefit%*%benefit

# Risk calculation
dosRisk<-as.matrix(dospertRisk[,-1])
dosRisk[is.na(dosRisk)]<-0

weightedRisk<-dosRisk%*%risk


### Calculate Dospert Aggregate Score 
## Excel sheet get the answer wrong, sheet=benefit

dospertAgg<-data.frame(benefit=weightedBenefit,risk=weightedRisk)
dospertAgg$DospertScore<-apply(dospertAgg,1,sum)
# set 0 to NA
dospertAgg[dospertAgg==0]<-NA


### Pairwise multiplication of coefficient and answers to each questions

detailRiskW<-dosRisk
for (i in 1:nrow(dosRisk)){
  detailRiskW[i,]<-t(risk)*dosRisk[i,]
}

detailBenefitW<-dosBenefit
for (i in 1:nrow(dosBenefit)){
  detailBenefitW[i,]<-t(benefit)*dosBenefit[i,]
}
head(detailBenefitW)


### calculate by each domain 

f<-gl(5,8)
n<-nrow(detailRiskW)
domainAggRisk<-matrix(data=rep(0,5*84),ncol=5)

for (i in 1:n){
  domainAggRisk[i,]<-tapply(detailRiskW[i,],f,sum)}
colnames(domainAggRisk)<-c("financial","ethical","health","recreational","social")
head(domainAggRisk)

### Benefit
domainAggBenefit<-matrix(data=rep(0,5*84),ncol=5)

for (i in 1:n){
  domainAggBenefit[i,]<-tapply(detailBenefitW[i,],f,sum)}
colnames(domainAggBenefit)<-c("financial","ethical","health","recreational","social")
head(domainAggBenefit)


## Generate risk preference index (categorize into low(false) and high(true)), by risk perception, expected benefit, and aggregate

domainAggBenefitx<-data.frame(domainAggBenefit)
domainAggBenefitx$agg<-apply(domainAggBenefitx,1,sum)

domainAggRiskx<-data.frame(domainAggRisk)
domainAggRiskx$agg<-apply(domainAggRiskx,1,sum)


for (i in 1:nrow(dospertAgg)){
  mean<-mean(dospertAgg[,3],na.rm=T)
  dospertAgg$risk_preference[i]<-dospertAgg[i,3]>=mean
}
dospertAgg$risk_preference<-as.factor(dospertAgg$risk_preference)
levels(dospertAgg$risk_preference) <- c("Low","High")

for (i in 1:nrow(domainAggBenefitx)){
  mean<-mean(domainAggBenefitx[,6],na.rm=T)
  domainAggBenefitx$risk_preference[i]<-domainAggBenefitx[i,6]>=mean
}
domainAggBenefitx$risk_preference<-as.factor(domainAggBenefitx$risk_preference)
levels(domainAggBenefitx$risk_preference) <- c("Low","High")

for (i in 1:nrow(domainAggRiskx)){
  mean<-mean(domainAggRiskx[,6],na.rm=T)
  domainAggRiskx$risk_preference[i]<-domainAggRiskx[i,6]>=mean
}
domainAggRiskx$risk_preference<-as.factor(domainAggRiskx$risk_preference)
levels(domainAggRiskx$risk_preference) <- c("Low","High")

benefit <- domainAggBenefitx$risk_preference
perception <- domainAggRiskx$risk_preference
agg <- dospertAgg$risk_preference
  

### Prepare input and functions
# The list of valid books
Domain <<- list("Risk Taking" = "taking",
                "Expected Benefit" = "benefit",
                "Risk Perception" = "perception",
                "Aggregate Dospert Score"="agg")
Risk<<-list("High Risk"="High",
            "Low Risk"="Low")



## Prepare index
index<-data.frame(benefit=benefit,perception=perception,agg=agg)

freeAssoc<-freeAssoc[,-1]

collapse<-data.frame(text=apply(freeAssoc[,-1],1,paste,collapse=" "))
# Transform text column to factor
collapse$text<-as.character(collapse$text)


getTextH<-function(dt,domain){
  dt[index[,domain]=="High",]
}
getTextL<-function(dt,domain){
  dt[index[,domain]=="Low",]
}


plot<-function(dt,freq,maxwords){
  wordcloud(dt, scale = c(3, 0.35), 
            min.freq=freq,
            max.words=maxwords, 
            colors=brewer.pal(8, "Dark2"))
}

#aa<-getTextH(collapse,"perception")

