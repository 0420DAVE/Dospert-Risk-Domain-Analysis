
### Load Data

setwd("~/Documents/## Github Repo/Dospert-Risk-Domain-Analysis-")
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


### Exploring relationships between domains, in benefit domain

library(ggplot2)
# Transfor back to data.frame from matrix for plotting, remove NA observations
DFdomainAggBenefit<-data.frame(domainAggBenefit)
DFdomainAggBenefit[DFdomainAggBenefit == 0] <- NA
DFdomainAggBenefit<-na.omit(DFdomainAggBenefit)

ggplot(DFdomainAggBenefit, aes(financial,ethical))+geom_point()
ggplot(DFdomainAggBenefit, aes(financial,recreational))+geom_point()

# regress against financial
model1<-lm(financial~.,data=DFdomainAggBenefit)
summary(model1)
# all are not significant at 0.05 level

# regress against ethical
model2<-lm(ethical~.,data=DFdomainAggBenefit)
summary(model2)
# health is significant at 0.05 significant level

# regress against health
model3<-lm(health~.,data=DFdomainAggBenefit)
summary(model3)
# recreational is significant at 0.01 significant level,ethical is significant at 0.05 level

# regress against recreational
model4<-lm(recreational~.,data=DFdomainAggBenefit)
summary(model4)
# health is significant at 0.01 significant level,social is significant at 0.01 level

with(DFdomainAggBenefit,cor.test(recreational,health))
with(DFdomainAggBenefit,cor.test(recreational,social))
with(DFdomainAggBenefit,cor.test(health,ethical))


### Whether scores follows normal disrtibution

library(ggplot2)
par(mfrow=c(1,2))
hist(dospertAgg$DospertScore,breaks=10)
qqnorm(dospertAgg$DospertScore, col="blue");qqline(dospertAgg$DospertScore, col=2)
dev.off()


library(caret)
preObj<-preProcess(dospertAgg,method=c("BoxCox"))
BCdospertAgg<-predict(preObj,dospertAgg)
# Plot Histogtam and QQ plot
par(mfrow=c(1,2));hist(BCdospertAgg$DospertScore);qqnorm(BCdospertAgg$DospertScore);
qqline(BCdospertAgg$DospertScore, col=2)



### Categorize participants to risk taking and risk averse. 

for (i in 1:nrow(dospertAgg)){
  mean<-mean(dospertAgg[,3],na.rm=T)
  dospertAgg$risk_preference[i]<-dospertAgg[i,3]>=mean
}

dospertAgg$risk_preference<-as.factor(dospertAgg$risk_preference)
levels(dospertAgg$risk_preference) <- c("Low","High")

### Group free association data to high-risk and low-risk people

indexHigh<-dospertAgg$risk_preference=="High"
indexLow<-dospertAgg$risk_preference=="Low"
HighFreeAssoc<-freeAssoc[indexHigh,]
LowFreeAssoc<-freeAssoc[indexLow,]

# Extract all word input from high risk people
HighWord<-character(0)
for (i in 2:ncol(HighFreeAssoc)){
  i<-HighFreeAssoc[,i]
  HighWord<-c(HighWord,i)
}
# Transform every character to lowercase
HighWord<-tolower(HighWord)

HighWord<-data.frame(table(HighWord));HighWord<-HighWord[order(-HighWord[,2]),]
# Adjust the Frequency to make High risk and low risk group comparable
HighWord$adjusted<-HighWord$Freq/(nrow(HighWord))*1000


# Extract all word input from low risk people
LowWord<-character(0)
for (i in 2:ncol(LowFreeAssoc)){
  i<-LowFreeAssoc[,i]
  LowWord<-c(LowWord,i)
}
# Transform every character to lowercase
LowWord<-tolower(LowWord)

LowWord<-data.frame(table(LowWord));LowWord<-LowWord[order(-LowWord$Freq),]
# Adjust the Frequency to make High risk and low risk group comparable
LowWord$adjusted<-LowWord$Freq/(nrow(LowWord))*1000


### Plot bar graph for top20 word frequency

barplot(HighWord$adjusted[1:20], names.arg = HighWord$HighWord[1:20],col="steelblue",las = 2,ylim = c(0,100))

barplot(LowWord$Freq[1:20], names.arg = LowWord$LowWord[1:20],col="steelblue",las = 2,ylim = c(0,100))


### Word Cloud

par(mfrow=c(1,2))
library(wordcloud)
wordcloud(HighWord$HighWord, HighWord$Freq, scale=c(3,.2), min.freq=6,
          max.words=55, random.order=FALSE, rot.per=.15,ordered.colors=T,random.color=F)

wordcloud(LowWord$LowWord, LowWord$Freq, scale=c(3,.2), min.freq=6,
          max.words=55, random.order=FALSE, rot.per=.15)

### Exploring some "interesting" word

# 1. no
HighWord[HighWord$HighWord=="no",]
LowWord[LowWord$LowWord=="no",]

# Why no has such a big difference? Is it because one participant put "no" in everybox?
x<-freeAssoc=="no"
y<-apply(x,1,sum)
y
sum(y>100,na.rm=T) # there is one guy input more than 100 no

# 2. illegal
HighWord[HighWord$HighWord=="illegal",]
LowWord[LowWord$LowWord=="illegal",]

# Naive Bayesian

### Clean Data
###### Do we eliminate all incomplete answers?

head(dospertAgg,3)
# Do we eliminate all incomplete answers?
table(complete.cases(freeAssoc))

collapse<-apply(freeAssoc[,-1],1,paste,collapse=" ")
collapse<-data.frame(type=dospertAgg$risk_preference,text=collapse)
# Take a look the column type
sapply(names(collapse),FUN=function(x){class(collapse[,x])})
# Transform text column to factor
collapse$text<-as.character(collapse$text)

# Delete NA participants
collapse<-na.omit(collapse)

### Prepare text using "tm"

require("tm")
text_corpus<-Corpus(VectorSource(collapse$text))
inspect(text_corpus[1])

###### Deal with punctuation, case.

# Transform all letter to small case
clean<-tm_map(text_corpus,tolower)

# Remove all numbers
clean<-tm_map(clean,removeNumbers)

# Remove all stop words
# To view stops words, type stopwords()
clean<-tm_map(clean,removeWords,stopwords())

# Remove punctuation
clean<-tm_map(clean,removePunctuation)

# Remove white space
clean<-tm_map(clean,stripWhitespace)
clean<-tm_map(clean,PlainTextDocument)

# Inspect
inspect(clean[1])


#### Create a document term (sparse) matrix (dtm)

dtm <- DocumentTermMatrix(clean)
library(Matrix)
# x<-Matrix(dtm,sparse=TRUE)
# If you want to visualize dtm in window, pass the code below 
# dfdtm<-as.data.frame(as.matrix(dtm))
# View(dfdtm)


### Create training and text set 
##### raw file:        collapse
##### plain text file: clean
##### sparse matrix:   dtm


library(caret)
inTrain<-createDataPartition(y=collapse$type,p=0.7,list=F)

train_raw <-collapse[inTrain,]
test_raw  <-collapse[-inTrain,]

train_clean <-clean[inTrain]
test_clean  <-clean[-inTrain]

train_dtm <-dtm[inTrain,]
test_dtm  <-dtm[-inTrain,]

### Creating indicator for features for frequent words

freq_dict<-findFreqTerms(train_dtm,5)

train_freq <- DocumentTermMatrix(train_clean,list(dictionary=freq_dict))
test_freq  <- DocumentTermMatrix(test_clean, list(dictionary=freq_dict))

##### train data using Naive Bayesian by ***converting all frequency data*** to categorical

convert_counts<-function(x){
  x<-ifelse(x>0,1,0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}
##  convert to data.frame 
train_freq<-as.data.frame(as.matrix(train_freq)) 
test_freq<-as.data.frame(as.matrix(test_freq)) 

##  convert to categorical data
train_freq<-apply(train_freq,MARGIN=2,convert_counts)
test_freq<-apply(test_freq,MARGIN=2,convert_counts)

# Add type column to train_freq and test_freq
train_freq<-data.frame(type=train_raw$type,train_freq,row.names=NULL)
test_freq<-data.frame(type=test_raw$type,test_freq,row.names=NULL)

#### Train data with Bayesian
library(e1071)
    classifier <- naiveBayes(train_freq[,-1], train_freq$type,laplace = 1)
    classifier[[2]][1:5]

    #### Evaluating model performance 

    pred<- predict(classifier, test_freq)
    table(pred,test_freq$type)
    
    library(gmodels)
    CrossTable(pred, test_freq$type,
               prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
               dnn = c('predicted', 'actual'))
# 
groupByState<-function(dt,index){
  
}
    
    