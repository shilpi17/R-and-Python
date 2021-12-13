##### Importing data
diabetes=read.csv("C:/Users/shilpi.x.singh/OneDrive - Accenture/shilpi.x.singh/upGrad/classification Project/diabetes2.csv")
head(diabetes)
## Analyzing Descriptive Statistics of the data
summary(diabetes)

##create histogram of each variable to understand if all variables are normally distributed 
library(tidyr)
library(ggplot2)
library(purrr)
diabetes %>%
keep(is.numeric) %>%
gather() %>%
ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_histogram()

# create box plot of all independant variables by class
boxplot(BP2~Outcome, data = diabetes ,varwidth =1,las=1)
boxplot(Glucose2~Outcome, data = diabetes ,varwidth =1,las=1)
boxplot(BMI2~Outcome, data = diabetes ,varwidth =1,las=1)
boxplot(ST~Outcome, data = diabetes ,varwidth =1,las=1)
boxplot(Insulin2~Outcome, data = diabetes ,varwidth =1,las=1)
boxplot(DiabetesPedigreeFunction~Outcome, data = diabetes ,varwidth =1,las=1)

table(diabetes$Outcome)## There is a class imbalance

# checking correlation between datapoints ,install Corrplot
install.packages("corrplot")
library(corrplot)
corrplot(cor(diabetes[,c(1,3,5,7,9,11,12,13,16)]), method="number", is.corr=FALSE)

# Since plot was not visible chcking correlation in table format
data2 = diabetes[,c(1,3,5,7,9,11,12,13,16)]
res=cor(data2)
round(res,2)

# Testing for Correlation between Age and Pregnancy
cor.test(diabetes$Age,diabetes$Pregnancies)
# Testing for Correlation between BMI and Skin Thickness
cor.test(diabetes$BMI2,diabetes$ST)

#Hypothesis testing using Pearson Chi Square test as our response variable is categorical

tab=table(diabetes$Age2,diabetes$Outcome)
chisq.test(tab)

#Two Sample t tests
data2 = diabetes[,c(5,16)] # take one numerical predictor and one categorical

# Creating Subset of data by response variable
Y0=subset(data2,data2$Outcome==0)$BP2 
Y1=subset(data2,data2$Outcome==1)$BP2

# Normality test
shapiro.test(diabetes$BP2)

# independent t test using wilcox as BP2 doesn't follow normality.
wilcox.test(Y0,Y1)


# Data Splitting into train and test data
install.packages("caret")
library(caret)
require(caTools)
set.seed(123)
sample=sample.split(diabetes,SplitRatio=.8)
train1 =subset(diabetes,sample==TRUE) # creates a training dataset named train1 with rows which are marked as TRUEdata
View(train1)
write.csv(train1,"C:/Users/shilpi.x.singh/Downloads/train1.csv")
test1 =subset(diabetes,sample==FALSE)# creates a test dataset named test1 with rows which are marked as False
View(test1)
write.csv(test1,"C:/Users/shilpi.x.singh/Downloads/test1.csv")

#### Fitting full logistic regression (LR) model with all features
fullmod=glm(as.factor(Outcome)~as.factor(Age2)+Pregnancies+BP2+ST+Insulin2+BMI2+DiabetesPedigreeFunction,data=train1,family="binomial")
summary(fullmod)

#### Selecting features for fitting reduced logistic regression model
library(MASS)
step=stepAIC(fullmod)

###Recreating model with selected features only
mod1=glm(as.factor(Outcome)~as.factor(Age2)+ST+Insulin2+BMI2+DiabetesPedigreeFunction,data=train1,family="binomial")
summary(mod1)

### Predicting probabilities using Logistic regression model
test1_new=test1[,c(7,9,11,12,15,16)]# creating test data with only selected features
pred_prob=predict(mod1,test1_new,type="response")
hist(pred_prob)

## predicting probability of an individual being diabetic
sampletest=data.frame(t(c(1,25,201,27,0.578)))
colnames(sampletest)=c("Age2","ST","Insulin2","BMI2","DiabetesPedigreeFunction")
predict(mod1,sampletest,type="response")

### Plotting ROC
library(pROC)
roc1=roc(test1[,16],pred_prob,plot=TRUE,legacy.axes=TRUE)
plot(roc1)
roc1$auc

#### Using ROC in deciding threshold
thres=data.frame(sen=roc1$sensitivities, spec=roc1$specificities,thresholds=roc1$thresholds)
head(thres)
thres[thres$sen>0.7&thres$spec>0.4,]

### Confusion Matrix
library(caret)
pred_Y=ifelse(pred_prob > 0.20,1,0)
confusionMatrix(as.factor(test1[,16]), as.factor(pred_Y))





###############################
## Random Forest
###############################
library(randomForest)
diabetes$Age2=as.factor(diabetes$Age2)
diabetes$Outcome=as.factor(diabetes$Outcome)

modRF=randomForest(Outcome~ ., data=diabetes,ntree=500, mtry=6)
modRF


test1$Outcome=as.factor(test1$Outcome)
test1$Age2=as.factor(test1$Age2)

modRF1=randomForest(Outcome~ ., data=test1,ntree=500, mtry=6)
modRF1

predict(modRF,test1[10,-16],type="response")












