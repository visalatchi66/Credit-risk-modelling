#Loading the libraries
install.packages("psych")
install.packages("GPArotation")
install.packages("rpart.plot")
library(dplyr)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(doBy)
library(rpart.plot)
library(caret)
library(psych)
library(GPArotation)
library(randomForest)

#Loading the dataset
df=read.csv('C:/Users/saluv/OneDrive/Desktop/DA/R/Credit Risk Data.csv')

#Looking at the dataframe and variable type
head(df,10)
str(df)
dim(df)
#Looking at the number of rows and columns in dataframe
nrow(df)
ncol(df)

#Checking for null values
anyNA(df)

anyNA(df$SeriousDlqin2yrs)
anyNA(df$ID)
anyNA(df$RevolvingUtilizationOfUnsecuredLines)
anyNA(df$age)
anyNA(df$NumberOfTime30.59DaysPastDueNotWorse)
anyNA(df$DebtRatio)
anyNA(df$NumberOfOpenCreditLinesAndLoans)
anyNA(df$NumberOfTimes90DaysLate)
anyNA(df$NumberRealEstateLoansOrLines)
anyNA(df$NumberOfTime60.89DaysPastDueNotWorse)
anyNA(df$NumberOfDependents)
anyNA(df$MonthlyIncome)

summary(df)


#Looking for duplicate values 
if(duplicated(df)==FALSE){
  print(df$ID)
}


#Issues in the dataset:
#1.Errorneous datatype of id
#2.Erroneous datatype of SeriousDlqin2yrs
#3.Presence of null values

#Creating a copy before cleaning
write.csv(df,file='df_clean.csv',row.names = FALSE)

#Reading df1
df1=read.csv('C:/Users/saluv/OneDrive/Desktop/DA/R/df_clean.csv')
head(df1)

#Converting the datype of id to char
df1$ID=as.character(df1$ID)
str(df1$ID)

#Converting datatype of SeriousDlqin2yrs to factor
df1$SeriousDlqin2yrs=as.factor(df1$SeriousDlqin2yrs)
str(df1$SeriousDlqin2yrs)


#Replacing null values in number of dependents as 0
df1$NumberOfDependents[is.na(df1$NumberOfDependents)==TRUE]=0
df1$NumberOfDependents

#Replacing null values of monthly income to mean monthly income
summary(df1)
df1$MonthlyIncome[is.na(df1$MonthlyIncome)==TRUE]=6670
df1$MonthlyIncome

#Removing data which has age as 0
filter(df1,age==0)
df1=df1[-c(65696),]

#finding mean of monthly income of defaulters
df1_def=filter(df1,SeriousDlqin2yrs==1)
head(df1_def)
mean(df1_def$MonthlyIncome)

#finding mean of monthly income of non-defaulters
df1_nondef=filter(df1,SeriousDlqin2yrs==0)
head(df1_nondef)
mean(df1_nondef$MonthlyIncome)


#Different age group

df1$age_group[(df1$age>=18 & df1$age<=30)]='Young'
df1$age_group[(df1$age>30 & df1$age<=55)]='Middleage'
df1$age_group[df1$age>55]='Old'
df1$age_group=as.factor(df1$age_group) 
str(df1$age_group)
anyNA(df1$age_group)

#Finding mean income by SeriousDlqin2yrs and  age_group
df1%>%group_by(SeriousDlqin2yrs)%>%summarise(mean_income = mean(MonthlyIncome))
df1%>%group_by(age_group)%>%summarise(mean_income_age= mean(MonthlyIncome))

summarise(df1,mean(df2$MonthlyIncome))
df1$ID[is.na(df1$age_group)==TRUE]
head(df1)

##Boxplots
#NumberRealEstateLoansOrLines
ggplot(df1,mapping=aes(x=age_group,y=NumberRealEstateLoansOrLines))+geom_boxplot(binwidth=2)
ggplot(df1,mapping=aes(x=SeriousDlqin2yrs,y=NumberRealEstateLoansOrLines))+geom_boxplot(binwidth=2)+scale_y_continuous(limits=c(0,10))

#RevolvingUtilizationOfUnsecuredLines
ggplot(df1,mapping=aes(x=age_group,y=RevolvingUtilizationOfUnsecuredLines))+geom_boxplot()+scale_y_continuous(limits=c(0,10))
ggplot(df1,mapping=aes(x=SeriousDlqin2yrs,y=RevolvingUtilizationOfUnsecuredLines))+geom_boxplot()+scale_y_continuous(limits=c(0,10))

#DebtRatio
ggplot(df1,mapping=aes(x=age_group,y=DebtRatio))+geom_boxplot()+scale_y_continuous(limits=c(0,5))
ggplot(df1,mapping=aes(x=SeriousDlqin2yrs,y=DebtRatio))+geom_boxplot()+scale_y_continuous(limits=c(0,5))

#Monthly Income
ggplot(df1,mapping=aes(x=age_group,y=MonthlyIncome))+geom_boxplot()+scale_y_continuous(limits=c(0,5))
ggplot(df1,mapping=aes(x=SeriousDlqin2yrs,y=MonthlyIncome))+geom_boxplot()+scale_y_continuous(limits=c(0,5))



##histogram
#NumberOfTime30.59DaysPastDueNotWorse
ggplot(df1,mapping=aes(x=NumberOfTime30.59DaysPastDueNotWorse))+geom_histogram(bandwidth=1)+scale_y_continuous(limits=c(0,25))
unique(df1$NumberOfTime30.59DaysPastDueNotWorse)
#ggplot(df1,mapping=aes(x=age_group,y=NumberOfTime30.59DaysPastDueNotWorse))+geom_boxplot()+scale_y_continuous(limits=c(0,2))

#NumberOfTime60.89DaysPastDueNotWorse
ggplot(df1,mapping=aes(x=NumberOfTime60.89DaysPastDueNotWorse))+geom_histogram(bandwidth=1)+scale_y_continuous(limits=c(0,25))
unique(df1$NumberOfTime60.89DaysPastDueNotWorse)

#Number of dependents
ggplot(df1,aes(x=NumberOfDependents,fill=SeriousDlqin2yrs))+geom_histogram()

#age_group vs no.dependents
ggplot(df1,aes(x=NumberOfDependents))+geom_histogram()+facet_wrap(~age_group)
#facet_wrap(~SeriousDlqin2yrs)

ggplot(df1,mapping=aes(x=MonthlyIncome))+geom_histogram(binwidth = 100)

##scatterplot
ggplot(df1,mapping=aes(x=MonthlyIncome,y=DebtRatio))+geom_point()+scale_y_continuous(breaks=c(0,1000,100),limits = c(0,1000))+scale_x_continuous(breaks=c(0,1000,100),limit=c(0,1000))
ggplot(df1,mapping=aes(x=MonthlyIncome,y=RevolvingUtilizationOfUnsecuredLines))+geom_point()
ggplot(df1,mapping=aes(x= NumberOfOpenCreditLinesAndLoans,y=MonthlyIncome))+geom_point()+scale_y_continuous(limits=c(0,1000))
ggplot(df1,mapping=aes(x=age,y=MonthlyIncome))+geom_point()
ggplot(df1,mapping=aes(x=DebtRatio,y=RevolvingUtilizationOfUnsecuredLines))+geom_point()+scale_y_continuous(limits=c(0,1000))+scale_x_continuous(limits=c(0,1000))


#t-test
t.test(formula(MonthlyIncome~age_group),data=df1,subset=age_group%in%c('Middleage','Old'))
t.test(formula(MonthlyIncome~SeriousDlqin2yrs),data=df1)
t.test(formula(RevolvingUtilizationOfUnsecuredLines~SeriousDlqin2yrs),data=df1)
t.test(formula(NumberOfOpenCreditLinesAndLoans~SeriousDlqin2yrs),data=df1)
t.test(formula(DebtRatio~SeriousDlqin2yrs),data=df1)
t.test(formula(NumberOfTime30.59DaysPastDueNotWorse~SeriousDlqin2yrs),data=df1)
t.test(formula(NumberOfTime60.89DaysPastDueNotWorse~SeriousDlqin2yrs),data=df1)
t.test(formula(NumberRealEstateLoansOrLines~SeriousDlqin2yrs),data=df1)
t.test(formula(NumberOfDependents~SeriousDlqin2yrs),data=df1)
t.test(formula(NumberOfTimes90DaysLate~SeriousDlqin2yrs),data=df1)


#aov
aov1=aov(RevolvingUtilizationOfUnsecuredLines~SeriousDlqin2yrs,data=df1)
summary(aov1)
#significant
aov2=aov(DebtRatio~SeriousDlqin2yrs,data=df1)
summary(aov2)

aov3=aov(MonthlyIncome~SeriousDlqin2yrs,data=df1)
summary(aov3)



#2 way aov
twoway_aov1=aov(MonthlyIncome~SeriousDlqin2yrs*age_group,data=df1)
summary(twoway_aov1)


twoway_aov2=aov(DebtRatio~SeriousDlqin2yrs*age_group,data=df1)
summary(twoway_aov2)

twoway_aov3=aov(RevolvingUtilizationOfUnsecuredLines~SeriousDlqin2yrs*age_group,data=df1)
summary(twoway_aov3)

twoway_aov4=aov(NumberOfOpenCreditLinesAndLoans~SeriousDlqin2yrs*age_group,data=df1)
summary(twoway_aov4)

twoway_aov5=aov(NumberOfTimes90DaysLate~SeriousDlqin2yrs*age_group,data=df1)
summary(twoway_aov5)

twoway_aov6=aov(NumberOfTime30.59DaysPastDueNotWorse~SeriousDlqin2yrs*age_group,data=df1)
summary(twoway_aov6)

twoway_aov7=aov(NumberOfTime60.89DaysPastDueNotWorse~SeriousDlqin2yrs*age_group,data=df1)
summary(twoway_aov7)

twoway_aov8=aov(NumberOfDependents~SeriousDlqin2yrs*age_group,data=df1)
summary(twoway_aov8)


##Copying before splitting data:
df1$SeriousDlqin2yrs=as.factor(df1$SeriousDlqin2yrs)
write.csv(df1,'df2.csv',row.names = FALSE)
df2=read.csv('C:/Users/saluv/OneDrive/Desktop/DA/R/df2.csv')
str(df2)
anyNA(df2)
df2$ID=as.character(df2$ID)
df2$SeriousDlqin2yrs=as.factor(df2$SeriousDlqin2yrs)

#splitting data
index=createDataPartition(y=df2$SeriousDlqin2yrs,p=0.7,list=FALSE)
df2_train=df2[index,]
df2_test=df2[-index,]
trcl=trainControl(method='repeatedcv',number=10,repeats=3)


str(df2)
str(df2_train)
str(df2_test)

#knn
df2_knn=train(SeriousDlqin2yrs~+RevolvingUtilizationOfUnsecuredLines+age+ NumberOfTime30.59DaysPastDueNotWorse+DebtRatio+MonthlyIncome+ NumberOfOpenCreditLinesAndLoans+NumberOfTimes90DaysLate+
                NumberRealEstateLoansOrLines+NumberOfTime60.89DaysPastDueNotWorse+NumberOfDependents,data=df2_train,method='knn',preProcess = c("center", "scale"),trControl=trcl,tuneLength=10)
df2_knn
knn_test_df2=predict(df2_knn,newdata=df2_test)
knn_test_df2
confusionMatrix(knn_test_df2,df2_test$SeriousDlqin2yrs)

#decision tree
tree_fit1=train(SeriousDlqin2yrs~+RevolvingUtilizationOfUnsecuredLines+age+ NumberOfTime30.59DaysPastDueNotWorse+DebtRatio+MonthlyIncome+ NumberOfOpenCreditLinesAndLoans+NumberOfTimes90DaysLate+
                  NumberRealEstateLoansOrLines+NumberOfTime60.89DaysPastDueNotWorse+NumberOfDependents,data=df2_train,method='rpart',parms=list(split='gini'),trControl=trcl,tuneLength=10)
tree_fit1
tree_df2=predict(tree_fit1,newdata=df2_test)
confusionMatrix(tree_df2,df2_test$SeriousDlqin2yrs)

#random forest
forest_df2=randomForest(SeriousDlqin2yrs~.,data=df2_train)
forest_df2
forest_predict_df2=predict(forest_df2,newdata=df2_test)
confusionMatrix(forest_predict_df2,df2_test$SeriousDlqin2yrs)


#linear regression
# df2_ln=train(SeriousDlqin2yrs~.,data=df2_train,method='lm',parms=list(split='gini'),trControl=trcl,tuneLength=10)
# summary(ln)
# df2_ln$results[c('RMSE','Rsquared')]
# 
# df2_ln$predict=predict(df2_ln,newdata=df2_test)
# df2$residualerror=df2$SeriousDlqin2yrs-df2_ln$predict
# df2$resid_square=(df2$residualerror)^2
# df2_sum_residerror=sum(df2$resid_square)
# df2_predict_mse=(df2_sum_residerror)/length(df2$residerror)
# df2_predict_rmse=(df2_predict_mse)^0.5


#Logistic regression
str(df2)
df2_logistic=train(SeriousDlqin2yrs~+age_group,data=df2_train,method='glm',family='binomial',trControl=trcl,tuneLength=10)
summary(df2_logistic)

logistic_predict=predict(df2_logistic,newdata=df2_test)
confusionMatrix(logistic_predict,df2_test$SeriousDlqin2yrs)

#SVMlinear
df2_SVM=train(SeriousDlqin2yrs~+RevolvingUtilizationOfUnsecuredLines+age+ NumberOfTime30.59DaysPastDueNotWorse+DebtRatio+MonthlyIncome+ NumberOfOpenCreditLinesAndLoans+NumberOfTimes90DaysLate+
                NumberRealEstateLoansOrLines+NumberOfTime60.89DaysPastDueNotWorse+NumberOfDependents,data=df2_train,method='svmLinear',trControl=trcl,tuneLength=10)

df2_svm_predict=predict(df2_SVM,newdata=df2_test)
confusionMatrix(df2_svm_predict,df2$SeriousDlqin2yrs)

#SVMRadial
df2_SVM_radial=train(SeriousDlqin2yrs~+RevolvingUtilizationOfUnsecuredLines+age+ NumberOfTime30.59DaysPastDueNotWorse+DebtRatio+MonthlyIncome+ NumberOfOpenCreditLinesAndLoans+NumberOfTimes90DaysLate+
                NumberRealEstateLoansOrLines+NumberOfTime60.89DaysPastDueNotWorse+NumberOfDependents,data=df2_train,method='svmRadial',trControl=trcl,tuneLength=10)

df2_svmradial_predict=predict(df2_SVM_radial,newdata=df2_test)
confusionMatrix(df2_svmradial_predict,df2$SeriousDlqin2yrs)

#Factor analysis
corr=cor(df2,method='pearson')
parallel=fa.parallal(df2,fm='minres',fa='fa')

factors=fa(data=df2,nfactor=  ,rotation='varimax',fm='minres')
print(factors$loadings,cutoff=0.4)
f=factanal(df2,factors=,rotation='varimax',scores='regression')
df2=cbind(df2,f$scores)
f_df2=subset(df2,select=c('Factor1','Factor2','Factor3','Factor4'))
corr=cor(d_df2,method='pearson')







