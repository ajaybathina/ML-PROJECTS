##### IMPORTING DATASET #####


Polling=read.csv(file.choose())

##### REPLACING THE MISSING VALUES #####
library(Hmisc)
Polling$Rasmussen=impute(Polling$Rasmussen,mean)
Polling$SurveyUSA=impute(Polling$SurveyUSA,mean)
poll=Polling[,-1]

##### CALCULATE VIF #####
library(car)
model1=glm(Republican~.,family=binomial(link='logit'),data=poll)
summary(model1)
vif(model1)

##### DELETING UNSIGNIFICANT VARIABLES #####
poll=poll[,-5]

##### SPLITING THE DATA INTO TRAIN AND TEST #####
set.seed(0)
dt = sort(sample(nrow(poll), nrow(poll)*.75))
train =poll[dt,]
test =poll[-dt,]
model2=glm(Republican~.,family=binomial(link='logit'),data=train)
summary(model2)

vif(model2)

##### FIND THE ACCURACY OF THE MODEL #####
testpredict = predict(model2,newdata=subset(test,select=c(1,2,3,4,5)),type='response')
fitted.results = ifelse(testpredict > 0.5,1,0) ##thrushold-0.5,if it is greater than 0.5 then it is said to be 1 otherwise 0
misClasificError= mean(fitted.results != test$Republican)
print(paste('Accuracy',1-misClasificError))

##### CONFUSION MATRIX #####

table(test$Republican,testpredict>0.5)

predcition = write.csv(fitted.results,"Y.csv")



##### END #####


