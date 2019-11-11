##### IMPORTING THE DATASET #####

loans= read.csv(choose.files())
summary(loans)
str(loans)


##### IMPUTING THE MISSING VALUES #####

library(Hmisc)

loans$log.annual.inc = impute(loans$log.annual.inc, mean)
loans$pub.rec= impute(loans$pub.rec, mean)
loans$days.with.cr.line= impute(loans$days.with.cr.line, mean)
loans$inq.last.6mths = impute(loans$inq.last.6mths,mean)
loans$revol.util= impute(loans$revol.util, mean)
loans$delinq.2yrs = impute(loans$delinq.2yrs, mean)
loans= loans[,-2]


##### MODEL #####

library(car)
model1= glm(not.fully.paid ~ ., family = binomial(link= "logit"), data = loans)
summary(model1)
vif(model1)

##### DROPING THE UNSIGNIFICANT VARIABLES #####

loans= loans[-c(5,7,9,11)]

 
##### SPLITING THE DATA INTO TRAIN AND TEST #####
set.seed(0)
dt = sort(sample(nrow(loans), nrow(loans)*.75))
train =loans[dt,]
test =loans[-dt,]
model2=glm(not.fully.paid~.,family=binomial(link='logit'),data=train)
summary(model2)
vif(model2)


##### FIND THE ACCURACY OF THE MODEL #####
testpredict = predict(model2,newdata=test,type='response')
fitted.results = ifelse(testpredict > 0.5,1,0) ##thrushold-0.5,if it is greater than 0.5 then it is said to be 1 otherwise 0
misClasificError= mean(fitted.results != test$not.fully.paid)
print(paste('Accuracy',1-misClasificError))

##### CONFUSION MATRIX #####

table(test$not.fully.paid,testpredict>0.5)

predcition = write.csv(fitted.results,"loanspred.csv")

##### END #####