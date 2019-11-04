##### IMPORTING THE DATASET #####

dataset= read.csv("baseball.csv")


##### SUBSETTING THE DATA IN TRAIN AND TEST #####

traindata= subset(dataset, Year<=2007)
testdata= subset(dataset, Year>2007)

##### FINDING THE MISSING VALUES #####

is.na(traindata)

##### DROPING THE UNSIGNIFICANT COLUMN#####

train= subset(traindata, select = -c(RankSeason,RankPlayoffs))

test= subset(testdata, select = -c(RankSeason,RankPlayoffs))

##### MODEL TRAINING #####

trainmodel= lm(W ~ ., data = train)
summary(trainmodel)

##### FEATURE SELECTION #####
trainmodel1= lm(W ~ Year+RS+RA+OBP+SLG+BA+Playoffs+G+OOBP+OSLG, data = train)
summary(trainmodel1)

##### FINAL MODEL #####
trainmodel2= lm(W ~ RS+RA+OBP+SLG+Playoffs+OOBP, data = train)
summary(trainmodel2)

##### TEST DATA #####
testfinal= subset(test, select = -c(Year,League,Team,BA,G,OSLG))

##### PREDICTION #####
finalpredict = predict(trainmodel2, newdata= testfinal)
summary(finalpredict)

##### ACCURACY OF MODEL #####

ssetest= sum((finalpredict- testfinal$W)^2)
ssttest= sum((mean(testfinal$W)- testfinal$W)^2)
r2= 1-(ssetest/ssttest)


##### END #####






