#Knn Education,Age,Pay
datknn<-dat4[,c(3,4,10)]
indxTrain <- createDataPartition(y = datknn$Pay,p = 0.75,list = FALSE)
training <- datknn[indxTrain,]
testing <- datknn[-indxTrain,]

prop.table(table(training$Pay)) * 100
prop.table(table(testing$Pay)) * 100
prop.table(table(datknn$Pay)) * 100

trainX <- training[,names(training) != "Pay"]
preProcValues <- preProcess(x = trainX,method = c("range"))
preProcValues

ctrl <- trainControl(method="cv") #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(Pay ~ ., data = training, method = "knn",
                na.action = 'na.omit',
                trControl = ctrl, preProcess = c("range"), tuneLength = 20)

#Output of kNN fit
knnFit
plot(knnFit)


knnpredict<-predict(knnFit,newdata=testing)
confusionMatrix(knnpredict,testing$Pay)

plot(knnpredict)

knnFit$finalModel

#neural net
ctrl <- trainControl(method="cv") #,classProbs=TRUE,summaryFunction = twoClassSummary)
nnfit <- train(Pay ~ ., data = training, method = "nnet",
               na.action = 'na.omit',
               trControl = ctrl, trace= FALSE,tuneLength=5)

nnfit

nnpredict<-predict(nnfit,newdata=testing)
confusionMatrix(nnpredict,testing$Pay)

nnfit$finalModel
plot(nnpredict)
plot(nnfit)

#Compare:
resamps <- resamples(list(
  KNN=knnFit,
  NeuralNet=nnfit))
resamps

summary(resamps)

plot(resamps)


