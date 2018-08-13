setwd("~/Fall'17/Data mining/Project 2/clean_proj1")
getwd()
load("Status_Non_DoD_2004_03b.rda") 
summary(dat6)
dat2 <- dat6
summary(dat2)
dat2$PseudoID <- NULL
dat2$Name <- NULL
dat2$Date <- NULL
dat2$Education <- as.numeric(as.character(dat2$Education))
dat2$Age <- as.numeric(substr(dat2$Age, start = 1, stop = 2))
dat2$LOS <- as.numeric(sub("-.*|\\+|<", "", dat2$LOS))
dat2$Pay <- cut(dat2$Pay,
                breaks = c(0, 50000, 75000, 100000 ,Inf),
                labels = c("<50k", "50-75k", "75k-100k", ">100k"))
summary(dat2)
dat2$StationID <- as.numeric(as.character(dat2$StationID))
summary(dat3)
dattry <- summary(dat2$Agency)
head(dat2$LOS)
  
head(dattry)
dat2$type <- cut(dattry,
                breaks = c(10000, 20000,30000,Inf),
                labels = c("small", "medium", "large"))

summary(type)
summary(dat2$Pay)
summary(dat2)
agencies <- names(which(table(dat2$Agency)>10000))
agencies
dat3 <- dat2[dat2$Agency %in% agencies, ]

#Add state name
state_trans <- read.csv('State Translations.txt',header = TRUE)
state_ID <- sapply(state_trans, FUN = function(x) substring(x, 1,2))
state_name <- trimws(sapply(state_trans, FUN = function(x) substring(x, 35,90)))
state_trans_table <- data.frame(state_ID = state_ID, state_name = state_name)
dat2$StationName <- sapply(dat2$Station, FUN = function(x)
  as.integer(substring(x, 1,2)))
names(state_trans_table) <- c("state_ID", "state_name")

m <- match(dat2$StationName, state_trans_table$state_ID)
dat2$States <-  state_trans_table$state_name[m]
summary(dat2)
#Add agency
m <- match(dat$Agency, agency_trans_table$agency_ID)
dat$AgencyName <-  agency_trans_table$agency_name[m]
# get rid of unused agency levels
dat3$AgencyName <- factor(dat3$Agency)
dat3$Agency <- factor(dat3$Agency)

dim(dat3)
summary(dat3)
sample_ID <- sample(1:nrow(dat3), size = 10000)
dat42 <- dat3[sample_ID, ]
summary(dat41)
#dat4 sample

library(rpart)
#predict pay
modelSE <- rpart(Pay ~ Education + LOS + StationID, data = dat41)
modelSE
rpart.plot(modelSE,extra = 2, under = TRUE, varlen=0, faclen=0)

modelEF <-rpart(Pay ~ Education + Fulltime, data= dat41)
modelEF
rpart.plot(modelEF, extra = 2, under = TRUE, varlen=0, faclen=0)

model <- rpart(Pay ~ Age + Education + LOS, data = dat41)
rpart.plot(model, extra = 2, under = TRUE, varlen=0, faclen=0)

modelES <- rpart(Pay ~ Education + LOS + StationID , data = dat41)
summary(dat41)
library("rpart.plot")
rpart.plot(model, extra = 2, under = TRUE, varlen=0, faclen=0)


model <- rpart(Pay ~ Age + Education + LOS + Agency, data = dat4)
model
rpart.plot(modelSE, extra = 2, under = TRUE, varlen=0,
           faclen=0)

mm <- model.matrix(~Agency, dat4)
dat5 <- cbind(dat4[, c("Pay", "Age", "Education", "LOS")], mm[,-1])
summary(dat5)
model <- rpart(Pay ~ ., data = dat5)
model
rpart.plot(model, extra = 2, under = TRUE, varlen=0,
           faclen=0)

library(caret)
#train model
#Pay ~ Education + Fulltime
fit1 <- train(Pay ~ Education   +LOS + StationID, data = dat42 , method = "rpart",
             na.action = na.pass,
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=10)
fit
rpart.plot(fit1$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0,cex = 0.8)
summary(dat41)
varImp(fit1)
testing <- dat3[-sample_ID, ]
testing <- testing[sample(1:nrow(testing), size = 1000), ]
pred <- predict(fit, newdata = testing, na.action = na.pass)
head(pred)
confusionMatrix(data = pred, testing$Pay)

#predict education
fit <- train(LOS ~ Pay + Age + Education, data = dat41 , method = "rpart",
             na.action = 'na.omit',
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=10)
fit
rpart.plot(fit$finalModel, under = TRUE, varlen=0, faclen=0)

varImp(fit)
testing <- dat3[-sample_ID, ]
testing <- testing[sample(1:nrow(testing), size = 1000), ]
pred <- predict(fit, newdata = testing, na.action = na.pass)
head(pred)
confusionMatrix(data = pred, testing$LOS)

str(pred)
str(testing$LOS)
table(factor(pred, levels=min(testing):max(testing)), 
      factor(testing, levels=min(testing):max(testing)))
table(pred)
table(testing$LOS)
Education ~ Pay + LOS + StationID

#divide by state
#texas
datcali<- dat3[dat3$StationID =='06',]
summary(datcali)
datdc<- dat3[dat3$StationID =='11',]
summary(datdc)
dattexas<- dat3[dat3$StationID == '48',]
datcali <- subset(datcali, StationID != '0', StationID != 'NA')
dattexas <- subset(dattexas, StationID != '0', StationID != 'NA')
datdc<- subset(datdc, StationID != '0', StationID != 'NA')
summary(dattexas)
summary(datdc)
head(dat41$Age)
#Fr bush texas and dc

#graphs for cali and texas
#sample
sample_ID <- sample(1:nrow(dattexas), size = 10000)
dattxsam <- dattexas[sample_ID, ]
summary(datdcsam)

fit <- train(Pay ~ Education + LOS + Age, data = dattxsam , method = "rpart",
             na.action = na.pass,
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=10)
fit
rpart.plot(fit$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0)

varImp(fit)
testing <- dattexas[-sample_ID, ]
testing <- testing[sample(1:nrow(testing), size = 1000), ]
pred <- predict(fit, newdata = testing, na.action = na.pass)
head(pred)
confusionMatrix(data = pred, testing$Pay)

#Cali #DC
sample_ID <- sample(1:nrow(datdc), size = 10000)
datdcsam <- datdc[sample_ID, ]
summary(datdcsam)

fit <- train(Pay ~ Education + LOS + Age, data = datdcsam , method = "rpart",
             na.action = na.pass,
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=10)
fit
rpart.plot(fit$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0)

varImp(fit)
testing <- datdc[-sample_ID, ]
testing <- testing[sample(1:nrow(testing), size = 1000), ]
pred <- predict(fit, newdata = testing, na.action = na.pass)
head(pred)
confusionMatrix(data = pred, testing$Pay)

#Agencywise
#VATA
summary(dat3)
datvata<-dat3[dat3$Agency == 'VATA',]
sample_ID <- sample(1:nrow(datvata), size = 10000)
datvatasam <- datvata[sample_ID, ]
summary(datvatasam)

fit <- train(Pay ~ Education + LOS + Age, data = datvatasam , method = "rpart",
             na.action = na.pass,
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=10)
fit
rpart.plot(fit$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0)

varImp(fit)
testing <- datvata[-sample_ID, ]
testing <- testing[sample(1:nrow(testing), size = 1000), ]
pred <- predict(fit, newdata = testing, na.action = na.pass)
head(pred)
confusionMatrix(data = pred, testing$Pay)

#IRS
summary(dat3)
datirs<-dat3[dat3$AgencyName == 'INTERNAL REVENUE SERVICE',]
summary(datirs)
sample_ID <- sample(1:nrow(datirs), size = 10000)
datirssam <- datirs[sample_ID, ]
summary(datdcsam)

fit <- train(Pay ~ Education + LOS + Age, data = datirssam , method = "rpart",
             na.action = na.pass,
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=10)
fit
rpart.plot(fit$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0)

varImp(fit)
testing <- datirs[-sample_ID, ]
testing <- testing[sample(1:nrow(testing), size = 1000), ]
pred <- predict(fit, newdata = testing, na.action = na.pass)
head(pred)
confusionMatrix(data = pred, testing$Pay)


#library foor chi test
library(FSelector)

summary(dat3)
nrow(dat3)
#drop na'
dat11 <- subset(dat3, Pay != 'NA' & Pay != '0'
               & Age != 'NA'
               & Education != 'NA'
               & PayPlan != 'NA'
               & Schedule != 'NA' & Station !='NA'& LOS != 'NA' & Category != 'NA'
               & SupervisoryStatus != 'NA' & Appointment != 'NA' & StationID != 'NA')
summary(dat11)
#column add manager, non manager
dat3$Manager <- FALSE
dat3$Manager[dat3$SupervisoryStatus %in% c("2","4","5")] <- "Manager"
dat3$Manager[dat3$SupervisoryStatus %in% c("6","7","8")] <- "Non manager"
#change column name
colnames(dat3)[which(names(dat3) == "Manager")] <- "StatusName"
dat4<- dat3
summary(dat4)
nrow(dat2)
#Column bachelors, non bachelors


#concern area categories
dat4$AgencyType[dat4$AgencyName %in% c("VETERANS HEALTH ADMINISTRATION")] <- "HEALTH"
dat4$AgencyType[dat4$AgencyName[grep("HEALTH",dat4$AgencyName)]] <- "HEALTH"
dat4$AgencyType[dat4$AgencyName %in% c("INTERNAL REVENUE SERVICE")] <- "REVENUE"
dat4$AgencyType[dat4$AgencyName %in% c("SOCIAL SECURITY ADMINISTRATION","TRANSPORT SECURITY ADMINISTRATION")] <- "SECURITY"
dat4$AgencyType[dat4$AgencyName %in% c("CITIZENSHIP AND IMMIGRATION SERVICES")] <- "IMMIGRATION"
dat4$AgencyType[dat4$AgencyName %in% c("ENVIRONMENTAL PROTECTION AGENCY")] <- "ENIVRONMENT"
dat4$AgencyType[is.na(dat4$AgencyType)] <- "OTHERS"
summary(dat4)
head(dat4)

#transformation rates
nrow(dat4)
head(sort(dat4$Pay, decreasing=TRUE),n =5000)
head(sort(dat4$Education, decreasing=TRUE),n =1000)
tail(sort(dat4$Education, decreasing=TRUE),n=100)

#KN model


datb<-dat4[,c(3,4,10)]
indxTrain <- createDataPartition(y = datb$Pay,p = 0.75,list = FALSE)
training <- datb[indxTrain,]
testing <- datb[-indxTrain,]

prop.table(table(training$Pay)) * 100
prop.table(table(testing$Pay)) * 100
prop.table(table(datb$Pay)) * 100

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

#PART
ctrl <- trainControl(method="cv") #,classProbs=TRUE,summaryFunction = twoClassSummary)
PARTFit <- train(Pay ~ ., data = training, method = "PART",
                 na.action = 'na.omit',
                 trControl = ctrl, preProcess = c("range"), tuneLength = 20)

PARTFit

PARTpredict<-predict(PARTFit,newdata=testing)
confusionMatrix(PARTpredict,testing$Pay)

plot(PARTFit)

plot(PARTpredict)

PARTFit$finalModel


ctrl <- trainControl(method="cv") #,classProbs=TRUE,summaryFunction = twoClassSummary)

# Random forrest
rfFit <- train(Pay ~ ., data = training, method = "rf", trControl = ctrl,
               na.action='na.omit',
               preProcess = c("range"), tuneLength = 20)
rfFit

rfpredict<-predict(rfFit,newdata=testing)
confusionMatrix(rfpredict,testing$Pay)


rfFit$finalModel

plot(rfpredict)

#Compare:
resamps <- resamples(list(
  KNN=knnFit,
  NeuralNet=nnfit,
  PART=PARTFit,
  RandomForest=rfFit))
resamps

summary(resamps)

plot(resamps)


#Chi test
#SupervisoryStatus ~ Education + LOS + Pay
tb1<-table(Education=dat4$Education,dat4$Pay)
tb1
chisq.test(tb1)
aggregate(Education ~ Pay, data=dat3, FUN = mean)

randomforestfit <- train( Education ~ LOS, method = "rf", data = dat41, tuneLength = 5,
                         trControl = trainControl(method = "cv", indexOut = train))

dat41.rf<-randomForest(Education ~ LOS, data=dat41, importance=TRUE, proximity= TRUE)
sapply(dat41,class)
mm <- model.matrix(~Agency+~catergory+~size, dat4)
dat5 <- cbind(dat4[, c("Pay", "Age", "Education", "LOS")], mm[,-1])

install("randomForest")
summary(dat41)
