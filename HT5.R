setwd("C:/Users/Zephyrus/Documents/U/7mo Semestre/Mineria de Datos/HT5-Mineria")
library(caret)
library(rpart)
library(e1071)
library(rpart.plot)

houses = read.csv('train.csv')
houses[is.na(houses)]<-0
houses$Id<-NULL


houses$clasification <- ifelse(houses$SalePrice > 290000, "Caras", ifelse(houses$SalePrice>170000, "Intermedia", "Economicas"))
houses$Condition2 <- NULL
houses$Exterior1st <- NULL
houses$RoofStyle <- NULL
houses$ExterCond <- NULL
houses$RoofMatl <- NULL
houses$Electrical <- NULL
houses$Heating <- NULL
houses$HeatingQC <- NULL
houses$MiscFeature <- NULL
houses$grupokm <- NULL
houses$Exterior2nd <- NULL
houses$SaleType <- NULL
houses$SalePrice<-NULL

porciento <- 70/100
set.seed(1234)

economicas<-houses[houses$clasification=="Economicas",]
intermedias<-houses[houses$clasification=="Intermedia",]
caras<-houses[houses$clasification=="Caras",]


numFilasTrainEcon<-sample(nrow(economicas), porciento*nrow(economicas))
trainEcon<-economicas[numFilasTrainEcon,]

numFilasTrainInter<-sample(nrow(intermedias), porciento*nrow(intermedias))
trainInter<-intermedias[numFilasTrainInter,]

numFilasTrainCaras<-sample(nrow(caras), porciento*nrow(caras))
trainCaras<-caras[numFilasTrainCaras,]


training<-rbind(trainInter, trainEcon, trainCaras)
test<-houses[setdiff(rownames(houses),rownames(training)),]

modelo<-naiveBayes(as.factor(clasification)~., data=training)

predTrain<-predict(modelo, newdata = training[,1:68])
training$prediccion <- predTrain
cf<-confusionMatrix(table(training$clasification, training$prediccion))
cf

predBayes<-predict(modelo, newdata = test[,1:68])
test$prediccion <- predBayes
cm<-confusionMatrix(table(test$clasification, test$prediccion))
cm

#Validacion cruzada

ct<-trainControl(method = "cv",training[,1:68],number=10, verboseIter=T)
modeloCaret<-train(clasification~.,data=training,method="nb",trControl = ct)
prediccionCaret<-predict(modeloCaret,newdata = test[,1:68])
test$prediccion <- prediccionCaret
cr<-confusionMatrix(table(test$prediccion,test$clasification))
cr
