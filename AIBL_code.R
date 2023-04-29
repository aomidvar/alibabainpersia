
x_testdata <- read.csv("c:/users/lenovo/desktop/test.csv")

x_traindata <- read.csv("c:/users/lenovo/desktop/train.csv")

install.packages("earth", lib="C:/Users/YourUser/Documents/R/win-library/3.3")

library(mlbench)
library(party)
library(tidyverse)
library(caret)
library(ranger)
library(e1071)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(readxl)
library(plotrix)
library(ggcorrplot)
library(dplyr)
library(GGally)
library(PerformanceAnalytics)
library(cowplot)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
library(randomForest)
library(gbm)
library(Metrics)
library(varImp)
library(vtreat)
library(AUC)
library(Boruta)
set.seed(123)

describe(x_traindata)
chart.Correlation(select(x_traindata, CDGLOBAL,LDELTOTAL, MMSCORE, MH8MUSCL, HMT7,HMT13,RCT20, RCT392, APGEN2, MH6HEPAT, BAT126, MH9ENDO), histogram = TRUE, main = "Correlation between Variables")

str(x_traindata)
str(x_testdata)
x_traindata$class <-factor(x_traindata$class)


boruta <- Boruta(x_traindata$class ~ ., data = x_traindata, doTrace = 2, maxRuns = 500)
boruta_signif <- names(boruta$finalDecision[boruta$finalDecision %in% c("Confirmed", "Tentative")])
print(boruta_signif)  # significant variables
plot(boruta, cex.axis=1.1, las=2, xlab="", main="Variable Importance")  # plot variable importance

library(earth)
marsModel <- earth(class ~ ., data=x_traindata) # build model
ev <- evimp (marsModel) # estimate variable importance
print(ev)

df = subset(x_traindata, select = c(CDGLOBAL,LDELTOTAL, MMSCORE, MH8MUSCL, HMT7,HMT13,RCT20, RCT392, APGEN2, MH6HEPAT, BAT126, MH9ENDO, class) )

 
rf_model <- train(
  class~.,
  tuneLength = 1,
  data = df, 
  method = "rf",
  trControl = trainControl(
    method = "cv", 
    number = 10, 
    verboseIter = TRUE))
  
nb_model<-naiveBayes(df,df$class)
nb_predict<-predict(nb_model,df,type="class")
view(nb_predict)

proc.time()


class.svm.model <- svm(class ~ ., data = df,cost=10, cross=10,type="C-classification",kernel="radial",na.action=na.omit)
proc.time()
svm_predict<-predict(class.svm.model,df,type="class")
view(svm_predict)

class.svm.model$tot.accuracy
table(class.svm.model$class, svm_predict$class)


rf_model
nb_model



AIBL.nb_model.prediction <-predict(nb_model, newdata = as.list(df, function(x) subset(df, is.na(df$class))                                                                                        ))

table(df$class, AIBL.nb_model.prediction)

AIBL.rf_model.prediction <-predict(rf_model, newdata = as.list(df, function(x) subset(df, is.na(df$class))                                                                                        ))



class.svm.model
 