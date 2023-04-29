data_raw <- read.csv("c:/users/lenovo/desktop/a1/creditworthiness.csv")
x_testdata <- read.csv("c:/users/lenovo/desktop/a1/creditworthiness-test.csv")
x_traindata <- read.csv("c:/users/lenovo/desktop/a1/creditworthiness-train.csv")

cw.train <- x_traindata[1:(nrow(x_traindata)/2),]
cw.test <- x_traindata[-(1:(nrow(x_traindata)/2)),]

install.packages("party", lib="C:/Users/YourUser/Documents/R/win-library/3.3")

library(party)

set.Seed (1234)
cw.train$credit.rating <-factor(cw.train$credit.rating)

cw.train<-select(cw.train, -functionary)
tree<-ctree(cw.train$credit.rating ~ ., data = cw.train, control = ctree_control(minbucket=2))
tree
plot(tree)
plot(tree, type="simple", inner_panel=node_inner(tree,abbreviate = FALSE, pval = TRUE, id = FALSE), terminal_panel=node_terminal(tree, abbreviate = FALSE, digits = 2, fill = c("white"),id = FALSE))

x_test_table1 <- read.csv("c:/users/lenovo/desktop/a1/test1.csv")
predict_table1<-predict(tree, x_test1)

view(predict_table1)

predict_model<-predict(tree, cw.test)
table(cw.train$credit.rating, predict_model)

idcol="functionary"
names(cw.train)[1] <- "functionary"

library(tidyverse)
str(data_raw)

library(caret)
library(ranger)


rf_model <- train(
  credit.rating~.,
  tuneLength = 1,
  data = cw.train, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)
proc.time()
predict_table_rf<-predict(rf_model, cw.test)
table(cw.train$credit.rating, predict_table_rf)
rf_model <- train(
  credit.rating~.,
  tuneLength = 5,
  data = cw.train, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 10, 
    verboseIter = TRUE
  )
)

rf_model <- train(
  credit.rating~.,
  tuneLength = 1,
  data = cw.train, 
  method = "rf",
  trControl = trainControl(
    method = "cv", 
    number = 10, 
    verboseIter = TRUE
  )
)

library(e1071)
svm_model<-svm(credit.rating~.,data=cw.train, gamma=0.1)

predict_table1_svm<-predict(svm_model, x_test1)
view(predict_table1_svm)
predict_table_svm<-predict(svm_model, cw.test)
table(cw.train$credit.rating, predict_table_svm)

#tune model
tune_out <- 
  tune.svm(credit.rating~.,data=cw.train, 
           cost = 10^(-1:2), 
           gamma = c(0.10, 1, 10), coef0 = c(0.1, 1, 10))

#list optimal values
tune_out$best.parameters$cost
tune_out$best.parameters$gamma
tune_out$best.parameters$coef0

svm_model<-svm(credit.rating~.,data=cw.train, gamma = 0.05,
               coef0 = 0, cost = 1)
predict_table_svm<-predict(svm_model,cw.test)
table(cw.train$credit.rating, predict_table_svm)


nb_model<-naiveBayes(cw.train,cw.train$credit.rating)
nb_predict<-predict(nb_default,cw.test,type="class")
view(nb_predict)
table(cw.test$credit.rating, nb_predict)

predict_table1_nb<-predict(nb_model, x_test1)
view(predict_table1_nb) 
print()

cw.train <- x_traindata[1:(nrow(x_traindata)/2),]
cw.test <- x_traindata[-(1:(nrow(x_traindata)/2)),]

cw.train$credit.rating[cw.train$credit.rating == 2] <- 0
cw.train$credit.rating[cw.train$credit.rating == 3] <- 0
cw.train$credit.rating
cw.test$credit.rating[cw.test$credit.rating == 2] <- 0
cw.test$credit.rating[cw.test$credit.rating == 3] <- 0

lr_model <- glm(credit.rating ~.,family=binomial(link='logit'),data=cw.train)
summary(lr_model)
svm_model_binary<-svm(credit.rating~.,data=cw.train, gamma=0.1,decision.values = TRUE, probability = TRUE)

cw.train$credit.rating <-factor(cw.train$credit.rating)
predict_table_lr_binary<-predict(lr_model, cw.test)
pred = predict(svm_model_binary,cw.train, decision.values = TRUE, probability = TRUE)
prob.versicolor <- attr(pred, "probabilities")[, "versicolor"]




library(e1071)
library(readxl)
library(caret)
  
class1.svm.model <- svm(credit.rating ~ ., data = cw.train,cost=10, cross=10,type="C-classification",kernel="radial",na.action=na.omit)
class1.svm.pred <- predict(class1.svm.model, cw.test)
finalmatrix<-data.matrix(pred, rownames.force = F)
  
library(dplyr)
library(pROC)
roc_svm_test <- roc(response = cw.train$credit.rating, predictor =as.numeric(pred))
  plot(roc_svm_test, add = TRUE,col = "red", print.auc=TRUE, print.auc.x = 0.5, print.auc.y = 0.3)
  legend(0.3, 0.2, legend = c("test-svm"), lty = c(1), col = c("blue"))

predict_table_svm_binary<-predict(svm_model_binary, cw.test,probability=TRUE)
table(cw.test$credit.rating, predict_table_svm_binary)
summary(svm_model_binary)

install.packages("ROCR", lib="C:/Users/YourUser/Documents/R/win-library/3.3")
library(ROCR)


pred_table_svm_binary = prediction(predict_table_svm_binary, cw.test$credit.rating)
perf_table_svm_binary = performance(predict_table_svm_binary, "acc")
plot(perf_table_svm_binary)

pred = prediction(as.numeric(pred), cw.test$credit.rating)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1) 

roc = performance(pred,"tpr","fpr")
