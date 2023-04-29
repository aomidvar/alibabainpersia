data_raw <- read.csv("c:/users/lenovo/desktop/a1/creditworthiness1.csv")

dataset <- as.matrix(data_raw[, c(2,3,5,7,8,46)])

training<-subset(dataset, !is.na(dataset$credit.rating))

ratings<-as.factor(dataset[, c(6)])
credit.supersom <- supersom(lapply(training,function(x) subset(x, !0)), somgrid(20, 20, "hexagonal"))

credit.som.prediction <-predict(credit.supersom, newdata = as.list(training, function(x) subset(training, is.na(dataset$credit.rating))))

testing<-subset(dataset, is.na(dataset$credit.rating))
test = subset(testing, select = -c(6) )

credit.som.test.prediction <-predict(credit.supersom, newdata = as.list(test, function(x) subset(testing, !is.na(dataset$credit.rating)))) 

credit.som.prediction <-predict(credit.supersom, newdata = as.list(training, function(x) subset(training, is.na(dataset$credit.rating))))
table(training$credit.rating, credit.som.prediction$prediction$credit.rating) 


neural_net = neuralnet(training$credit.rating ~ re.balanced..paid.back..a.recently.overdrawn.current.acount  
                       +                         + FI3O.credit.score+X0..accounts.at.other.banks+savings.on.other.accounts, data = training, 
                       +                         hidden = 1, linear.output = TRUE) 

plot(neural_net)
                       