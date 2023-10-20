Churn <- read.csv("C:/Churn.csv", stringsAsFactors = TRUE)
library(rpart); library(rpart.plot); library(ggplot2)

# Finding the correlated variables
ggplot(Churn ,aes(Day.Charge, Day.Mins)) + geom_point()
ggplot(Churn ,aes(Night.Charge, Night.Mins)) + geom_point()
ggplot(Churn ,aes(Eve.Charge, Eve.Mins)) + geom_point()
ggplot(Churn ,aes(Intl.Charge, Intl.Mins)) + geom_point()

# Normalize(Min-Max) the numerical data
ZscoreFunc = function(x){
  x_star = (x - mean(x))/sd(x)
  return(x_star)
}

Churn$Account.Length2 <- ZscoreFunc(Churn$Account.Length)
Churn$VMail.Message2 <- ZscoreFunc(Churn$VMail.Message)
Churn$Day.Mins2 <- ZscoreFunc(Churn$Day.Mins)
Churn$Day.Calls2 <- ZscoreFunc(Churn$Day.Calls)
Churn$Eve.Mins2 <- ZscoreFunc(Churn$Eve.Mins)
Churn$Eve.Calls2 <- ZscoreFunc(Churn$Eve.Calls)
Churn$Night.Mins2 <- ZscoreFunc(Churn$Night.Mins)
Churn$Night.Calls2 <- ZscoreFunc(Churn$Night.Calls)
Churn$Intl.Mins2 <- ZscoreFunc(Churn$Intl.Mins)
Churn$Intl.Calls2 <- ZscoreFunc(Churn$Intl.Calls)
Churn$CustServ.Calls2 <- ZscoreFunc(Churn$CustServ.Calls)

# CART algorithm
churn_tree <- rpart(Churn. ~ Int.l.Plan + VMail.Plan + Account.Length2 + VMail.Message2 + 
                      Day.Mins2 + Day.Calls2 + Eve.Mins2 + Eve.Calls2 + Night.Mins2 +
                      Night.Calls2 + Intl.Mins2 + Intl.Calls2 + CustServ.Calls2, data = Churn, 
                    method = "class")
print(churn_tree)
rpart.plot(churn_tree)

# predict churn using the CART tree
prediction <- predict(churn_tree, type = "class")

conf_mat <- table(Churn$Churn., prediction)
dimnames(conf_mat) <- list(Actual = c('+', '-'), Predicted = c('+', '-'))
conf_mat

(Fpp <- conf_mat[2,1]/(conf_mat[2,1] + conf_mat[1,1]))
(Fnp <- conf_mat[1,2]/(conf_mat[1,2] + conf_mat[2,2]))
(OER <- (conf_mat[1,2] + conf_mat[2,1])/sum(conf_mat[,]))
(Accuracy <- 1 - OER)
(Sensitivity <- conf_mat[1,1]/(conf_mat[1,1] + conf_mat[1,2]))
(Specifity <- conf_mat[2,2]/(conf_mat[2,2] + conf_mat[2,1]))

