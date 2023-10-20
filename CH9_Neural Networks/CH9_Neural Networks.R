set.seed(123456)
library(neuralnet); library(ggplot2)
Churn <- read.csv("C:/Users/ASUS/Desktop/CH9_Neural Networks/Churn.csv", stringsAsFactors = TRUE)
Churn <- Churn[1:500,]           # Comment this line if computation time is not important

# Finding the correlated variables
ggplot(Churn ,aes(Day.Charge, Day.Mins)) + geom_point()
ggplot(Churn ,aes(Night.Charge, Night.Mins)) + geom_point()
ggplot(Churn ,aes(Eve.Charge, Eve.Mins)) + geom_point()
ggplot(Churn ,aes(Intl.Charge, Intl.Mins)) + geom_point()

# Normalize(Min-Max) the numerical data
minmaxFunc = function(x){
  x_star = (x - min(x))/(max(x) - min(x))
  return(x_star)
}

Churn$Account.Length2 <- minmaxFunc(Churn$Account.Length)
Churn$VMail.Message2 <- minmaxFunc(Churn$VMail.Message)
Churn$Day.Mins2 <- minmaxFunc(Churn$Day.Mins)
Churn$Day.Calls2 <- minmaxFunc(Churn$Day.Calls)
Churn$Eve.Mins2 <- minmaxFunc(Churn$Eve.Mins)
Churn$Eve.Calls2 <- minmaxFunc(Churn$Eve.Calls)
Churn$Night.Mins2 <- minmaxFunc(Churn$Night.Mins)
Churn$Night.Calls2 <- minmaxFunc(Churn$Night.Calls)
Churn$Intl.Mins2 <- minmaxFunc(Churn$Intl.Mins)
Churn$Intl.Calls2 <- minmaxFunc(Churn$Intl.Calls)
Churn$CustServ.Calls2 <- minmaxFunc(Churn$CustServ.Calls)

# indicator variables
Churn$Int.l.Plan2 <- Churn$VMail.Plan2 <- Churn$Churn.2 <- c(rep(0, length(Churn$Churn.)))

for (i in 1:length(Churn$Churn.2)) {
  if(Churn$Int.l.Plan[i] == "yes") Churn$Int.l.Plan2[i] <- 1
  if(Churn$VMail.Plan[i] == "yes") Churn$VMail.Plan2[i] <- 1
  if(Churn$Churn.[i] == "True.") Churn$Churn.2[i] <- 1
}

# delete the extra variables
Churn <- Churn[, -c(1:22)]

# run the neural network
churnnet <- neuralnet(formula = Churn.2 ~ Account.Length2 + VMail.Message2 + Day.Mins2 
                      + Day.Calls2 + Eve.Mins2 + Eve.Calls2 + Night.Mins2 + Night.Calls2
                      + Intl.Mins2 + Intl.Calls2 + CustServ.Calls2 + Int.l.Plan2 
                      + VMail.Plan2, data = Churn, rep = 10, hidden = 6, linear.output = FALSE)

print(churnnet)
plot(churnnet, show.weights = FALSE)
churnnet$weights


