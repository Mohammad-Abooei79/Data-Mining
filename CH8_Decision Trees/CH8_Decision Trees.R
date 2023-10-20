library("rpart"); library("rpart.plot"); library("C50"); library('ggplot2')

Churn <- read.csv("C:/Users/Asus/Dropbox/My PC (DESKTOP-NN6U0SM)/Downloads/Documents/Data Mining/R/HW_CH8/Churn.csv",
                  stringsAsFactors = TRUE)

# Finding the correlated variables

ggplot(Churn ,aes(Day.Charge, Day.Mins)) + geom_point()
ggplot(Churn ,aes(Night.Charge, Night.Mins)) + geom_point()
ggplot(Churn ,aes(Eve.Charge, Eve.Mins)) + geom_point()
ggplot(Churn ,aes(Intl.Charge, Intl.Mins)) + geom_point()

# Standardize numeric variables

Churn$Account.Length.z <- (Churn$Account.Length - mean(Churn$Account.Length))/sd(Churn$Account.Length)
Churn$VMail.Message.z <- (Churn$VMail.Message - mean(Churn$VMail.Message))/sd(Churn$VMail.Message)
Churn$Day.Mins.z <- (Churn$Day.Mins - mean(Churn$Day.Mins))/sd(Churn$Day.Mins)
Churn$Day.Calls.z <- (Churn$Day.Calls - mean(Churn$Day.Calls))/sd(Churn$Day.Calls)
Churn$Eve.Mins.z <- (Churn$Eve.Mins - mean(Churn$Eve.Mins))/sd(Churn$Eve.Mins)
Churn$Eve.Calls.z <- (Churn$Eve.Calls - mean(Churn$Eve.Calls))/sd(Churn$Eve.Calls)
Churn$Night.Mins.z <- (Churn$Night.Mins - mean(Churn$Night.Mins))/sd(Churn$Night.Mins)
Churn$Night.Calls.z <- (Churn$Night.Calls - mean(Churn$Night.Calls))/sd(Churn$Night.Calls)
Churn$Intl.Mins.z <- (Churn$Intl.Mins - mean(Churn$Intl.Mins))/sd(Churn$Intl.Mins)
Churn$Intl.Calls.z <- (Churn$Intl.Calls - mean(Churn$Intl.Calls))/sd(Churn$Intl.Calls)
Churn$CustServ.Calls.z <- (Churn$CustServ.Calls - mean(Churn$CustServ.Calls))/sd(Churn$CustServ.Calls)

# Q11
# CART decision tree

cart_tree <- rpart(Churn. ~ Account.Length.z + VMail.Message.z + Day.Mins.z + Day.Calls.z+
                     Eve.Mins.z + Eve.Calls.z + Night.Mins.z + Night.Calls.z + Intl.Mins.z +
                     Intl.Calls.z + CustServ.Calls.z + Int.l.Plan + VMail.Plan, data = Churn,
                   method = "class")
print(cart_tree)
rpart.plot(cart_tree)

# Q12
# C4.5 decision tree

names(Churn)
predictors <- Churn[,c(6, 7, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33)]
target <- Churn$Churn.

C4.5_tree <- C5.0(predictors, target)
summary(C4.5_tree)
plot(C4.5_tree)
