cereal <- read.csv("C:/cereal.csv")

# replacing missing values (-1) with NAs
for (i in 1:length(cereal[1,])){
  for(j in 1:length(cereal[,i])) {
    if(cereal[j,i] == -1) cereal[j,i] <- NA
  }
}

# Create the dummy variables
unique(cereal$mfr) # six indicators for mfr
unique(cereal$type) # one indicator for type

cereal$mfr_N <- cereal$mfr_Q <- cereal$mfr_K <- cereal$mfr_R <- cereal$mfr_G <- 
  cereal$mfr_P <- cereal$type_C <- c(rep(0, length(cereal$cups)))

for(i in 1:length(cereal$cups)) {
  if(cereal$mfr[i] == 'N') cereal$mfr_N[i] <- 1
  if(cereal$mfr[i] == 'Q') cereal$mfr_Q[i] <- 1
  if(cereal$mfr[i] == 'K') cereal$mfr_K[i] <- 1
  if(cereal$mfr[i] == 'R') cereal$mfr_R[i] <- 1
  if(cereal$mfr[i] == 'G') cereal$mfr_G[i] <- 1
  if(cereal$mfr[i] == 'P') cereal$mfr_P[i] <- 1
  if(cereal$type[i] == 'C') cereal$type_C <- 1
}

# deleting NAs in the duplicated dataset
cereal_na <- na.omit(cereal)

# regression model for sugars
model_1 <- lm(sugars ~ calories + protein + fat + sodium + fiber + potass 
              + vitamins + shelf + weight + cups + type_C + mfr_P + mfr_R + mfr_G 
              + mfr_K + mfr_Q + mfr_N, data = cereal_na) 
# since Quaker Oatmeal value for carbo is missing, we cannot use it

summary(model_1)
step_1 <- step(model_1, direction = "both")
summary(step_1)  

model_2 <- lm(formula = sugars ~ calories + protein + sodium + potass + shelf + 
                mfr_P + mfr_G + mfr_K, data = cereal_na)
summary(model_2)  # all variables are significant

# imputing sugars value for Quaker Oatmeal
predict(model_2, newdata = cereal[58,])

# inserting this value in the main dataset
cereal[58, 10] <- predict(model_2, newdata = cereal[58,])

# regression model for carbo
carbo_1 <- lm(carbo ~ calories + protein + fat + sodium + fiber + sugars + potass 
              + vitamins + shelf + weight + cups + type_C + mfr_P + mfr_R + mfr_G 
              + mfr_K + mfr_Q + mfr_N, data = cereal_na)
summary(carbo_1)
step_2 <- step(carbo_1, direction = "both")
summary(step_2)

carbo_2 <- lm(formula = carbo ~ calories + protein + fat + fiber + sugars + weight 
              + cups + mfr_R, data = cereal_na)
summary(carbo_2) # all variables are significant

# imputing and inserting the carbo value for Quaker Oatmeal
predict(carbo_2, newdata = cereal[58,])
cereal[58, 9] <- predict(carbo_2, newdata = cereal[58,])

  
  
