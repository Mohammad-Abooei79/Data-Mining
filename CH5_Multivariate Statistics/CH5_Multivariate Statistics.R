# reading the data set
cereal <- read.csv("C:/cereal.csv")

# Q11
rating <- cereal$rating
fiber <- cereal$fiber
model <- lm(rating~fiber)

# Q13
summary(model)
'yes it does make sense, since we have some cereals in our data set with 0 fiber'

# Q15
summary(model)$adj.r.squared
"adjusted R_squared = 0.3325"

# Q16
new_fiber <- data.frame(fiber=3)
predict(model, new_fiber)

# Q17
predict(model, new_fiber, interval = 'confidence', level = 0.95)

# Q18
predict(model, new_fiber, interval = 'predict', level = 0.95)

# Q19
summary(model)

library(ggplot2)
ggplot(data = cereal, aes(x = fiber, y = rating)) + 
  geom_point() + 
  stat_smooth(method = 'lm', se = TRUE)

# Q20
sugars <- cereal$sugars

model_2 <- lm(rating ~ fiber + sugars)
summary(model_2)

# Q21
summary(model_2)$coefficients

# Q22
summary(model)$adj.r.squared ; summary(model_2)$adj.r.squared

# Q23
sqrt(sum((model$residuals)^2)/75) ; sqrt(sum((model_2$residuals)^2)/75)







