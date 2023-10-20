# reading the data set
ClassifyRisk <- read.csv("C:/Users/Asus/Dropbox/My PC (DESKTOP-NN6U0SM)/Downloads/Documents/Data Mining/R/HW_CH7/ClassifyRisk.csv")

library(class)

ClassifyRisk_2 <- ClassifyRisk[,c(-1,-2,-6)]
record_1 <- c(22, 0, 46156.98)

# using Min-Max for income and age
range_age <- max(ClassifyRisk$age) - min(ClassifyRisk$age)
range_income <- max(ClassifyRisk$income) - min(ClassifyRisk$income)

ClassifyRisk_2$age <- (ClassifyRisk$age - min(ClassifyRisk$age))/range_age
ClassifyRisk_2$income <- (ClassifyRisk$income - min(ClassifyRisk$income))/range_income

# reclassifying the marital status variable
for (i in 1:length(ClassifyRisk_2$marital_status)) {
  if(ClassifyRisk_2$marital_status[i] == 'single') {ClassifyRisk_2$marital_status[i] = 0}
  else{ClassifyRisk_2$marital_status[i] = 1}
  }

# applying the transformation on record_1
record_1[1] <- (record_1[1] - min(ClassifyRisk$age))/range_age
record_1[3] <- (record_1[3] - min(ClassifyRisk$income))/range_income
record_1

true_class <- ClassifyRisk$risk

knn(ClassifyRisk_2, record_1, k = 2, cl = true_class, prob = TRUE)

