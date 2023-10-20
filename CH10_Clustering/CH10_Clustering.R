set.seed(12345)

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)

cereal <- read.csv("C:/cereal.csv")
# deleting unnecessary variables
cereal <- cereal[, -c(1,16)]

# applying Min-Max normalization and assigning indicator variables
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

minmaxFunc = function(x){
  x_star = (x - min(x))/(max(x) - min(x))
  return(x_star)
}

cereal$calories <- minmaxFunc(cereal$calories)
cereal$protein <- minmaxFunc(cereal$protein)
cereal$fat <- minmaxFunc(cereal$fat)
cereal$sodium <- minmaxFunc(cereal$sodium)
cereal$fiber <- minmaxFunc(cereal$fiber)
cereal$carbo <- minmaxFunc(cereal$carbo)
cereal$sugars <- minmaxFunc(cereal$sugars)
cereal$potass <- minmaxFunc(cereal$potass)
cereal$vitamins <- minmaxFunc(cereal$vitamins)
cereal$shelf <- minmaxFunc(cereal$shelf)
cereal$weight <- minmaxFunc(cereal$weight)
cereal$cups <- minmaxFunc(cereal$cups)

# delete extra variables (mfr & type)
cereal <- cereal[, -c(1,2)]

# Kmeans clustering
km_model <- kmeans(cereal, centers = 5)
km_model

# optimal number of clusters
fviz_nbclust(cereal, kmeans, nstart=100, method = "wss") + 
  geom_vline(xintercept = 5, linetype = 1)

