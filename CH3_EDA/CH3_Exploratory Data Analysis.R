"Stu Name: Mohammad Abooei Mehrizi"
"Stu Number: 810998078"

adult <- read.csv("C:/Users/Asus/Dropbox/My PC (DESKTOP-NN6U0SM)/Downloads/Documents/Data Mining/R/HW_CH3/adult.csv")
library(ggplot2)

# Q22

summary(adult)
str(adult)

# Q25

(t1 = table(adult$income, adult$education))
(p1 = round(prop.table(t1, margin = 2), 3)*100)
barplot(p1, col = c('lightblue','red'))      # yes

(t2 = table(adult$income, adult$workclass))
(p2 = round(prop.table(t2, margin = 2), 3)*100)
barplot(p2, col = c('lightblue','red'))      # no

(t3 = table(adult$income, adult$marital.status))
(p3 = round(prop.table(t3, margin = 2), 3)*100)
barplot(p3, col = c('lightblue','red'))      # yes

(t4 = table(adult$income, adult$occupation))
(p4 = round(prop.table(t4, margin = 2), 3)*100)
barplot(p4, col = c('lightblue','red'))     #yes

(t5 = table(adult$income, adult$relationship))
(p5 = round(prop.table(t5, margin = 2), 3)*100)
barplot(p5, col = c('lightblue','red'))    #yes

(t6 = table(adult$income, adult$race))
(p6 = round(prop.table(t6, margin = 2), 3)*100)
barplot(p6, col = c('lightblue','red'))    #no

(t7 = table(adult$income, adult$sex))
(p7 = round(prop.table(t7, margin = 2), 3)*100)
barplot(p7, col = c('lightblue','red'))    #yes

(t8 = table(adult$income, adult$native.country))
(p8 = round(prop.table(t8, margin = 2), 3)*100)
barplot(p8, col = c('lightblue','red'))   #yes

# Q26

table(adult$marital.status, adult$relationship)
table(adult$sex, adult$marital.status)
table(adult$sex, adult$workclass)
table(adult$sex, adult$education)
table(adult$sex, adult$relationship)

# Q28

table(adult$workclass)

# Q30

ggplot(data = adult, aes(x=age, fill=income)) + geom_histogram()
ggplot(data = adult, aes(x=age, fill=income)) + geom_histogram(position = 'fill')

ggplot(data = adult, aes(x=fnlwgt, fill=income)) + geom_histogram()
ggplot(data = adult, aes(x=fnlwgt, fill=income)) + geom_histogram(position = 'fill')

ggplot(data = adult, aes(x=education.num, fill=income)) + geom_histogram()
ggplot(data = adult, aes(x=education.num, fill=income)) + geom_histogram(position = 'fill')

ggplot(data = adult, aes(x=capital.gain, fill=income)) + geom_histogram()
ggplot(data = adult, aes(x=capital.gain, fill=income)) + geom_histogram(position = 'fill')

ggplot(data = adult, aes(x=capital.loss, fill=income)) + geom_histogram()
ggplot(data = adult, aes(x=capital.loss, fill=income)) + geom_histogram(position = 'fill')

ggplot(data = adult, aes(x=hours.per.week, fill=income)) + geom_histogram()
ggplot(data = adult, aes(x=hours.per.week, fill=income)) + geom_histogram(position = 'fill')

# Q32

par(mfrow=c(1,1))
plot(adult$age[adult$income=='<=50K'], adult$education.num[adult$income=='<=50K'], col='red',
     pch=20, cex=1.8)
points(adult$age[adult$income=='>50K'], adult$education.num[adult$income=='>50K'], col='lightblue',
     pch=20)
legend("topleft" , legend = c( "<=50K" , ">50K") , col = c("red" , "lightblue") , pch = 20 , cex = 0.8)

plot(adult$education.num[adult$income=='<=50K'], adult$hours.per.week[adult$income=='<=50K'], col='red',
     pch=20, cex=1.2)
points(adult$education.num[adult$income=='>50K'], adult$hours.per.week[adult$income=='>50K'], col='lightblue',
       pch=20)
legend("topleft" , legend = c( "<=50K" , ">50K") , col = c("red" , "lightblue") , pch = 20 , cex = 0.8)

# Q33

for(i in 1:32561){
  if(adult$age[i]<38){adult$age.group[i] = 'Young'}
  if(adult$age[i]>=38 & adult$age[i]<62){adult$age.group[i] = 'Adult'}
  if(adult$age[i]>=62){adult$age.group[i] = 'Old'}
}

table(adult$age.group)
(t9 = table(adult$income, adult$age.group))
(p9 = round(prop.table(t9, margin = 2), 3)*100)

barplot(p9, col =c('lightblue','red'))
legend('topright', col = c('lightblue','red'), legend = c('<=50K','>50'), pch = 20)


flag.age = ifelse(adult$age < 52, 'Young', 'Old')
table(flag.age)
(t10 = table(adult$income, flag.age))
(p10 = round(prop.table(t10, margin = 2), 3)*100)

barplot(p10, col =c('lightblue','red'))
legend('topright', col = c('lightblue','red'), legend = c('<=50K','>50'), pch = 20)

# Q34

n = 32561
nbins = 3
whichbin <- c(rep(0, n))

# Equal frequency
freq <- n/nbins
# Sort the data
age.sorted <- sort(adult$age)
for (i in 1:nbins) {
  for (j in 1:n) {
    if((i-1)*freq < j && j <=i*freq)
      whichbin[j] <- i
  }
}
whichbin

(t11=table(adult$income, whichbin))
(p11=round(prop.table(t11, margin = 2), 3)*100)
barplot(p11)

# Equal width
range_age <- max(adult$age) - min(adult$age) + 1
binwidth <- range_age/nbins
for (i in 1:nbins) {
  for (j in 1:n) {
    if((i-1)*binwidth < adult$age[j] &&
       adult$age[j] <= (i)*binwidth)
      whichbin[j] <- i
  }
}
whichbin

(t12=table(adult$income, whichbin))
(p12=round(prop.table(t12, margin = 2), 3)*100)
barplot(p12)






