# STATS500-HW1
# author: Ningyuan Wang
# date: "9/17/2019"

# Libraries and Data
library(faraway)
data("teengamb")

# data clean
dim(teengamb) 
colnames(teengamb)#sex, status, income, verbal, and gamble
teengamb$sex<-factor(teengamb$sex)
levels(teengamb$sex)<-c("Male","Female")#treat "sex" as a categorical variable

# numerical summary 
summary(teengamb)

# graphical summary 
hist(teengamb$gamble,xlab = "Gamble Expenditure",main="")
# In the study, most teenagers cost no more than 25 pounds in gamble per year

plot(teengamb$sex, teengamb$gamble, xlab="Sex", ylab="Gamble Expenditure")
# Male spend more money in gamble than female

plot(teengamb$status,teengamb$gamble, xlab = "Status", ylab="Gamble Expenditure")
#the gamble cost is less as the teengaers' parents occpuation increases

plot(teengamb$income,teengamb$gamble, xlab = "Income", ylab="Gamble Expenditure")
#has some relationship between income and gamble 


# additional questions
round(mean(teengamb$income),digits = 2)#4.64
round(median(teengamb$income),digits = 2)#3.25
round(mean(teengamb$gamble),digits  = 2)#19.3
round(median(teengamb$gamble),digits  = 2)#6
plot(density(teengamb$income),main=" ",xlab = "Income")
plot(density(teengamb$gamble),main=" ",xlab = "Gamble")


sort(unique(teengamb$verbal))#9 different values for the variable "verbal"
boxplot(teengamb$verbal, xlab="Verbal")

teengamb[teengamb$verbal<3,] 
#Verbal 2 is at Row 31, and verbal 1 is at Row 35.














