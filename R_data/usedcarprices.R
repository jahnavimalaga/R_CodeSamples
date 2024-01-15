usedcarprices=read.csv(file.choose()) #import data
head(usedcarprices) #first 6obs of data
tail(usedcarprices) #last 6 obd of data
str(usedcarprices)  #data structure & data type of each vaiable
nrow(usedcarprices) #no of rows
ncol(usedcarprices) #no of col
class(usedcarprices)#data strucutre
summary(usedcarprices)# descriptive stats for numerical
#frequency counts for categorical or string variables
summary(usedcarprices$Price)#summary of one variable price

hist(usedcarprices$Price)
boxplot(usedcarprices$Price,horizontal=T)#box plot
plot(density(usedcarprices$Price))#desity
boxplot(usedcarprices$Age,horizontal=T)
hist(usedcarprices$Age)

plot(density(usedcarprices$KM))

table(usedcarprices$FuelType)#frequency counts
table(usedcarprices$FuelType,usedcarprices$AutoType)
#cross tabulation

aggregate(usedcarprices$Price~usedcarprices$FuelType,FUN=mean)

#equivalent to groupby function in python 
#is the average price of automatic & manual cars same
#is the average age of automatic & manual cars same
#is the average price of metcolor & nonmetcolor cars same
#is the average KM of automatic & manual cars same

aggregate(usedcarprices$Price~usedcarprices$AutoType,FUN=mean)
aggregate(usedcarprices$Age~usedcarprices$AutoType,FUN=mean)
aggregate(usedcarprices$KM~usedcarprices$AutoType,FUN=mean)
aggregate(usedcarprices$Price~usedcarprices$MetColorType,FUN=mean)

summary(usedcarprices$Price)

usedcarprices$PriceType=ifelse(usedcarprices$Price>=12000,"very high",
                               ifelse(usedcarprices$Price>=10000,"high price",
                                      ifelse(usedcarprices$Price>=8500,"medium price",
                                             "low price")))
head(usedcarprices)
#for usge type ,speed type,age type
summary(usedcarprices$KM)
usedcarprices$UsageType=ifelse(usedcarprices$KM>=110,"very high",
                               ifelse(usedcarprices$KM>=90,"high price","low"))

#null - there is no significant difference average in prices
#of autmatic & manual - both means are equal
#alternate - there is significant dif averge in prices of automatic and manual-
#both means are not equal

t.test(usedcarprices$Price~usedcarprices$AutoType)

#since p-value is greater than .05,accept NULL or Fail to Reject NULL

#null - there is no significant difference average in prices
#of metcolor & nommetcolor - both means are equal
#alternate - there is significant dif averge in prices of metcol and nonmetcolr 
#cars both means are not equal

t.test(usedcarprices$Price~usedcarprices$MetColorType)

#null - there is no significant difference average in prices
#of petrol desiel & cng - both means are equal
#alternate - there is significant dif averge in prices of petrol desiel cng
#cars both means are not equal
#anova single aov()
summary(aov(usedcarprices$Price~usedcarprices$FuelType))
#ANOVA SINGLE FAACTOR
#null - there is no significant difference average in AGE
#of petrol desiel & cng - both means are equal
#alternate - there is significant dif averge in AGE of petrol desiel cng
#cars both means are not equal

aggregate(usedcarprices$Age~usedcarprices$FuelType,FUN=mean)

summary(usedcarprices$Age~usedcarprices$FuelType)

#NULL -there is no relationship between fueltype and usage type
#alternate - there is relationship between fuel type and usage type
chisq.test(table(usedcarprices$FuelType,usedcarprices$UsageType))

#NULL -there is no relationship between autotype and usage type
#alternate - there is relationship between autotype and usage type
chisq.test(table(usedcarprices$AutoType,usedcarprices$UsageType))

#Linear Regression Model
usedcarreg=lm(Price~.,data=usedcarprices[-1])#building the model
summary(usedcarreg)#check the output for adjustment 

plot(usedcarreg)#it gives u four plots when u press enter in the console
sqrt(mean(usedcarreg$residuals^2))#RMSE

#Decision tree model #
usedcarrpart=rpart(Price~.,data=usedcarprices[-1])
summary(usedcarrpart)
rpart.plot(usedcarrpart)#package :rpart.plot

#RANDOM foreser #package required "randomForest"

usedcarrandforest=randomForest(Price~.,data=usedcarprices[-1],ntree=3000,do.trace=100)
print(usedcarrandforest)

