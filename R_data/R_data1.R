churndata=read.csv(file.choose())
str(churndata)
churndata1=churndata[-c(1,3,4)]#negative vector indexing
#delete columns state,areacode,phonenumer
#null - the average custemer service calls made by customers churn 
#true id equalto customers churn false
aggregate(churndata1$Number.Customer.Service.calls~churndata1$Churn,FUN=mean)
t.test(churndata1$Number.Customer.Service.calls~churndata1$Churn)
#there is no relationship between customer churn & voicemail subscribe 
chisq.test(table(churndata1$Churn,churndata1$Voice.mail.Plan))

#Binary Logistic Regression#
churnlogit=glm(Churn~.,data=churndata1,family="binomial")
summary(churnlogit)
churnlogitpredict=predict(churnlogit,type="response")
table(Actual=churndata1$Churn,Predict=churnlogitpredict>0.5)#Confusion Matrix
(4191+152)/(4191+102+555+152)#accuracy -0.8686=86.86%

#Decision Tree #Package required is "rpart"
churnrpart=rpart(Churn~.,data=churndata1)
summary(churnrpart)
plot(churnrpart)
text(churnrpart)
rpart.plot(churnrpart)#package required "rpart plot"
churnrpartpredict=predict(churnrpart,type="class")
(4233+512)/(4233+60+195+512)
#random forest(3000 trees)#package "random forest"
churndata1randforest=randomForest(Churn~.,data=churndata1[-1],ntree=3000,do.trace=100)
print(churndata1randforest)

#Gradient Boosting Machine #package "gbm"
churndata2=churndata1
churndata2$Churn=ifelse(churndata2$Churn==" True.",1,0)
table(churndata2$Churn)

churngbm=gbm(Churn~.,data=churndata2,distribution="bernoulli",n.trees=3000,cv.folds=3)
bestiter=gbm.perf(churngbm,method="cv")

gbmpredict=predict(churngbm,churndata2,bestiter)
table(churndata2$Churn,gbmpredict>0.5)
(4282+28)/(4282+11+679+28)

#Neural Networks -  packages "nnet"
churnnnet=nnet(Churn~.,data=churndata1,size=10,maxit=100)
summary(churnnnet)
churnnnetpredict=predict(churnnnet,typr="class")
table(churndata1$Churn,churnnnetpredict)
(4292+1)/(4292+1+786+1)
