bank=read.csv(file.choose())
head(bank)
chisq.test(table(bank$loan,bank$marital))

bank1=bank[-c(1,2,11)]

#Logictic regrssion
banklogit=glm(y~.,data=bank1,family="binomial")
summary(banklogit)
banklogitpredict=predict(banklogit,type="response")
table(Actual=bank1$y,Predict=banklogitpredict>0.5)#Confusion Matrix
#(4191+152)/(4191+102+555+152)

#Decision Tree #Package required is "rpart"
bankrpart=rpart(y~.,data=bank1)
summary(bankrpart)
plot(bankrpart)
text(bankrpart)
rpart.plot(bankrpart)#package required "rpart plot"
bankrpartpredict=predict(bankrpart,type="class")
#(4233+512)/(4233+60+195+512)
#random forest(3000 trees)#package "random forest"
bank1randforest=randomForest(y~.,data=bank1[-1],ntree=3000,do.trace=100)
print(bank1randforest)

bank1randforest=randomForest(y~.,data=bank1[-1],ntree=3000,do.trace=100)
print(bank1randforest)


#boosting machine
bank2=bank1
bank2$y=ifelse(bank2$bank==" True.",1,0)
table(bank2$y)

bankgbm=gbm(y~.,data=bank2,distribution="bernoulli",n.trees=3000,cv.folds=3)
bestiter=gbm.perf(bankgbm,method="cv")

gbmpredict=predict(bankgbm,bank2,bestiter)
table(bank2$y,gbmpredict>0.5)
#(4282+28)/(4282+11+679+28)

#Neural Networks -  packages "nnet"
banknnet=nnet(y~.,data=bank1,size=10,maxit=100)
summary(banknnet)
banknnetpredict=predict(banknnet,typr="class")
table(bank1$y,banknnetpredict)
(4292+1)/(4292+1+786+1)

