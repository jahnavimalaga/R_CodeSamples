housetrain=read.csv(file.choose(),stringsAsFactors = F)
housetest=read.csv(file.choose(),stringsAsFactors = F)
totaldata=rbind(housetrain[-81],housetest)#rowbinding concatinationg rows there must
#equal no of columns rbind=row binding cbind=column binding
for (f in 1:length(names(totaldata))){
  levels(housetrain[, f])=levels(totaldata[, f])
}
#we have to match the levels in the two datasets order to a
#void any problem ie same levels should be present 
#this code is for matching the levels with in the variables
sort(colSums(is.na(totaldata)),decreasing=T)#checking missing values
str(totaldata)#checking for total data
totaldatacat=totaldata[sapply(totaldata,is.character)]#split catogorical columns
totaldatanum=totaldata[sapply(totaldata,is.numeric)]#split numerical columns
colnames(totaldatacat)#column names
sort(colSums(is.na(totaldatacat)),decreasing=T)#checking missing values
noncols=c( "PoolQC","MiscFeature","Alley","Fence","FireplaceQu")#charecter vector
#the fundemental datastructure of R is vector.the vector startswith lowercase
#c followed by paranthesis items sperated by copmas
for(col in noncols){
  totaldatacat[sapply(totaldatacat[col],is.na),col]="missing"
}
totaldatacat=sapply(totaldatacat,
                    function(x) ifelse(is.na(x),names(which.max(table(x))),x))
values=c('YearBuilt', 'YearRemodAdd','MoSold', 'YrSold','OverallQual',
         'OverallCond','MSSubClass','MoSold','GarageYrBlt')
totaldataval=totaldatanum[values]
totaldatanum=totaldatanum[,!colnames(totaldatanum)%in%values]
#! represents not equal to
#%in% represents matching function
#sapply row apply lapply column apply
totaldataval=sapply(totaldata,function(x) ifelse(is.na(x),
                                                 names(which.max(table(x))),x))
totaldatanum=sapply(totaldatanum,function(x) ifelse(is.na(x),mean(x,na.rm=T),x))
totaldatadf=data.frame(totaldatanum,totaldataval,totaldatacat)
sort(colSums(is.na(totaldatadf)),,decreasing = T)
housetraindf=totaldatadf[1:1460,]#comma at the end in the row selection
housetraindf=cbind(housetraindf,SalePrice=housetrain$SalePrice)
housetestdf=totaldatadf[1461:2919,]
