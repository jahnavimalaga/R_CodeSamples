housetrain1=read.csv(file.choose())
housetest1=read.csv(file.choose())
totaldata1=rbind(housetrain1[-81],housetest1)
for(f in 1:length(names(totaldata1))){
  levels(housetrain1[, f])=levels(totaldata1[, f])
}
#package required 'miss forest"
totaldata1impute=missForest(totaldata1)
