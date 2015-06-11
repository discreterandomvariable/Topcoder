a=read.csv("C:/Users/Akshay/Desktop/TopCoder/train1.csv")
b=read.csv("C:/Users/Akshay/Desktop/TopCoder/testing.csv")

a1=unique(a[,c(1,11:26)])
b1=unique(b[,c(1,11:26)])

aa1=subset(a[,c(1:10)],agedays=='1')
aa1=aa1[,-2]
names(aa1)=c('subjid','wtkg_1','htcm_1','lencm_1','bmi_1','waz_1','haz_1','whz_1','baz_1')
aa2=subset(a[,c(1:10)],agedays=='123')
aa2=aa2[,-2]
names(aa2)=c('subjid','wtkg_123','htcm_123','lencm_123','bmi_123','waz_123','haz_123','whz_123','baz_123')
aa3=subset(a[,c(1:10)],agedays=='366')
aa3=aa3[,-2]
names(aa3)=c('subjid','wtkg_366','htcm_366','lencm_366','bmi_366','waz_366','haz_366','whz_366','baz_366')
aa4=subset(a[,c(1:10)],agedays=='1462')
aa4=aa4[,-2]
names(aa4)=c('subjid','wtkg_1462','htcm_1462','lencm_1462','bmi_1462','waz_1462','haz_1462','whz_1462','baz_1462')
aa5=subset(a[,c(1:10,27)],agedays=='2558')
aa5=aa5[,-2]
names(aa5)=c('subjid','wtkg_2558','htcm_2558','lencm_2558','bmi_2558','waz_2558','haz_2558','whz_2558','baz_2558','geniq')

bb1=subset(b[,c(1:10)],agedays=='1')
bb1=bb1[,-2]
names(bb1)=c('subjid','wtkg_1','htcm_1','lencm_1','bmi_1','waz_1','haz_1','whz_1','baz_1')
bb2=subset(b[,c(1:10)],agedays=='123')
bb2=bb2[,-2]
names(bb2)=c('subjid','wtkg_123','htcm_123','lencm_123','bmi_123','waz_123','haz_123','whz_123','baz_123')
bb3=subset(b[,c(1:10)],agedays=='366')
bb3=bb3[,-2]
names(bb3)=c('subjid','wtkg_366','htcm_366','lencm_366','bmi_366','waz_366','haz_366','whz_366','baz_366')
bb4=subset(b[,c(1:10)],agedays=='1462')
bb4=bb4[,-2]
names(bb4)=c('subjid','wtkg_1462','htcm_1462','lencm_1462','bmi_1462','waz_1462','haz_1462','whz_1462','baz_1462')
bb5=subset(b[,c(1:10)],agedays=='2558')
bb5=bb5[,-2]
names(bb5)=c('subjid','wtkg_2558','htcm_2558','lencm_2558','bmi_2558','waz_2558','haz_2558','whz_2558','baz_2558')


m1=merge(merge(merge(merge(merge(a1,aa1,by='subjid',all.x=T,all.y=T),aa2,by='subjid',all.x=T,all.y=T),aa3,by='subjid',all.x=T,all.y=T),aa4,by='subjid',all.x=T,all.y=T),aa5,by='subjid',all.x=T,all.y=T)
m2=merge(merge(merge(merge(merge(b1,bb1,by='subjid',all.x=T,all.y=T),bb2,by='subjid',all.x=T,all.y=T),bb3,by='subjid',all.x=T,all.y=T),bb4,by='subjid',all.x=T,all.y=T),bb5,by='subjid',all.x=T,all.y=T)

#write.csv(m11,'C:/Users/Akshay/Desktop/TopCoder/prep_train.csv',row.names=F)
#write.csv(m22,'C:/Users/Akshay/Desktop/TopCoder/prep_test.csv',row.names=F)

m11=m1[,-c(19,27:29,31:33,35,44,52,54:57)]
m22=m2[,-c(19,27:29,31:33,35,44,52,54:57)]

m11=m11[,-c(6,7)]
m22=m22[,-c(6,7)]



index=sample(1:nrow(train1),0.8*nrow(train1))
test=temp[index,]
train=temp[-index,]

test$siteid=as.factor(test$siteid)
test$sexn=as.factor(test$sexn)
test$feedingn=as.factor(test$feedingn)
test$gagebrth=as.factor(test$gagebrth)
test$apgar1=as.factor(test$apgar1)
test$apgar5=as.factor(test$apgar5)
test$demo1n=as.factor(test$demo1n)
test$mmaritn=as.factor(test$mmaritn)
test$demo2n=as.factor(test$demo2n)


m11$siteid=as.factor(m11$siteid)
m11$sexn=as.factor(m11$sexn)
m11$feedingn=as.factor(m11$feedingn)
m11$gagebrth=as.factor(m11$gagebrth)
m11$apgar1=as.factor(m11$apgar1)
m11$apgar5=as.factor(m11$apgar5)
m11$demo1n=as.factor(m11$demo1n)
m11$mmaritn=as.factor(m11$mmaritn)
m11$demo2n=as.factor(m11$demo2n)

temp=na.omit(m11)

train$siteid=as.factor(train$siteid)
train$sexn=as.factor(train$sexn)
train$feedingn=as.factor(train$feedingn)
train$gagebrth=as.factor(train$gagebrth)
train$apgar1=as.factor(train$apgar1)
train$apgar5=as.factor(train$apgar5)
train$demo1n=as.factor(train$demo1n)
train$mmaritn=as.factor(train$mmaritn)
train$demo2n=as.factor(train$demo2n)

train1=as.data.frame(makeind(train,all=F))
test1=as.data.frame(makeind(test,all=F))

train2=train1[,c(1,6,14,15,29,33:36,39,42:44,47,66:68,74,78:83)]
test2=test1[,c(1,6,14,15,29,33:36,39,42:44,47,66:68,74,78:83)]

train3=cbind(train2[,-c(2:5)],scale(train2[,c(2:5)]))
test3=cbind(test2[,-c(2:5)],scale(test2[,c(2:5)]))

fit=lm(geniq ~ .,train3[,-1])
pred=predict(fit,test3[,-c(1,2)])
fit_check=data.frame('predicted_value'=pred,'actual'=test3[,2])
fit_check$sqr_error=(fit_check$predicted_value-fit_check$actual)^2
fit_check$sseo=(fit_check$actual-mean(train3$geniq))^2
sum(fit_check$sqr_error)/sum(fit_check$sseo)


wss= c()
for(i in 2:30)
  wss[i]=sum(kmeans(scale_data,centers=i,iter.max=100)$withinss)/kmeans(scale_data,centers=i, iter.max=100)$betweenss
plot(1:30, wss, type="b", xlab="Number of Clusters",
     ylab="total Within Cluster/Between Cluster Variance")

library(useful)
PlotHartigan(FitKMeans(scale_data, max.clusters=30, iter.max=100))

fit=kmeans(scale_data,6,iter.max=100)
ff1=data.frame(ff,"cluster"=fit$cluster)
ff2=cbind(ff1,'parity'=df[,23])
