library(BayesTree)
library(VIM)
library(fpc)
library(cluster)
library(smoothSurv)
library(leaps)
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

missing_count=data.frame(summary(aggr(m1[,-58],plot=F))[[1]],summary(aggr(m2,plot=F))[[1]])
names(missing_count)=c('Variable_Training','Missing_Training','Variable_Test','Missing_Test')
missing_count=missing_count[,-3]
removal=subset(missing_count,Missing_Training==8332)

m1=m1[,-c(6,7)]
m2=m2[,-c(6,7)]

m11=m1[,!names(m1)%in%removal$Variable_Training]
m22=m2[,!names(m2)%in%removal$Variable_Training]

m11$siteid=as.factor(m11$siteid)
m11$sexn=as.factor(m11$sexn)
m11$feedingn=as.factor(m11$feedingn)
m11$gagebrth=as.factor(m11$gagebrth)
m11$apgar1=as.factor(m11$apgar1)
m11$apgar5=as.factor(m11$apgar5)
m11$demo1n=as.factor(m11$demo1n)
m11$mmaritn=as.factor(m11$mmaritn)
m11$demo2n=as.factor(m11$demo2n)

m22$siteid=as.factor(m22$siteid)
m22$sexn=as.factor(m22$sexn)
m22$feedingn=as.factor(m22$feedingn)
m22$gagebrth=as.factor(m22$gagebrth)
m22$apgar1=as.factor(m22$apgar1)
m22$apgar5=as.factor(m22$apgar5)
m22$demo1n=as.factor(m22$demo1n)
m22$mmaritn=as.factor(m22$mmaritn)
m22$demo2n=as.factor(m22$demo2n)

s1=as.data.frame(summary(aggr(m11,plot=F))[1])
s2=as.data.frame(summary(aggr(m22,plot=F))[1])

s11=subset(s1,s1[,2]!=0)
aggr(m11[,as.character(s11[,1])])
plot(aggr(m11[,as.character(s11[,1])]),cex.axis=0.4)


clusterboot(makeind(na.omit(m11),all=F),B=5, distances=(class(m11)=="dist"),
            bootmethod="boot",
            bscompare=TRUE,
            multipleboot=FALSE,
            clustermethod=pamkCBI,noisemethod=FALSE)

wss= c()
for(i in 2:30)
  wss[i]=sum(pam(makeind(na.omit(m11),all=F),k=i)$clusinfo[,3])
plot(1:30, wss, type="b", xlab="Number of Clusters",
     ylab="Dissimilarity")

m11_imp=kNN(m11[,-c(1,42)],k=10,imp_var=F)
m22_imp=kNN(m22[,-1],k=10,imp_var=F)

m111=data.frame(m11_imp,'geniq'=m11$geniq)
m222=m22_imp

m111$change_wtkg_1=(m111$wtkg_123-m111$wtkg_1)/122
#m111$change_wtkg_1=(m111$wtkg_366-m111$wtkg_1)/365
m111$change_ht_1=(m111$lencm_366-m111$lencm_1)/365
m111$change_wtkg_2=(m111$wtkg_366-m111$wtkg_123)/(366-123)
m111$change_ht_2=(m111$htcm_1462-m111$lencm_366)/(1462-366)
m111$change_wtkg_3=(m111$wtkg_1462-m111$wtkg_366)/(1462-366)
m111$change_ht_3=(m111$htcm_2558-m111$htcm_1462)/(2558-1462)
m111$change_wtkg_4=(m111$wtkg_2558-m111$wtkg_1462)/(2558-1462)
m111$change_bmi_1=(m111$bmi_366-m111$bmi_1)/365
m111$change_bmi_2=(m111$bmi_1462-m111$bmi_366)/(1462-366)
m111$change_bmi_3=(m111$bmi_2558-m111$bmi_1462)/(2558-1462)

m222$change_wtkg_1=(m222$wtkg_123-m222$wtkg_1)/122
#m222$change_wtkg_1=(m222$wtkg_366-m222$wtkg_1)/365
m222$change_ht_1=(m222$lencm_366-m222$lencm_1)/365
m222$change_wtkg_2=(m222$wtkg_366-m222$wtkg_123)/(366-123)
m222$change_ht_2=(m222$htcm_1462-m222$lencm_366)/(1462-366)
m222$change_wtkg_3=(m222$wtkg_1462-m222$wtkg_366)/(1462-366)
m222$change_ht_3=(m222$htcm_2558-m222$htcm_1462)/(2558-1462)
m222$change_wtkg_4=(m222$wtkg_2558-m222$wtkg_1462)/(2558-1462)
m222$change_bmi_1=(m222$bmi_366-m222$bmi_1)/365
m222$change_bmi_2=(m222$bmi_1462-m222$bmi_366)/(1462-366)
m222$change_bmi_3=(m222$bmi_2558-m222$bmi_1462)/(2558-1462)


#write.csv(m111,'C:/Users/Akshay/Desktop/TopCoder/sample.csv',row.names=F)

train=m111[,c(1:14,18:21,23,27:30,34:37,42:51,41)]
test=m222[,c(1:14,18:21,23,27:30,34:37,41:50)]

train1=std.data(train,names(train)[c(7,10:13,15:37)])
test1=std.data(test,names(test)[c(7,10:13,15:37)])

#pca=prcomp(train[,c(7,10:13,15:37)],center=TRUE,scale.=TRUE)
#train2=data.frame(pca$x[,1:12],train[,-c(7,10:13,15:37)])


mic=as.data.frame(makeind(train1,all=F))
fit=lm(geniq ~ ., data=mic)
coef=as.data.frame(summary(fit)$coefficients)
coef1=subset(coef,coef[,4]<0.1)
var=c(rownames(coef1)[-1],'geniq')

data=mic[,c(1,5,7,11,12,14,20,21,24,25,26,28,30:33,35,38:40,42:44,52,65,69,71,77:80,29)]
full=lm(geniq ~ ., train1)
null=lm(geniq ~ 1, train1)
step(full,scope=list(lower=null, upper=full),direction="backward",trace=1)

#data=mic[,c(1:12,14:17,19,22:24,26:28,36,49,53,55,61:64,13)]

model=lm(geniq ~ poly(meducyrs,2) + haz_1 + poly(waz_366,4) + baz_366 + poly(change_ht_1,2) + 
     change_wtkg_2 + poly(change_ht_3,2) + poly(change_wtkg_4,3) + change_bmi_1 + 
     change_bmi_3 + siteid.5 + siteid.10 + siteid.15 + siteid.31 + 
     siteid.45 + siteid.60 + siteid.66 + siteid.71 + feedingn.1 + 
     feedingn.2 + feedingn.3 + apgar1.1 + apgar5.4 + demo1n.1 + 
     demo2n.1 + demo2n.2 + demo2n.3 + demo2n.4,data)

model2=lm(geniq ~ poly(meducyrs,2) + haz_1 + poly(waz_366,4) + baz_366 + poly(change_ht_1,2) + 
     change_wtkg_2 + poly(change_ht_3,2) + poly(change_wtkg_4,3) + change_bmi_1 + 
     change_bmi_3 + siteid + feedingn + 
     apgar1 + apgar5 + demo1n + 
     demo2n , train1)

full=lm(geniq ~ poly(meducyrs,2) + haz_1 + poly(waz_366,4) + baz_366 + poly(change_ht_1,2) + 
     change_wtkg_2 + poly(change_ht_3,2) + poly(change_wtkg_4,3) + change_bmi_1 + 
     change_bmi_3 + siteid + feedingn + 
     apgar1 + apgar5 + demo1n + 
     demo2n , train1)

fit22=lm(formula = geniq ~ poly(meducyrs, 2) + haz_1 + I(waz_366^4) + I(waz_366^3) + baz_366 + poly(change_ht_1,2) + change_wtkg_2 + I(change_ht_3^2) + I(change_wtkg_4^3) + I(change_wtkg_4^2) + change_bmi_1 + siteid + feedingn + apgar5 + demo1n + demo2n, data = train1)

rownames(train1)=m11[,1]
rownames(test1)=m22[,1]
index=sample(1:nrow(train1),0.8*nrow(train1))
train11=train1[index,]
test11=train1[-index,]
train111=as.data.frame(makeind(train11,all=F))
test111=as.data.frame(makeind(test11,all=F))

fits=lm(formula = geniq ~ poly(meducyrs, 2) + haz_1 + I(waz_366^4) + I(waz_366^3) + baz_366 + poly(change_ht_1,2) + change_wtkg_2 + I(change_ht_3^2) + I(change_wtkg_4^3) + I(change_wtkg_4^2) + change_bmi_1 + siteid.5 + siteid.10 + siteid.15 + siteid.31 + 
          siteid.45 + siteid.60 + siteid.66 + siteid.71 + feedingn.1 + 
          feedingn.2 + feedingn.3 + apgar1.1 + apgar5.4 + demo1n.1 + 
          demo2n.1 + demo2n.2 + demo2n.3 + demo2n.4, data = train111)
fits=lm(geniq ~ poly(meducyrs,2) + haz_1 + poly(waz_366,4) + baz_366 + poly(change_ht_1,2) + 
     change_wtkg_2 + poly(change_ht_3,2) + poly(change_wtkg_4,3) + change_bmi_1 + 
     change_bmi_3 + siteid.5 + siteid.10 + siteid.15 + siteid.31 + 
       siteid.45 + siteid.60 + siteid.66 + siteid.71 + feedingn.1 + 
       feedingn.2 + feedingn.3 + apgar1.1 + apgar5.4 + demo1n.1 + 
       demo2n.1 + demo2n.2 + demo2n.3 + demo2n.4, train111)
fits=lm(geniq ~ meducyrs + haz_1 + waz_366 + baz_366 + change_ht_1 + 
       change_wtkg_2 + change_ht_3 + change_wtkg_4 + change_bmi_1 + 
       change_bmi_3 + siteid.5 + siteid.10 + siteid.15 + siteid.31 + 
       siteid.45 + siteid.60 + siteid.66 + siteid.71 + feedingn.1 + 
       feedingn.2 + feedingn.3 + apgar1.1 + apgar5.4 + demo1n.1 + 
       demo2n.1 + demo2n.2 + demo2n.3 + demo2n.4,train111)

pred=predict(fits,test111[,-29])
fit_check=data.frame('predicted_value'=pred,'actual'=test111[,29])
fit_check$sqr_error=(fit_check$predicted_value-fit_check$actual)^2
fit_check$sseo=(fit_check$actual-mean(train11$geniq))^2
sum(fit_check$sqr_error)/sum(fit_check$sseo)
