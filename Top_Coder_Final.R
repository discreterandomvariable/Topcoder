library(BayesTree)
library(VIM)
library(fpc)
library(cluster)
library(smoothSurv)
library(leaps)
library(psych)
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

missing_count=data.frame(summary(aggr(m1[,-58],plot=T))[[1]],summary(aggr(m2,plot=T))[[1]])
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

#describe(m11)

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

m11_imp=kNN(m11[,-c(1,42)],k=10,imp_var=F)
m22_imp=kNN(m22[,-1],k=10,imp_var=F)

m111=data.frame(m11_imp,'geniq'=m11$geniq)
m222=m22_imp

m111$change_wtkg_1=(m111$wtkg_123-m111$wtkg_1)/122
m111$change_ht_1=(m111$lencm_366-m111$lencm_1)/365
m111$change_wtkg_2=(m111$wtkg_366-m111$wtkg_123)/(366-123)
m111$change_ht_2=(m111$htcm_1462-m111$lencm_366)/(1462-366)
m111$change_wtkg_3=(m111$wtkg_1462-m111$wtkg_366)/(1462-366)
m111$change_ht_3=(m111$htcm_2558-m111$htcm_1462)/(2558-1462)
m111$change_wtkg_4=(m111$wtkg_2558-m111$wtkg_1462)/(2558-1462)
m111$change_bmi_1=(m111$bmi_366-m111$bmi_1)/365
m111$change_bmi_2=(m111$bmi_1462-m111$bmi_366)/(1462-366)
m111$change_bmi_3=(m111$bmi_2558-m111$bmi_1462)/(2558-1462)

#m1c=m111[,-c(1:6,8,9,14)]
#cm=cor(m1c)
#apply(cm,2,function(x)which(x>0.9))

m222$change_wtkg_1=(m222$wtkg_123-m222$wtkg_1)/122
m222$change_ht_1=(m222$lencm_366-m222$lencm_1)/365
m222$change_wtkg_2=(m222$wtkg_366-m222$wtkg_123)/(366-123)
m222$change_ht_2=(m222$htcm_1462-m222$lencm_366)/(1462-366)
m222$change_wtkg_3=(m222$wtkg_1462-m222$wtkg_366)/(1462-366)
m222$change_ht_3=(m222$htcm_2558-m222$htcm_1462)/(2558-1462)
m222$change_wtkg_4=(m222$wtkg_2558-m222$wtkg_1462)/(2558-1462)
m222$change_bmi_1=(m222$bmi_366-m222$bmi_1)/365
m222$change_bmi_2=(m222$bmi_1462-m222$bmi_366)/(1462-366)
m222$change_bmi_3=(m222$bmi_2558-m222$bmi_1462)/(2558-1462)

train=m111[,c(1:14,18:21,23,27:30,34:37,42:51,41)]
test=m222[,c(1:14,18:21,23,27:30,34:37,41:50)]



rownames(train)=m11[,1]
rownames(test)=m22[,1]

train_num=apply(train[,-c(1:6,8,9,14,38)],2,function(x) (x-mean(x))/sd(x))
test_num=apply(test[,-c(1:6,8,9,14)],2,function(x) (x-mean(x))/sd(x))

train1=data.frame(train_num,train[,c(1:6,8,9,14,38)])
test1=data.frame(test_num,test[,c(1:6,8,9,14)])

model_call=lm(formula = geniq ~ meducyrs + I(meducyrs^2) + I(waz_366^3) + 
                 I(waz_366^2) + I(change_ht_1^2) + change_ht_3 + I(change_wtkg_4^2) + 
                 I(change_ht_3^2) + waz_1 + change_wtkg_1 + siteid + sexn +  
                 apgar5 + demo1n + demo2n + sexn:demo1n + feedingn:demo1n, 
               data = train1)

pred=predict(model_call,test1)
check=data.frame('subjid'=rownames(test1),'predicted_value'=pred)
result=merge(check,m1[,c(1,56)],by='subjid',all.x=T,all.y=F)
result$geniq_1=ifelse(is.na(result$geniq),result$predicted_value,result$geniq)
names(result)=c('subjid','predicted_value','geniq_1','geniq')

write.csv(result,'C:/Users/Akshay/Desktop/TopCoder/results_AkshayLahoti_pearsonresidual.csv',row.names=F)

#################################

index=sample(1:nrow(train1),0.8*nrow(train1))
train11=train1[index,]
tr_test11=train1[-index,]

full=lm(geniq ~ meducyrs + I(meducyrs^2) + I(waz_366^3) + I(waz_366^2) + waz_366 
    + I(change_ht_1^2) + change_ht_1 + change_ht_3 + I(change_wtkg_4^2) + change_wtkg_4
    + I(change_ht_3^2) + I(change_wtkg_4^3) + mage + mcignum + parity + 
    gravida + waz_1 + haz_1 + whz_1 + baz_1 + waz_123 + haz_366 + whz_366 +
    baz_366 + waz_1462 + haz_1462 + whz_1462 + baz_1462 + change_wtkg_1 +
    change_wtkg_2 + change_ht_2 + change_wtkg_3 + change_bmi_1 + change_bmi_2 + 
    change_bmi_3 + siteid + sexn + feedingn + gagebrth + apgar1 + apgar5 +
    demo1n + mmaritn + demo2n + siteid:sexn + siteid:feedingn + siteid:gagebrth 
    + siteid:apgar1 + mmaritn:demo2n + siteid:demo2n + siteid:mmaritn 
    + siteid:apgar5 + siteid:demo1n + sexn:feedingn + sexn:gagebrth 
    + sexn:apgar1 + sexn:demo2n + sexn:mmaritn + sexn:apgar5 + sexn:demo1n 
    + feedingn:gagebrth + feedingn:apgar1 + gagebrth:apgar1 + apgar1:apgar5 
    + demo1n:demo2n + demo1n:mmaritn + feedingn:demo2n + feedingn:mmaritn 
    + feedingn:demo1n + feedingn + gagebrth:demo2n + gagebrth:mmaritn 
    + gagebrth:demo1n +gagebrth + apgar1:demo2n + apgar1:mmaritn 
    + apgar1:demo1n + apgar5:demo2n + apgar5:mmaritn + apgar5:demo1n,train11)


coef=as.data.frame(summary(full)$coefficients)
coef1=subset(coef,coef[,4]<0.05)
var=c(rownames(coef1)[-1],'geniq')

model=lm(geniq ~ meducyrs + I(meducyrs^2) + I(waz_366^3) + I(waz_366^2) + waz_366 + 
I(change_ht_1^2) + change_ht_3 + I(change_wtkg_4^2) + I(change_ht_3^2) + mage + 
waz_1 + change_wtkg_1 + siteid + sexn + feedingn + apgar5 + demo1n + mmaritn + demo2n +
siteid:feedingn + siteid:gagebrth + siteid:apgar1 + mmaritn:demo2n + siteid:demo2n +
siteid:mmaritn + siteid:apgar5 + siteid:demo1n + sexn:gagebrth + sexn:apgar5 +
sexn:demo1n + feedingn:gagebrth + feedingn:apgar1 + gagebrth:apgar1 + apgar1:apgar5 +
feedingn:demo1n + gagebrth:mmaritn + feedingn:mmaritn + apgar1:mmaritn + apgar1:demo2n +
apgar5:mmaritn + apgar5:demo1n, train11)

model1=lm(geniq ~ 1, train11)
step(model,scope=list(lower=model1, upper=model),direction="backward",trace=1)
step(model1,scope=list(lower=model1, upper=model),direction="forward",trace=1)

model_call=lm(formula = geniq ~ meducyrs + I(meducyrs^2) + I(waz_366^3) + 
     I(waz_366^2) + I(change_ht_1^2) + change_ht_3 + I(change_wtkg_4^2) + 
     I(change_ht_3^2) + waz_1 + change_wtkg_1 + siteid + sexn + 
     feedingn + apgar5 + demo1n + demo2n + sexn:demo1n + feedingn:demo1n, 
     data = train11)

model_call2=lm(formula = geniq ~ meducyrs + I(meducyrs^2) + I(waz_366^3) + 
                I(waz_366^2) + I(change_ht_1^2) + change_ht_3 + I(change_wtkg_4^2) + 
                I(change_ht_3^2) + waz_1 + change_wtkg_1 + siteid + sexn  
               + apgar5 + demo1n + demo2n + sexn:demo1n + feedingn:demo1n, 
              data = train11)

model_call1=lm(formula = geniq ~ siteid + meducyrs + demo1n + demo2n + waz_366 + 
                 feedingn + I(waz_366^2) + waz_1 + change_wtkg_1 + I(meducyrs^2) + 
                 I(change_ht_3^2) + I(waz_366^3) + change_ht_3 + I(change_wtkg_4^2) + 
                 I(change_ht_1^2) + apgar5 + sexn + demo1n:feedingn + demo1n:sexn, 
                 data = train11)

#leaps=regsubsets(geniq ~ .,data=train11,nbest=10,really.big=T)
#plot(leaps, scale="adjr2")
#plot(leaps, scale="bic")

#model_1=lm(geniq ~ meducyrs + waz_123 + siteid + feedingn + demo1n + demo2n, train11)

pred=predict(model_call2,tr_test11[,-38])
fit_check=data.frame('predicted_value'=pred,'actual'=tr_test11[,38])
fit_check$sqr_error=(fit_check$predicted_value-fit_check$actual)^2
fit_check$sseo=(fit_check$actual-mean(train11$geniq))^2
sum(fit_check$sqr_error)/sum(fit_check$sseo)
