a=read.csv("C:/Users/Akshay/Desktop/TopCoder/train1.csv")
b=read.csv("C:/Users/Akshay/Desktop/TopCoder/testing.csv")

a1=unique(a[,c(1,11:26)])
b1=unique(b[,c(1,11:26)])

a2=a[,c(1:10)]
b2=b[,c(1:10)]
t1=as.data.frame(table(a2$subjid))
t2=as.data.frame(table(b2$subjid))
names(t1)=c('subjid','Count')
names(t2)=c('subjid','Count')
a2=merge(a2,t1,by='subjid',all.x=T,all.y=T)
b2=merge(b2,t2,by='subjid',all.x=T,all.y=T)

temp1=subset(a2,Count=='5')
temp2=subset(a2,Count=='4')
temp3=subset(a2,Count=='3')
temp4=subset(a2,Count=='2')
temp5=subset(a2,Count=='1')

ad1=subset(a2,agedays=='1')
ad2=subset(a2,agedays=='123')
ad3=subset(a2,agedays=='366')
ad4=subset(a2,agedays=='1462')
ad5=subset(a2,agedays=='2558')

btemp1=subset(b2,Count=='5')
btemp2=subset(b2,Count=='4')
btemp3=subset(b2,Count=='3')
btemp4=subset(b2,Count=='2')
btemp5=subset(b2,Count=='1')

a3=subset(a,agedays=='2558')
a3=unique(a3[,c(1,27)])
