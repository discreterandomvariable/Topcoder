d1=read.csv("C:/Users/Akshay/Desktop/TopCoder/train1.csv")
d2=read.csv("C:/Users/Akshay/Desktop/TopCoder/testing.csv")
length(intersect(d1$subjid,d2$subjid))
t1=d1[which(d1$subjid%in%intersect(d1$subjid,d2$subjid)),]
t2=d2[which(d2$subjid%in%intersect(d1$subjid,d2$subjid)),]
f1=subset(d1,!is.na(d1$geniq))
f2=subset(t1,!is.na(t1$geniq))

aggregate(geniq ~ siteid + sexn,f1,mean)
aggregate(geniq ~ siteid + sexn,f2,mean)

tab=as.data.frame(table(d1$subjid))
tab1=as.data.frame(table(d2$subjid))
