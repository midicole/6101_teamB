install.packages("Ecdat")
library(Ecdat)
data("Workinghours")
help("Workinghours")

### check the dimension of data ###
dim(Workinghours)

### check structure of data ###
str(Workinghours)
summary(Workinghours)



by(Workinghours,Workinghours$nonwhite,summary)
par(mfrow=c(1,3))
boxplot(hours ~ nonwhite,data=Workinghours,main="hours vs race")
boxplot(hours ~ owned,data=Workinghours,main="hours vs owned")
boxplot(hours ~ mortgage,data=Workinghours,main="hours vs mortgage")

t.test(hours ~ nonwhite, Workinghours,var.equal=T)
t.test(hours ~ owned, Workinghours,var.equal=T)
t.test(hours ~ mortgage, Workinghours,var.equal=T)

### use violin plot here to double check our conclusion about 0 and 1
install.packages("vioplot")
library(vioplot)
x1<-Workinghours$hours[Workinghours$nonwhite==0]
x2<-Workinghours$hours[Workinghours$nonwhite==1]
par(mfrow=c(1,1))
vioplot(x1,x2,names = c("nonwhite 0","nonwhite 1"),col = "gold")

y1<-Workinghours$hours[Workinghours$owned==0]
y2<-Workinghours$hours[Workinghours$owned==1]
par(mfrow=c(1,1))
vioplot(y1,y2,names = c("owned 0","owned 1"),col = "gold")

z1<-Workinghours$hours[Workinghours$mortgage==0]
z2<-Workinghours$hours[Workinghours$mortgage==1]
par(mfrow=c(1,1))
vioplot(z1,z2,names = c("mortgage 0","mortgage 1"),col = "gold")

summary(lm(hours~.,data=Workinghours))
