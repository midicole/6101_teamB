housing_narm<-housing[complete.cases(housing),]
nrow(housing_narm)
str(housing_narm)
housing_narm$Suburb<-as.factor(housing_narm$Suburb)
housing_narm$CouncilArea<-as.factor(housing_narm$CouncilArea)
housing_narm<-housing_narm[,c(-2,-7,-8,-10,-18,-19,-20,-21)]
dim(housing_narm)
housing_narm$Type<-as.factor(housing_narm$Type)
housing_narm$Method<-as.factor(housing_narm$Method)
housing_narm$Type<-as.factor(housing_narm$Type)
View(housing_narm_1)
housing_narm_1<-housing_narm[,-1]
summary(lm(Price~.,data=housing_narm_1))

summary(lm(Price~.,data=housing_narm))
