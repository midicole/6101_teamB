### remove rows with NA values ###
housing_narm<-housing[complete.cases(housing),]
nrow(housing_narm)
str(housing_narm)




### remove Bedroom2, address, Coordinates, Suburb, Postcode
housing_narm<-housing_narm[,c(-1,-2,-10,-11,-18,-19)]
dim(housing_narm)

str(housing_narm)



housing_narm$Type<-as.factor(housing_narm$Type)
housing_narm$Method<-as.factor(housing_narm$Method)
housing_narm$SellerG<-as.factor(housing_narm$SellerG)
housing_narm$CouncilArea<-as.factor(housing_narm$CouncilArea)
housing_narm$Regionname<-as.factor((housing_narm$Regionname))
housing_narm$Price<-as.integer(housing_narm$Price)
### housing_narm$Date<-as.factor((housing_narm$Date))
### I am not sure if data should be converted into factor
str(housing_narm)

View(housing_narm)
### change format from 2e+06 to 2000000
options(scipen = 10)


### Graphical Method
hist(housing_narm$Price)
summary(housing_narm$Price)
cor(housing_narm[,c(1,3,7,8,9,10,11,12,15)])
plot(housing_narm$Rooms,housing_narm$Price,xlab="number of rooms",ylab="price")


summary(lm(Price~.,data=housing_narm_1))

summary(lm(Price~.,data=housing_narm))
