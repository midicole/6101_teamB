### now we use dataset food on the master branch which has been extracted 
dim(food)
str(food)
food$State<-as.factor(food$State)

### sampling process removed, new "food.csv" has been uploaded
### Model Building
summary(lm(LILATracts_1And10~.-CensusTract-State,data = food))
glm_1sttry<-glm(LILATracts_1And10~.-CensusTract-State,data = food,family = "binomial")
summary(glm_1sttry)
