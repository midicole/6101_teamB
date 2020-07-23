food<-food[,c("CensusTract","State","Urban","POP2010","OHU2010","NUMGQTRS","LILATracts_1And10","PovertyRate","MedianFamilyIncome","TractLOWI","TractKids","TractSeniors","TractWhite","TractBlack","TractAsian","TractNHOPI","TractAIAN","TractOMultir","TractHispanic","TractHUNV","TractSNAP")]
dim(food)
str(food)
install.packages("sampling")
library("sampling")
food$State<-as.factor(food$State)
### 
set.seed(123)
food_drawn<-strata(food,"State",size=c(120,20,150,70,800,120,80,20,20,420,200,30,30,310,150,80,80,110,110,40,140,150,280,130,70,140,30,50,70,30,200,50,490,220,20,300,100,80,320,20,110,20,150,530,60,20,190,150,50,140,10),method = "srswor")
dim(food_drawn)
food_drawn$ID_unit
food_drawn<-food[food_drawn$ID_unit,]
dim(food_drawn)
str(food_drawn)
summary(lm(LILATracts_1And10~.-CensusTract-State,data = food_drawn))
glm_1sttry<-glm(LILATracts_1And10~.-CensusTract-State,data = food_drawn,family = "binomial")
summary(glm_1sttry)
