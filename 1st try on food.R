<<<<<<< HEAD
<<<<<<< HEAD
### now we use dataset food on the master branch which has been extracted 
dim(food)
str(food)
food$State<-as.factor(food$State)

# Changed y variable name 
colnames(food)[8] <- c("Desert")

# Make Regions a factor 
food$Region <- as.factor(food$Region)
str(food)

# Make Urban a factor 
food$Urban<- as.factor(food$Urban)
str(food)
levels(food$Urban)

#Correlation Tests on x variables and y 
cor.test(food$POP2010, food$Desert)
cor.test(food$TractBlack, food$Desert)
cor.test(food$PovertyRate, food$Desert)
cor.test(food$TractSNAP, food$Desert)

# Mapping Census Data
library(tigris)
library(acs)
library(stringr) 


### sampling process removed, new "food.csv" has been uploaded
### Model Building
summary(lm(Desert~.-CensusTract-State,data = food))
glm_1sttry<-glm(Desert~.-CensusTract-State,data = food,family = "binomial")
summary(glm_1sttry)



#Eric ANOVA building test:

summary(food)

my_data <- food

set.seed(123)
dplyr::sample_n(my_data,10)

levels(my_data$State)

#Creating levels as seen under the my_data

my_data$State <- ordered(my_data$POP2010) 
                         levels = c("State", "Urban", "POP2010","OHU2010", "NUMGQTRS", "LILATracts_1And10]", "PovertyRate", "MedianFamilyIncome",
                                    "TractLOWI", "TractKids", "TractSenios", "TractWhite", "TractBlack", "TractAsian", "TractNHOPI", "TractAIAN", "TractOMultir", 
                                    "TractHispanic","TractHUNV","TractSNAP")
library(dplyr)
group_by(my_data, POP2010) %>%
  summarise(
        count = n(),
        mean = mean(Urban, na.rm = TRUE),
        sd = sd(Urban, na.rm = TRUE)
                           )
install.packages("ggpubr")


#Used this as a test, got a plot with some data points.. Not sure how to get more on the Y axis? (Not numeric here perhaps?)

library("ggpubr")
ggboxplot(my_data, x = "Urban", y = "POP2010", 
          color = "POP2010", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "POP2010", xlab = "Urban")



# Box plot
boxplot(POP2010 ~ Urban, data = my_data,
        xlab = "Urban", ylab = "POP2010",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))


#More Plots

library("gplots")
mean(POP2010 ~ Urban, data = my_data, frame = FALSE,
          xlab = "Urban", ylab = "POP2010",
          main="Mean Plot with 95% CI") 


# Compute the analysis of variance
res.aov <- aov(POP2010 ~ Urban, data = my_data)

# Summary of the analysis

summary(res.aov)

####

# Ashley Chi-Square (only works with two categotical variables)
chisq.test(food$Urban,y=food$Desert)
chisq.test(food$Region, y=food$Desert)
chisq.test(food$State, y=food$Desert)


=======
=======
>>>>>>> 90b8ce984ad4025db84a61ea10f398584d360ad0
### now we use dataset food on the master branch which has been extracted 
dim(food)
str(food)
food$State<-as.factor(food$State)
food$Region<-as.factor(food$Region)

### sampling process removed, new "food.csv" has been uploaded
### Model Building
summary(lm(LILATracts_1And10~.-CensusTract-State,data = food))
glm_1sttry<-glm(LILATracts_1And10~.-CensusTract-State,data = food,family = "binomial")
summary(glm_1sttry)



#Eric ANOVA building test:

summary(food)

my_data <- food

set.seed(123)
dplyr::sample_n(my_data,10)

levels(my_data$State)

#Creating levels as seen under the my_data

my_data$State <- ordered(my_data$POP2010) 
                         levels = c("State", "Urban", "POP2010","OHU2010", "NUMGQTRS", "LILATracts_1And10", "PovertyRate", "MedianFamilyIncome",
                                    "TractLOWI", "TractKids", "TractSenios", "TractWhite", "TractBlack", "TractAsian", "TractNHOPI", "TractAIAN", "TractOMultir", 
                                    "TractHispanic","TractHUNV","TractSNAP")
library(dplyr)
group_by(my_data, POP2010) %>%
  summarise(
        count = n(),
        mean = mean(Urban, na.rm = TRUE),
        sd = sd(Urban, na.rm = TRUE)
                           )
install.packages("ggpubr")


#Used this as a test, got a plot with some data points.. Not sure how to get more on the Y axis? (Not numeric here perhaps?)

library("ggpubr")
ggboxplot(my_data, x = "Urban", y = "POP2010", 
          color = "POP2010", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "POP2010", xlab = "Urban")



# Box plot
boxplot(POP2010 ~ Urban, data = my_data,
        xlab = "Urban", ylab = "POP2010",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))


#More Plots

library("gplots")
mean(POP2010 ~ Urban, data = my_data, frame = FALSE,
          xlab = "Urban", ylab = "POP2010",
          main="Mean Plot with 95% CI") 


# Compute the analysis of variance
res.aov <- aov(POP2010 ~ Urban, data = my_data)

# Summary of the analysis

summary(res.aov)

#### Di Anova copy and paste part

res.aov_1 <- aov(POP2010 ~ Urban, data = my_data)
res.aov_2 <- aov(POP2010 ~ State, data = my_data)
res.aov_3 <- aov(POP2010 ~ Region, data = my_data)
res.aov_4 <- aov(POP2010 ~ OHU2010, data = my_data)
res.aov_5 <- aov(POP2010 ~ NUMGQTRS, data = my_data)
res.aov_6 <- aov(POP2010 ~ LILATracts_1And10, data = my_data)
res.aov_7 <- aov(POP2010 ~ PovertyRate, data = my_data)
res.aov_8 <- aov(POP2010 ~ MedianFamilyIncome, data = my_data)
res.aov_9 <- aov(POP2010 ~ TractLOWI, data = my_data)
res.aov_10 <- aov(POP2010 ~ TractKids, data = my_data)
res.aov_11 <- aov(POP2010 ~ TractSeniors, data = my_data)
res.aov_12 <- aov(POP2010 ~ TractWhite, data = my_data)
res.aov_13 <- aov(POP2010 ~ TractBlack, data = my_data)
res.aov_14 <- aov(POP2010 ~ TractAsian, data = my_data)
res.aov_15 <- aov(POP2010 ~ TractNHOPI, data = my_data)
res.aov_16 <- aov(POP2010 ~ TractAIAN, data = my_data)
res.aov_17 <- aov(POP2010 ~ TractOMultir, data = my_data)
res.aov_18 <- aov(POP2010 ~ TractHispanic, data = my_data)
res.aov_19 <- aov(POP2010 ~ TractHUNV, data = my_data)
res.aov_20 <- aov(POP2010 ~ TractSNAP, data = my_data)
<<<<<<< HEAD
>>>>>>> d1e9fff705beed85ed6101aac89ff9b58d7d8343
=======


summary(res.aov_1)
summary(res.aov_2)
summary(res.aov_3)
summary(res.aov_4)
summary(res.aov_5)
summary(res.aov_6)
summary(res.aov_7)
summary(res.aov_8)
summary(res.aov_8)
summary(res.aov_9)
summary(res.aov_10)
summary(res.aov_11)
summary(res.aov_12)
summary(res.aov_13)
summary(res.aov_14)
summary(res.aov_15)
summary(res.aov_16)
summary(res.aov_17)
summary(res.aov_18)
summary(res.aov_19)
summary(res.aov_20)


### grahphs
boxplot(POP2010~Urban,data=food,main="Urban")

boxplot(POP2010~LILATracts_1And10,data=food,main="LILATracts_1And10")

t.test(y=food$POP2010,x=food$Urban)
### results indicate that there is a difference for POP2010 between urban=1 and urban=1
### mean(food$POP2010[food$Urban==0])
### mean(food$POP2010[food$Urban==1])

t.test(y=food$POP2010,x=food$LILATracts_1And10)

food_without_firstthree<-food[,c(-1,-2,-3)]
install.packages("car")
library(car)
cor(food_without_firstthree)
scatterplotMatrix(food_without_firstthree,spread=FALSE,smoother.args=list(lty=2),main="scatter plot matrix")
### this step took me like 5 minutes, didn't use pairs() here because it may need more time

















>>>>>>> 90b8ce984ad4025db84a61ea10f398584d360ad0
