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
