### now we use dataset food on the master branch which has been extracted 
dim(food)
str(food)
food$State<-as.factor(food$State)

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

####

# Ashley Chi-Square (only works with two categotical variables)
chisq.test(food$Urban,y=food$LILATracts_1And10)
chisq.test(food$State, y=food$LILATracts_1And10)


# Making Regions into a dummy Variable 
state.regions <- factor(food.access$Region, levels= c(""))
