# Installing packages
install.packages("datarium") 
install.packages("qqplotr") 
install.packages("ggplot2")
install.packages("psych")
install.packages("readxl")
install.packages("corrplot")

# Loading packages
library(tidyverse)
library(readr)
library(ggplot2)
library(corrplot)
library(psych)
library(car)
library(readxl)
library(caret)


# Load data
red_wine <- read_excel("winequality-red.xlsx")
white_wine <- read_excel("winequality-white.xlsx")

# Add type variable
red_wine$type <- "red"
white_wine$type <- "white"

# Combining datasets
wine <- rbind(red_wine, white_wine)

#converting type into factor
wine$type <- as.factor(wine$type)

#Data exploration
head(wine)
summary(wine)

##Check for missing values
colSums(is.na(wine))

#Renaming the column
wine <- wine %>% 
  rename(
    citric_acid = `citric acid`,
    total_sul = `total sulfur dioxide`,
    fixed_sul = `free sulfur dioxide`,
    vo_acidity = `volatile acidity`,
    fix_acidity = `fixed acidity`,
    res_sugar = `residual sugar`
  )

#Distribution of Quality
ggplot(wine, aes(x = quality)) +
  geom_histogram(binwidth = 1, color='black', fill='skyblue') +
  labs(title = "Distribution of Wine Quality", x = "Quality score", y = "frequency")

# Histogram of quality by type
ggplot(wine, aes(x = quality, fill = type)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Distribution of Wine Quality by Type", x = "Quality score", y = "frequency")

# Use box plot to compare red and white by type
boxplot(quality ~ type, data=wine, names=c("red", "white"), 
        main="Quality ratings for Red and White wine")

#Correlation Analysis
#Correlation matrix
cont_var <- wine %>% select(-type)
round(cor(cont_var, method = "spearman"), digit =2)

#visualizing the correlation matrix
corrplot(cor(cont_var, method = "spearman"))

#correlation by wine type
#correlation of the red
cor_red <- wine %>%
  filter(type == "red") %>%
  select(where(is.numeric)) %>%
  cor()
round((cor_red), digits = 2)

#Correlation of the white
cor_white <- wine %>%
  filter(type == "white") %>%
  select(where(is.numeric)) %>%
  cor()
round((cor_white), digits = 2)

#Regression analysis
#Using the Lm
model_1 <- lm(quality~alcohol, wine)
summary.lm(model_1)

#plotting on a scatter plot
plot( quality ~ alcohol, wine,
     col="red",
     main = "Regression:Quality & Alcohol")
#adding the reg line
abline(model_1, col="black")

#Residuals' Independence
plot(model_1, 1)

#Normality of residuals
plot(model_1, 2)

#Homoscedasticity
plot(model_1, 3)

#The MLR
#model_2
model_2 <- lm(quality ~ alcohol + density, wine)
summary.lm(model_2)

#model_3
model_3 <- lm(quality ~ alcohol + chlorides, wine)
summary.lm(model_3)

#model_4
model_4 <- lm(quality ~ alcohol + vo_acidity, wine)
summary.lm(model_4)

#model_5
model_5 <- lm(quality ~ alcohol + chlorides + vo_acidity, wine)
summary.lm(model_5)

#model_6
model_6 <- lm(quality ~ alcohol + density + vo_acidity, wine)
summary.lm(model_6)

#Assumption
#linearity
pairs(wine[,c(12,11,8,2)], lower.panel = NULL, pch = 19,cex = 0.2) 

# Residuals’ Independence 
plot(model_6, 1)

#Normality of residuals:
plot(model_6, 2)

#Equal variances of the residuals (Homoscedasticity)
plot(model_6, 3)

#multicollinearity
vif(model_6)

#model2 <- lm( quality ~ type + alcohol + vo_acidity + chlorides, wine)
#summary.lm(model2)

#Logistic Regression
#unique values of quality
unique(wine$quality)

#counts of the unique values
table(wine$quality)

#classifying the rating to 0 and 1
wine$quality_binary <- ifelse(wine$quality >= 6, 1, 0)

#backward stepwise
#model1
model_logiatic_1 <- glm(quality_binary ~ alcohol + fix_acidity + vo_acidity 
                        + citric_acid + res_sugar + fixed_sul + total_sul 
                        + density + pH + sulphates + chlorides + type, 
                        data = wine, family = "binomial")
summary(model_logiatic_1)

#model2
model_logiatic_2 <- glm(quality_binary ~ alcohol + fix_acidity + vo_acidity 
                        + res_sugar + fixed_sul + total_sul 
                        + density + pH + sulphates + type, 
                        data = wine, family = "binomial")
summary(model_logiatic_2)

#model3
model_logiatic_3 <- glm(quality_binary ~ alcohol + vo_acidity 
                        + res_sugar + fixed_sul + total_sul 
                        + density + pH + sulphates + type, 
                        data = wine, family = "binomial")
summary(model_logiatic_3)


#Variance Importance
Imp <- varImp(model_logiatic_3, scale = FALSE)
Imp

#model4
model_logiatic_4 <- glm(quality_binary ~ alcohol + vo_acidity, 
                        data = wine, family = "binomial")
summary(model_logiatic_4)

#Assumptions
#linearity
#first
probs <- predict(model_logiatic_1, data=wine,type="response")  
wine$probs <- probs 

#second
logits <- log(probs/(1-probs)) 
wine$logits <- logits 

#viewing the scatterplot
pairs(wine[,c(16,11,2,8)], lower.panel = NULL, upper.panel = panel.smooth, 
      pch = 19,cex = 0.2)

# Influential values check
plot(model_logiatic_1, which = 4, id.n = 3)

#Multicollinearity
vif(model_logiatic_1)

#density, res_sugar and type voilates the assumption 
#rerun the model

#model 5
model_logiatic_5 <- glm(quality_binary ~ alcohol + fix_acidity + vo_acidity 
                        + citric_acid + fixed_sul + total_sul + pH + sulphates + chlorides, 
data = wine, family = "binomial")
summary(model_logiatic_5)

#model 6
model_logiatic_6 <- glm(quality_binary ~ alcohol + vo_acidity + fixed_sul + total_sul + sulphates + chlorides, 
                        data = wine, family = "binomial")
summary(model_logiatic_6)

#first
probs <- predict(model_logiatic_6, data=wine,type="response")  
wine$probs <- probs 

#second
logits <- log(probs/(1-probs)) 
wine$logits <- logits 

data.frame(colnames(wine)) 

#viewing the scatterplot
pairs(wine[,c(16,11,2,6)], lower.panel = NULL, upper.panel = panel.smooth, 
      pch = 19,cex = 0.2)

# Influential values check
plot(model_logiatic_6, which = 4, id.n = 3)

#Multicolinearity
vif(model_logiatic_6) 

#plotting the logistic regression curve
ggplot(wine, aes(x=alcohol+vo_acidity, y=quality_binary)) + geom_point() + 
  stat_smooth(method="glm", color="green", se=FALSE, 
              method.args = list(family=binomial)) 

#Hypothesis testing
#first testing
t.test(quality ~ type, data = wine)

#second testing
t.test(alcohol ~ type, data = wine)

#Chi-squared
# Creating contingency table
tbl <- table(wine$type, wine$quality_binary)
tbl

# Run Chi-square test of independence
chi_test <- chisq.test(tbl)
chi_test
