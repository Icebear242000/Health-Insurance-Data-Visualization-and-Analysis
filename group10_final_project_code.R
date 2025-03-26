# importing required data sets
library(tidyverse)
library(readxl)
library(GGally)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ISLR)
library(leaps)
library(scales)  

insurance_data <- read.csv("insurance.csv")

# Check the structure of the dataset
str(insurance_data)

# Check the first few rows of the dataset
head(insurance_data)

#scatter plot with points colored by gender and a regression line for BMI vs. charges
ggplot(insurance_data, aes(x = bmi, y = charges, color = factor(sex))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Insurance Charges vs. BMI",
       x = "BMI",
       y = "Insurance Charges",
       color = "Gender")

#grouped bar plot to compare average insurance charges for smokers and non-smokers by gender
ggplot(insurance_data, aes(x = factor(smoker), y = charges, fill = factor(sex))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Insurance Charges by Smoker Status and Gender",
       x = "Smoker",
       y = "Average Insurance Charges",
       fill = "Gender") +
  scale_fill_manual(values = c("lightblue", "lightcoral"))  


##bar plot to visualize average insurance charges across different regions

ggplot(insurance_data, aes(x = region, y = charges, fill = region)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Insurance Charges Across Different Regions",
       x = "Region",
       y = "Average Insurance Charges (in thousands)",
       fill = "Region") +
  scale_fill_manual(values = c("lightblue", "lightcoral", "lightgreen", "lightpink")) +  
  scale_y_continuous(labels = dollar_format(scale = 1e-3))  

#box plot to visualize the distribution of insurance charges across different regions

ggplot(insurance_data, aes(x = region, y = charges, fill = region)) +
  geom_boxplot() +
  labs(title = "Insurance Charges Across Different Regions",
       x = "Region",
       y = "Insurance Charges(in thousands)",
       fill = "Region") +
  scale_fill_manual(values = c("lightblue", "lightcoral", "lightgreen", "lightpink")) +
  scale_y_continuous(labels = dollar_format(scale = 1e-3))


# Predict charges using the linear regression model
insurance_data$predicted_charges <- predict(model, newdata = insurance_data)

# Create a scatter plot with regression line
ggplot(insurance_data, aes(x = charges, y = predicted_charges)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Observed vs. Predicted Charges",
       x = "Observed Charges",
       y = "Predicted Charges") +
  theme_minimal()
# Pair plot for BMI vs. Charges
ggplot(insurance_data, aes(x = bmi, y = charges, color = smoker)) +
  geom_point() +
  facet_grid(. ~ region) +
  labs(title = "Pair Plot of BMI vs. Charges",
       x = "BMI",
       y = "Insurance Charges",
       color = "Smoker Status")

# Group the data by the number of children and calculate the mean insurance cost for each group
avg_charges_by_children <- aggregate(charges ~ children, data = insurance_data, FUN = mean)

# Plotting the bar graph
ggplot(avg_charges_by_children, aes(x = as.factor(children), y = charges)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Insurance Cost by Number of Children",
       x = "Number of Children",
       y = "Average Insurance Cost") +
  theme_minimal() +
  theme(axis.text.x = element_text( hjust = 1))

# Create a box plot for BMI and sex
ggplot(insurance_data, aes(x = sex, y = bmi, fill = sex)) +
  geom_boxplot() +
  labs(title = "Box Plot of BMI by Sex",
       x = "Sex",
       y = "BMI") +
  theme_minimal()

# Statistical Modelling

df <- read.csv('insurance.csv')
View(df)

head(df)

summary(df)

# Numerical Encoding
df$sex <- as.factor(df$sex)
df$smoker <- as.factor(df$smoker)
df$region <- as.factor(df$region)

df$sex <- as.integer(df$sex)
df$smoker <- as.integer(df$smoker)
df$region <- as.integer(df$region)

summary(df)
cor(df)

windows()
ggpairs(df, lower = list(continuous = wrap("points", alpha = 0.5, size = 1)))

# Linear Regression Model

lm.all <- lm(log(charges)~., data = df)
model_summ <- summary(lm.all)
model_summ

residFit(lm.all, outlierVal = 9)

scaleLocation(lm.all, outlierVal = 9)

normalQQ(lm.all, outlierVal = 9)

sum(model_summ$residuals)

mean(model_summ$residuals^2)

par(mfrow=c(2,2)) 
plot(lm.all)
coef(lm.all)

# Best Subset Selection

regfit_full <- regsubsets(log(charges)~., df)
summary(regfit_full)

# Regression Tree

set.seed(1)
train = sample(1:nrow(df), nrow(df)/2)
set.seed(543)
rpart.ins=rpart(log(charges)~.,data = df[train,],
                method="anova", cp = 0.001)

par(mfrow=c(1,1))
rpart.plot(rpart.ins, cex = 0.4)

plotcp(rpart.ins)
printcp(rpart.ins)

# Pruned Regression Tree

rpart.plot.1se <- prune(rpart.ins,cp=0.0084931)
rpart.plot(rpart.plot.1se , extra=1,
           roundint=FALSE, digits=3,
           main="1-se Insurance regression tree")

# Random Forest Regression

rf.ins <- randomForest(log(charges)~., data = df, ntree = 100, importance = TRUE)
rf.ins
importance(rf.ins)

varImpPlot(rf.ins)
