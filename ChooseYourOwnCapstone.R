## Daniel Constable
## United States
## HarvardX : Capstone Project

#### INTRODUCTION ####

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

#Load the libraries
library(tidyverse)
library(caret)
library(data.table)
library(dslabs)
library(dplyr)
library(ggthemes)

income_data <- read.csv("https://raw.githubusercontent.com/SlightNorth/adultcensus/main/adult.csv")

# check the structure of the data
str(income_data)

# check the dimensions of the data

dim(income_data)

# check the head of the data
head(income_data)

# check working class

income_data %>% group_by(workclass) %>%
  summarize(n=n()) 
  
income_data %>% group_by(workclass) %>% ggplot(aes(workclass)) + geom_bar() + theme_stata() + scale_x_discrete(guide = guide_axis(n.dodge=3))

# check eduation

income_data %>% group_by(education) %>%
  summarize(n=n()) 

income_data %>% group_by(education) %>% ggplot(aes(education)) + geom_bar() + theme_stata() + scale_x_discrete(guide = guide_axis(n.dodge=3))


# check marital status

income_data %>% group_by(marital.status) %>%
  summarize(n=n()) 

income_data %>% group_by(marital.status) %>% ggplot(aes(marital.status)) + geom_bar() + theme_stata() + scale_x_discrete(guide = guide_axis(n.dodge=3))

# check occupation

income_data %>% group_by(occupation) %>%
  summarize(n=n())

income_data %>% group_by(occupation) %>% ggplot(aes(occupation)) + geom_bar() + theme_stata() + scale_x_discrete(guide = guide_axis(n.dodge=3))

# check relationships

income_data %>% group_by(relationship) %>%
  summarize(n=n())

income_data %>% group_by(relationship) %>% ggplot(aes(relationship)) + geom_bar() + theme_stata() + scale_x_discrete(guide = guide_axis(n.dodge=3))


# check race

income_data %>% group_by(race) %>%
  summarize(n=n()) 

income_data %>% group_by(race) %>% ggplot(aes(race)) + geom_bar() + theme_stata() + scale_x_discrete(guide = guide_axis(n.dodge=3))

# check sex

income_data %>% group_by(sex) %>%
  summarize(n=n()) 

income_data %>% group_by(sex) %>% ggplot(aes(sex)) + geom_bar() + theme_stata() + scale_x_discrete(guide = guide_axis(n.dodge=3))


# check hours per week

income_data %>% group_by(hours.per.week) %>%
  summarize(n=n())

income_data %>% group_by(hours.per.week) %>% ggplot(aes(hours.per.week)) + geom_bar() + theme_stata() + scale_x_discrete(guide = guide_axis(n.dodge=3))

# check native country

income_data %>% group_by(native.country) %>%
  summarize(n=n()) 

income_data %>% group_by(native.country) %>% ggplot(aes(native.country)) + geom_bar() + theme_stata() + scale_x_discrete(guide = guide_axis(n.dodge=3))


# check income

income_data %>% group_by(income) %>%
  summarize(n=n()) 

income_data %>% group_by(income) %>% ggplot(aes(income)) + geom_bar() + theme_stata() + scale_x_discrete(guide = guide_axis(n.dodge=3))

income_data %>% group_by(race) %>% ggplot(aes(race, income)) + geom_() + theme_stata() + scale_x_discrete(guide = guide_axis(n.dodge=3))

sum(income_data$income == ">50K")

# define the outcome and predictors

y <- income_data$income


# generate training and test sets
set.seed(2, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- income_data[test_index, ]
train_set <- income_data[-test_index, ]

# check logistic regression

summary(glm(income ~ ., family = binomial(), income_data))

# use Decision Tree
library(rpart)
library(rpart.plot)
fit_tree <- rpart(income~.,data=train_set,method = 'class')
print(fit_tree)
rpart.plot(fit_tree, box.col=c("red", "blue"))
decision_prediction<- predict(fit_tree,newdata=test_set[-15],type = 'class')
TreeAcu<-confusionMatrix(decision_prediction,test_set$income)$overall[1]
TreeAcu

# use Random Forest

library("randomForest")
random_forest <- randomForest(income~., data = train_set, method = "class", ntree = 500, do.trace = 100)

test_set$rf.predicted.income <- predict(random_forest, test_set, type = "class")
test_set$rf.predicted.income