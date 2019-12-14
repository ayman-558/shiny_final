library(tidyverse)
library(ggplot2)
library(DataExplorer)
library(faraway)
library(e1071)


#Read the data
heart<-read_csv("C:/Users/Ighwair/Desktop/shiny_final/shinayapp/heart.csv")
heart2<-heart %>% select(age,chol,oldpeak,trestbps,thalach)


#After eliminating non significant predictors 
heart$sex <- factor(heart$sex)
heart$cp <- factor(heart$cp)
heart$fbs <- factor(heart$fbs)
heart$restecg <- factor(heart$restecg)
heart$exang <- factor(heart$exang)
heart$ca <- factor(heart$ca)
#heart$thal <- factor(heart$thal)
heart$target <- factor(heart$target)

#describe(heart)

#Introduction

#Explaining the Variables 

#Classification tree 


# Check NAs
# no mssing data

#Change variables to factor 

heart$sex <- factor(heart$sex)
heart$cp <- factor(heart$cp)
heart$fbs <- factor(heart$fbs)
heart$restecg <- factor(heart$restecg)
heart$exang <- factor(heart$exang)
heart$ca <- factor(heart$ca)
heart$thal <- factor(heart$thal)
heart$target <- factor(heart$target)

#Creating a test and training sets
set.seed(90)
train <- sample(1:nrow(heart), size = nrow(heart)*0.8)
test <- dplyr::setdiff(1:nrow(heart), train)

heartTrain <- heart[train, ]
heartTest <- heart[test, ]
