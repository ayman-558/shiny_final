library(tidyverse)
library(ggplot2)
library(DataExplorer)
library(faraway)
library(e1071)

#Read the data
heart<-read_csv("heart.csv")
heart2<-heart %>% select(age,chol,oldpeak,trestbps,thalach)

ds <- heart
# add names to dataset
names(ds) <- c( "age", "sex", "cp",
                "trestbps", "chol",
                "fbs", "restecg",
                "thalach","exang",
                "oldpeak","slope",
                "ca","thal","target")


feature.list <- list("age" = "age", "sex" ="sex",
                     "cp"= "cp","trestbps" = "trestbps",
                     "chol"="chol","fbs"="fbs",
                     "restecg"="restecg","thalach"="thalach",
                     "exang"="exang","oldpeak"="oldpeak",
                     "slope"="slope","ca"="ca","thal"="thal")

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


theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)