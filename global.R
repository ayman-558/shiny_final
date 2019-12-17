library(tidyverse)
library(ggplot2)
library(DataExplorer)
library(faraway)
library(e1071)
library(Metrics)
library(shiny)
library(caret)
library(DT)
library(class) 
library(scales)




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


table.settings <- list(searching = F, pageLength = 5, bLengthChange = F,
                       bPaginate = F, bInfo = F )


# define theme for ggplots ####
fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[9]
  color.axis.title = palette[9]
  color.title = palette[9]
  text.size <- 14
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=text.size,color=color.axis.title)) +
    theme(legend.title = element_text(size=text.size,color=color.axis.title)) +
    theme(legend.position = "bottom") +
    theme(legend.direction = "vertical") +
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=text.size, vjust=1.25)) +
    theme(axis.text.x=element_text(size=text.size,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=text.size,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=text.size,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=text.size,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

