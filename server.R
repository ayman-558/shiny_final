library(shiny)
library(caret)
library(ggplot2)
library(DT)
library(tidyverse)
library(DataExplorer)
library(class) 
library(e1071)



heart$sex <- factor(heart$sex)
heart$cp <- factor(heart$cp)
#heart$cp = factor(heart$cp, levels = c(1,2,3,4))
heart$fbs <- factor(heart$fbs)
heart$restecg <- factor(heart$restecg)
heart$exang <- factor(heart$exang)
heart$ca <- factor(heart$ca)
heart$thal <- factor(heart$thal)
#heart$thal = factor(heart$thal, levels = c( 3,6,7))
heart$target <- factor(heart$target)

heartTest$sex <- factor(heartTest$sex)
heartTest$cp <- factor(heartTest$cp)
#heart$cp = factor(heart$cp, levels = c(1,2,3,4))
heartTest$fbs <- factor(heartTest$fbs)
heartTest$restecg <- factor(heartTest$restecg)
heartTest$exang <- factor(heartTest$exang)
heartTest$ca <- factor(heartTest$ca)
heartTest$thal <- factor(heartTest$thal)
#heart$thal = factor(heart$thal, levels = c( 3,6,7))
heartTest$target <- factor(heartTest$target)


heartTrain$sex <- factor(heartTrain$sex)
heartTrain$cp <- factor(heartTrain$cp)
#heart$cp = factor(heart$cp, levels = c(1,2,3,4))
heartTrain$fbs <- factor(heartTrain$fbs)
heartTrain$restecg <- factor(heartTrain$restecg)
heartTrain$exang <- factor(heartTrain$exang)
heartTrain$ca <- factor(heartTrain$ca)
heartTrain$thal <- factor(heartTrain$thal)
#heart$thal = factor(heart$thal, levels = c( 3,6,7))
heartTrain$target <- factor(heartTrain$target)
heartTrain$thalach<-as.numeric(heartTrain$target)






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

# keep an original copy of the data to retain classes
# for displays
original.ds <- ds
ds <- na.omit(ds)
# change the class of all columns to numeric
ds <- as.data.frame(apply(ds, 2, as.numeric))

# remove na/missing values (original data shows as ?)

# all values of num > 0 are cases of heart disease 
# as per the data descriptions at the uci repository
ds$target <- factor(ds$target, levels = c(0,1), labels = c("negative", "positive"))

# standardize/normalize the data
# for knn we want to ensure the distance is normalized for all 
# features

# standardize all point except the response variable
standardized.X <- scale(ds[,-14])
set.seed(55)

# create training and test sets
training.index <- createDataPartition(ds$target, p = .8,list = F)
train.X <- standardized.X[training.index,]
test.X  <- standardized.X[-training.index,]
train.Y <- ds$target[training.index]
test.Y <- ds$target[-training.index]

# table settings ####

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


class(heartTrain$thalach)

server <- function(input, output) {
    # options(shiny.maxRequestSize = 800*1024^2)
    call_me <- eventReactive(input$submit, {
        
        # age = (input$age)
        # sex = (input$sex)
        # cp = (input$cp)
        # fbs=(input$fbs)
        # trestbps = (input$trestbps)
        # chol = (input$chol)
        # restecg = (input$restecg)
        #  thalach = (input$thalach)
        #  exang = (input$exang)
        #  oldpeak = (input$oldpeak)
        #  slope = (input$slope)
        # ca = (input$ca)
        #  thal = (input$thal)
        
        
        # data_input <- data.frame(input$age, factor(input$sex), factor(input$cp), input$trestbps, input$chol, factor(input$fbs), factor(input$restecg), input$thalach, factor(input$exang), input$oldpeak, input$slope, factor(input$ca), factor(input$thal))
        #data_input <- data.frame(heart$age, factor(heart$sex), factor(heart$cp), heart$trestbps, heart$chol, factor(heart$fbs), factor(heart$restecg), heart$thalach, factor(heart$exang), heart$oldpeak, heart$slope, factor(heart$ca), na.omit(factor(heart$thal)))
        #data_input<-heartTest
        #data_input<-data.frame(heart)
        
        #PREPROCESS INPUT DATA
        ##CONVERT CONTINUOUS TO CATEGORICAL 
        # breaks <- c(0,30, 35, 40, 50, 60, 70, 80, 90, 100, Inf)
        # age_transformed <- cut(data_input$age,breaks = breaks, right=FALSE, labels=c(1:10))
        # breaks1 <- c(0, 110, 120, 130, 140, 150, 160, Inf)
        # trestbps_transformed <-cut(data_input$trestbps,breaks = breaks1, right=FALSE, labels=c(1:7))
        # breaks2 <- c(0, 180, 200, 220, 240, 260, 280, 300, Inf)
        # chol_transformed <-cut(data_input$chol,breaks = breaks2, right=FALSE, labels=c(1:8))
        # breaks3 <- c(0, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, Inf)
        # thalach_transformed <-cut(data_input$thalach,breaks = breaks3, right=FALSE, labels=c(1:13))
        # breaks4 <- c(-10, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, Inf)
        # oldpeak_transformed <- cut(data_input$oldpeak,breaks = breaks4, right=FALSE, labels=c(1:14))
        
        ##REPLACE DATA WITH NEW TRANSFORMED VALUES
        # for(i in 1:ncol(data_input))
        # {
        #   data_input$age <- age_transformed
        #   data_input$trestbps <- trestbps_transformed
        #   data_input$chol <- chol_transformed
        #   data_input$thalach <- thalach_transformed
        #   data_input$oldpeak <- oldpeak_transformed
        # }
        #glm_fit <- nnet::multinom(target ~ ., data=heartTrain)
        glm_fit <- glm(target ~ ., data=heartTrain,family = binomial(link = "logit"))
        
        #glm_fit <- nnet::multinom(heartTrain$target ~ ., data=heartTrain)
        #glm_fit <- glm(heartTrain$target ~ ., data=heartTrain,family = binomial)
        
        #pred <- glm_fit %>% predict(input$data_input)
        #pred
        
        
        test<-round(predict(glm_fit, type="response", 
                            newdata=data.frame("age" = input$age,"sex" = input$sex[1], 
                                               "cp" = input$cp, 
                                               "fbs" = input$fbs,"trestbps" = input$trestbps, 
                                               "restecg" = input$restecg),"chol" = input$chol,
                            "thalach" = input$thalach,"exang" = input$exang, "oldpeak" = input$oldpeak,
                            "slope" = input$slope, "ca" = input$ca, "thal" = input$thal))
        
        
        
        output$text7 <- renderText({   
            if( test() <1)
                paste("There's a risk of heart disease")
            else
                NULL
            
        }) 
        output$text8 <- renderText({   
            if( test() > 0)
                NULL
            else
                paste("You have nothing to worry for now!")
            
        }) 
        
    })
    
    output$text1 <- renderText({"The heart disease prediction is   "})
    output$text2 <- renderText(paste(call_me()))
    
    data<-heart
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("Heart_Disease", ".csv", sep="")
        },
        content = function(file) {
            write.csv(data, file)
        }
    )
    
    output$ex1 <- renderUI({
        withMathJax(helpText('Dynamic output 1:  $$\\alpha^2$$'))
    })
    
    selectedData <- reactive({
        heart[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
        set.seed(10)
        kmeans(selectedData(), input$clusters, iter.max = input$iteration, algorithm = "MacQueen")
    })
    
    output$plot1 <- renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    output$plot2 <- renderPlot({
        hierClust <- hclust(dist(data.frame(heart$thal, heart$chol)))
        plot(hierClust, xlab = "")
        
    })
    
    observe({
        set.seed(1)
        knn.pred <- knn(data.frame(train.X[,input$checkGroup]),
                        data.frame(test.X[,input$checkGroup]),
                        train.Y, k = input$k)
        
        
        output$value <- renderText({ paste("Accuracy of Classification = ",accuracy(test.Y,knn.pred)) })
        output$confusionMatrix <- renderDataTable({
            # modify this to show title - confusion matrix
            # /false positive/positive false negative/negative
            true.positive    <- sum(knn.pred == "positive" & test.Y == "positive")
            false.positive   <- sum(knn.pred == "negative" & test.Y == "positive")
            true.negative    <- sum(knn.pred == "negative" & test.Y == "negative")
            false.negative   <- sum(knn.pred == "positive" & test.Y == "negative")
            row.names <- c("Prediction - FALSE", "Prediction - TRUE" )
            col.names <- c("Reference - FALSE", "Reference - TRUE")
            cbind(Outcome = row.names, as.data.frame(matrix( 
                c(true.negative, false.negative, false.positive, true.positive) ,
                nrow = 2, ncol = 2, dimnames = list(row.names, col.names))))
        }, options = table.settings
        )
        
    })
    
    observe({
        input_feature_x <- as.symbol(input$featureDisplay_x)
        input_feature_y <- as.symbol(input$featureDisplay_y)
        
        output$distPlotA <- renderPlot({
            ggplot(ds, aes_string(input$featureDisplay_x, fill = "factor(target)")) + 
                geom_histogram(position = "dodge") + 
                labs(x = input$featureDisplay_x,
                     y = "Count") + fte_theme() +
                scale_fill_manual(guide = F,values=c("#7A99AC", "#E4002B")) 
            
        })
        
        output$distPlotB <- renderPlot({
            ggplot(ds, aes_string(input$featureDisplay_y, 
                                  fill = "factor(target)")) + 
                geom_histogram(position = "dodge") +
                labs(x = input$featureDisplay_y,
                     y = "Count") + fte_theme() +
                scale_fill_manual(guide = F,values=c("#7A99AC", "#E4002B")) 
            
            
        })
        
        output$ScatterPlot <- renderPlot({
            ggplot(ds, aes_string(x = input$featureDisplay_x, 
                                  y = input$featureDisplay_y, 
                                  color = "factor(target)")) + 
                geom_point(size = 4, position = position_jitter(w = 0.1, h = 0.1)) + 
                labs(x = input$featureDisplay_x,
                     y = input$featureDisplay_y) +
                fte_theme() + 
                scale_color_manual(name = "Heart Disease",values=c("#7A99AC", "#E4002B")) 
        })          
        
    })
    
    
}


# setwd("C:/Users/Ighwair/Desktop/shiny_final")
