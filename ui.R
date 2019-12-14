
library(shiny)
library(caret)
library(shiny)
library(readr)
library(ggplot2)
library(tidyverse)

heart2<-heart %>% select(age,chol,oldpeak,trestbps,thalach)

shinyUI(navbarPage("",
                   
                   tabPanel("Information Page",titlePanel("Heart Disease"),
                            includeMarkdown("information.Rmd"),
                            
                   ),
                   tabPanel("Prediction",
                            
                            titlePanel("Heart disease prediction"),
                            
                            
                            sidebarPanel(
                                
                                numericInput(inputId = "age", 
                                             label = "Age", 
                                             value = NULL, 
                                             min = 20, 
                                             max = 100),
                                
                                
                                selectInput("sex", "Gender:",
                                            c(" " = "NULL",
                                              "Male" = 1,
                                              "Female" = 0)),
                                
                                selectInput("fbs", "fasting blood sugar:",
                                            c(" " = "NULL",
                                              ">120 mg/dl" = 1,
                                              "<120 mg/dl" = 0)),
                                
                                selectInput("cp", "Chest pain type:",
                                            c(" " = "NULL",
                                              "None" = 4,
                                              "Mild" = 3,
                                              "Moderate" = 2,
                                              "Severe" = 1)),
                                
                                
                                numericInput(inputId = "trestbps", 
                                             label = "Resting systolic blood pressure", 
                                             value = NULL, 
                                             min = 40, 
                                             max = 220), 
                                
                                numericInput(inputId = "chol", 
                                             label = "Cholesterol", 
                                             value = NULL, 
                                             min = 80, 
                                             max = 650),  
                                
                                
                                selectInput("restecg", "Rest ECG:",
                                            c(" " = "NULL",
                                              "Normal" = 0,
                                              "Slight abnormality" =1,
                                              "Left ventricular hypertrophy" = 2
                                            )),
                                
                                 numericInput(inputId = "thalach", 
                                              label = "Maximum heart rate achieved", 
                                              value = NULL, 
                                              min = 50, 
                                             max = 220),
                                
                                selectInput("exang", "Exercise induced angina:",
                                            c(" " = "NULL",
                                              "Yes" = 1,
                                              "No" = 0
                                            )),
                                
                                numericInput(inputId = "oldpeak", 
                                             label = "ST depression induced by exercise relative to rest", 
                                             value = NULL,
                                             min = -5, 
                                             max = 10),  
                                
                                selectInput("slope", "Slope of the peak exercise ST segment:",
                                            c(" " = "NULL",
                                              "Upsloping" = 1,
                                              "Flat" = 2,
                                              "Downsloping" =3
                                            )),
                                
                                selectInput("ca", "Number of major vessels (0-3) colored by flourosopy:",
                                            c(" " = "NULL",
                                              "Low" = 0,
                                              "Mild" = 1,
                                              "Moderate" = 2,
                                              "High" = 3
                                            )),
                                
                                selectInput("thal", "Presence of defect:",
                                            c(" " = "NULL",
                                              "Normal" = 3,
                                              "Fixed defect" = 6,
                                              "Reversible defect" = 7
                                            )),
                                
                                actionButton("submit", "Submit")
                            ), #close side bar
                            
                            # Show result
                            mainPanel(
                                textOutput("text1"),
                                textOutput("text2"),
                                textOutput("text7"),
                                textOutput("text8")
                            )
                   ),
                   
                   tabPanel("Download",
                            fluidPage(
                                downloadLink("downloadData", "Download")
                            )
                   ),
                   
                   
                   
                   tabPanel("Clustering",
                            fluidPage(
                                pageWithSidebar(
                                    headerPanel('Heart Disease k-means clustering'),
                                    sidebarPanel(
                                        selectInput('xcol', 'X Variable', names(heart2)),
                                        selectInput('ycol', 'Y Variable', names(heart2),
                                                    selected=names(heart)[[4]]),
                                        
                                        numericInput('clusters', 'Cluster count', 3,
                                                     min = 1, max = 7),
                                        numericInput('iteration', '# of Iterations of Algorithm', value = 1, min = 1, max = 6)
                                    ),
                                    mainPanel(
                                        plotOutput('plot1'),
                                        plotOutput('plot2')
                                    )
                                ))
                   )
                   
                   ,
                   
                   tabPanel("Data Exploration",
                            fluidPage(
                                
                                column(4, selectInput("featureDisplay_x", 
                                                      label = h3("X-Axis Feature"), 
                                                      choices = feature.list,
                                                      selected = feature.list[1])),
                                column(4, selectInput("featureDisplay_y", 
                                                      label = h3("Y-Axis Feature"), 
                                                      choices = feature.list,
                                                      selected = feature.list[2]))
                                
                            ),
                            fluidRow(
                                column(4,
                                       plotOutput("distPlotA")
                                ),                              
                                column(4,
                                       plotOutput("distPlotB")      
                                ),
                                column(4,
                                       plotOutput("ScatterPlot")
                                       
                                       
                                )
                            )
                   )
                   ,
                   
                   tabPanel("Knn",
                            
                            # Application title
                            titlePanel("Classification of Heart Disease w/KNN"),
                            
                            # Sidebar with a slider input for number of bins
                            sidebarLayout(
                                sidebarPanel(
                                    sliderInput("k",
                                                "number of neighbors",
                                                min = 1,
                                                max = 20,
                                                value = 5),
                                    checkboxGroupInput("checkGroup", label = h3("Dataset Features"), 
                                                       choices = feature.list, inline = F,
                                                       selected = names(feature.list))
                                    
                                ),
                                
                                # Display KNN results
                                mainPanel(
                                    dataTableOutput('confusionMatrix'),
                                    verbatimTextOutput("value"),
                                    #includeMarkdown("ShinyAppDescription.Rmd")
                                    
                                )
                            )
                            
                   )
                   
                   
)

)

