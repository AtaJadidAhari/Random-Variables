library(shiny)
library(shinythemes)
shinyUI(fluidPage(theme=shinytheme("united"),
                  headerPanel(
                    HTML('Random Variables
                         '
                    ), "Random Variables"
                    ),
                  fluidRow(
                    column(4,
                           wellPanel(
                             radioButtons("dist","Distribution type:",
                                          list(
                                            "Bernoulli"="bern","Binomial"="bin","Geometric"="geom","Poisson"="poi", # discrete
                                            "Exponential"="exp","Gamma"="gam", # continuous
                                            "Normal"="norm","Uniform"="unif"
                                          )
                             ),
                             sliderInput("n","Sample size:",1,1000,500),
                             uiOutput("dist1"),
                             uiOutput("dist2"),
                             uiOutput("dist3"),
                  
                             downloadButton("dldat", "Download Sample", class="btn-block btn-warning"),
                             fileInput(inputId="esUp", label = "Upload data to estimate", multiple = FALSE, accept = c(
                               "text",
                               "text/comma-separated-values,text/plain"
                               ),
                               width = NULL,
                                       buttonLabel = "Browse...", placeholder = "No file selected")
                           )
                    ),
                    column(8,
                           tabsetPanel(
                             tabPanel("Plot", plotOutput("plot",height="auto")),
                             tabPanel("Summary",verbatimTextOutput("summary")),
                             tabPanel("Estimation",
                                      
                                      plotOutput("esPlot", height = "auto")                                          
                                      )
                           )
                    )
                  )
                  ))
