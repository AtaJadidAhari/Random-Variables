library(shinydashboard)





###################

header <- dashboardHeader(
  title = "R Project",
  dropdownMenu(
    type = "notifications", 
    icon = icon("envelope"),
    badgeStatus = NULL,
    headerText = "Contact Us:",
    
    notificationItem("Ali Salmani: alisalmani200149 at gmail", icon = icon("envelope")),
    notificationItem("others", icon = icon("envelope"))
                     
  )
)

sidebar <- dashboardSidebar(
  
  
  sidebarMenu(
    id = "tabs",
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Charts", tabName = "charts", icon = icon("bar-chart-o"),
             
             menuSubItem(text = "Uniform", tabName = "uniform", icon = NULL),
             menuSubItem(text = "Bernouli", tabName = "bernouli", icon = NULL),
             menuSubItem(text = "Binomial", tabName = "binomial", icon = NULL),
             menuSubItem(text = "Geometric", tabName = "geometric", icon = NULL),
             menuSubItem(text = "Exponential", tabName = "exponential", icon = NULL),
             menuSubItem(text = "Gamma", tabName = "gamma", icon = NULL),
             menuSubItem(text = "Poisson", tabName = "poisson", icon = NULL),
             menuSubItem(text = "Normal", tabName = "normal", icon = NULL)
             
    ),
    
    menuItem("About Us", tabName = "aboutUs", icon = icon("users"))
  )
)

body <- dashboardBody(
  
  tags$div(class = "se-pre-con"),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "script.js"),
    tags$script(src = "http://cdnjs.cloudflare.com/ajax/libs/modernizr/2.8.2/modernizr.js")
  ),
  
  
  
  
  
  
  
  tabItems(
    tabItem(tabName = "uniform",
            
            fluidRow(
              box(title = "Input", status = "primary", "Box content")
            ),
            
            fluidRow(
              box(title = "Input", status = "primary", "Box content")
            )
            
    ),
    
    tabItem(tabName = "bernouli",
            
            fluidRow(
              box(title = "Input",width = 12, status = "primary",
                  sliderInput(inputId = "sliderBernouli","Choose simulations:",1000,2000,1500),
                  numericInput("bernouliP","p:",0.5,min = 0, max = 1),
                  actionButton(inputId = "bernouliSubmit", label = "Submit")
              )
            ),
            
            fluidRow(
              box(title = "output",width = 12, status = "primary",
                  plotOutput(outputId = "plotBernouli")
              )
            )
            
    ),
    
    tabItem(tabName = "binomial",
            
            fluidRow(
              box(title = "Input",width = 12, status = "primary",
                  sliderInput(inputId = "sliderBinomial","Choose simulations:",1000,2000,1500),
                  numericInput("binomialN","n:",50),
                  numericInput("binomialP","p:",0.5,min = 0, max = 1),
                  actionButton(inputId = "binomialSubmit", label = "Submit")
              )
            ),
            
            fluidRow(
              box(title = "output",width = 12, status = "primary",
                  plotOutput(outputId = "plotBinomial")
              )
            )
            
    ),
    
    
    tabItem(tabName = "geometric",
            
            fluidRow(
              box(title = "Input", status = "primary", "Box content")
            ),
            
            fluidRow(
              box(title = "Input", status = "primary", "Box content")
            )
            
    ),
    
    
    tabItem(tabName = "exponential",
            
            fluidRow(
              box(title = "Input", status = "primary", "Box content")
            ),
            
            fluidRow(
              box(title = "Input", status = "primary", "Box content")
            )
            
    ),
    
    tabItem(tabName = "gamma",
            
            fluidRow(
              box(title = "Input", status = "primary", "Box content")
            ),
            
            fluidRow(
              box(title = "Input", status = "primary", "Box content")
            )
            
    ),
    
    
    tabItem(tabName = "poisson",
            
            fluidRow(
              box(title = "Input", status = "primary", "Boxsdf content")
            ),
            
            fluidRow(
              box(title = "Input", status = "primary", "Box content")
            )
            
    ),
    
    
    tabItem(tabName = "normal",
            
            fluidRow(
              box(title = "Input",width = 12, status = "primary",
                  sliderInput(inputId = "sliderNormal","Choose simulations:",1000,2000,1500),
                  numericInput("normalMean","Mean:",0, min = 0, max = 100),
                  numericInput("normalVariance","Variance:",1,min = 1, max = 100),
                  actionButton(inputId = "normalSubmit", label = "Submit")
              )
            ),
            
            fluidRow(
              box(title = "output",width = 12, status = "primary",
                  plotOutput(outputId = "plotNormal")
              )
            )
            
    ),
    
    tabItem(tabName = "aboutUs",
            fluidRow(
              box(title = "About US", status = "primary",solidHeader = TRUE, "This is US, the last of US", width = 12)
            ),
            
            fluidRow(
              box(title = "Ali Salmani", status = "success",solidHeader = TRUE, "whispering silence"),
              box(title = "Amin Talebi", status = "info",solidHeader = TRUE, "")
            ),
            
            fluidRow(
              box(title = "Ata Jadid Ahari", status = "warning",solidHeader = TRUE, ""),
              box(title = "Shahab Rahimi Rad", status = "danger",solidHeader = TRUE, "")
            )
            
    ),
    
    tabItem(tabName = "home",
            fluidRow(
              box(title = "Welcome", status = "warning", width = 12, tags$p("Welcome to home page, please choose an RV from slidebar"))
            )
            
    )
  )
)

ui <- dashboardPage(
  skin = "purple",
  header = header,
  sidebar = sidebar,
  body = body
)
