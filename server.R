library(shiny)
library(ggplot2)

cugen <- function(n){
  return(runif(n,0,1))
}

brgen <- function(p){
  x <- cugen(1)
  
  if(x <= p)
    return(0)
  else
    return(1)
}

brgenPlot <- function(simulations, p){
  x <- replicate(simulations, brgen(p))
  
  df <- as.data.frame(x)
  
  
  myPlot <- ggplot(data=df, aes(x)) + 
    geom_histogram(binwidth = 1, 
                   col="red", 
                   aes(fill=..count..)) + 
    theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
    scale_fill_distiller(palette = "Spectral")
  
  return(myPlot)
}


bigen <- function(n, p){
  
  
  res = 0
  
  for(i in 1:n){
    res = res + brgen(p)
  }
  
  return(res)
}


bigenPlot <- function(simulations, n, p){
  x <- replicate(simulations, bigen(n,p))
  df <- as.data.frame(x)


  myPlot <- ggplot(data=df, aes(x)) + 
    geom_histogram(binwidth = 1, 
                   col="red", 
                   aes(fill=..count..)) + 
    theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
    scale_fill_distiller(palette = "Spectral")
  
  return(print(myPlot))
}


nogen <- function(u, s){
  
  x <- rpois(1, 100000) #it has to be changed to pogen(1, 1000000)
  x = x - 100000
  x = x * sqrt(s)
  x = x + u
  
  return(x)
}


nogenPlot <- function(simulations = 10, u = 0, s = 1){
  
  
  set.seed(123)
  x <- replicate(simulations, nogen(u, s))
  
  y <- density(x, n = 2^12)
  myPlot <- ggplot(data.frame(x = y$x, y = y$y), aes(x, y)) + geom_line() + 
    geom_segment(aes(xend = x, yend = 0, colour = x)) + theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
    scale_color_gradient(low = '#00F260', high = '#0575E6')
   
  return(print(myPlot))
  
  
  
  # or : 
  # q <- density(x) 
  # plot(q)
  
}







server <- function(input, output) {
  
  
  
  bernouliData <- eventReactive(input$bernouliSubmit,{
    
    validate(
      need(is.numeric(input$bernouliP),"Please enter a valid number"),
      need(input$bernouliP >= 0 && input$bernouliP <= 1,"P must be between 0 and 1")
    )
    
    
    brgenPlot(input$sliderBernouli,as.numeric(input$bernouliP))
  })
  
  output$plotBernouli <- renderPlot({
    bernouliData()
  })
  
  binomialData <- eventReactive(input$binomialSubmit,{
    
    validate(
      need(is.numeric(input$binomialN),"Please enter a valid number"),
      need(is.numeric(input$binomialP),"Please enter a valid number"),
      need(input$binomialP >= 0 && input$binomialP <= 1,"P must be between 0 and 1")
    )
    
    
    bigenPlot(input$sliderBinomial,as.numeric(input$binomialN),as.numeric(input$binomialP))
  })
  
  output$plotBinomial <- renderPlot({
    binomialData()
  })
  
  normalData <- eventReactive(input$normalSubmit,{
    
    validate(
      need(is.numeric(input$normalVariance),"Please enter a valid number"),
      need(is.numeric(input$normalMean),"Please enter a valid number"),
      need(input$normalVariance > 0,"Variance must be positive")
    )
    
    
    nogenPlot(input$sliderNormal,as.numeric(input$normalMean),as.numeric(input$normalVariance))
  })
  
  
  
  output$plotNormal <- renderPlot({
    normalData()
  })
}
