
#' @title Bernouli random generator
#' @description   a method to create Bernouli random variables
#'@param p: a float number as the parameter of the Bernouli distribution
#'@return  a random number from Bernouli distribution (0 or 1) where the generated Bernouli variable will be of parameter p.

brgen <- function(p){
  x <- cugen(1)
  
  if(x <= p)
    return(0)
  else
    return(1)
}

#' This function plots a distribution containig n Bernouli random variables with parameter p using ggplot2
#' @param simulations: number of random numbers produced using brgen with parameter p
#' @param p: the parameter of Bernouli random variable (between 0 and 1)
#' @return a plot of produced random numbers

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
