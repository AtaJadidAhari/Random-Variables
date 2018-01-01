#' @title Binomial random generator
#' @description   a method to create Binomial random variables
#'@param n: a positive integer as the parameter of Binomial random variable, used for producing a random number between 0 and n
#'@param p: a float number as the parameter of Binomial random variable, used for generating Bernouli random variables with parameter p
#'@return  a random number from Binomial distribution where the generated Binomial variable will be of (n,p)


bigen <- function(n, p){
  
  
  res = 0
  
  for(i in 1:n){
    res = res + brgen(p)
  }
  
  return(res)
}



#' This function plots a distribution cantainig n Binomial random variables with parameters n,p using ggplot2
#' @param simulations: number of random numbers produced using bigen with parameters n,p
#' @param n: the parameter of Binomial random variable, used for producing random number between 0 and n
#' @param p: the parameter of Binomial random variable, used for generating Bernouli random variables with parameter p
#' @return a plot of produced random numbers

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

