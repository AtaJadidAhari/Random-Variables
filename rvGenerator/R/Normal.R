#' @title Normal random generator
#' @description   a method to create Normal random variables
#'@param u: a float number as the parameter of Normal random variable, determining mean
#'@param s: a positive float number as the parameter of Normal random variable, determining variance
#'@return  a random number from Normal distribution where the generated Normal variable will be of (u,s)

nogen <- function(u, s){
  
  x <- pogen(1,100000)
  x = x - 100000
  x = x * sqrt(s)
  x = x + u
  
  return(x)
}


#' This function plots a Normal random variables with parameters u,s using ggplot2
#' @param simulations: number of random numbers produced using nogen with parameters u,s
#' @param u: the parameter of Normal random variable, as the mean of Normal RV
#' @param s: the parameter of Normal random variable, as the variance of Normal RV
#' @return a plot of produced random numbers


nogenPlot <- function(simulations = 10, u = 0, s = 1){
  
  
  set.seed(123)
  x <- replicate(simulations, nogen(u, s))
  
  y <- density(x, n = 2^12)
  myPlot <- ggplot(data.frame(x = y$x, y = y$y), aes(x, y)) + geom_line() + 
    geom_segment(aes(xend = x, yend = 0, colour = x)) + theme(panel.background = element_rect(fill = 'white', colour = 'white')) +
    scale_color_gradient(low = '#00F260', high = '#0575E6')
   
  return(print(myPlot))

}
