#' @title Poisson random generator
#' @description   a method to create Possion random variables
#'@param lambda: a float number as the parameter of the underlying exponential distribution
#'@param t: a float as the length of time interval
#'@return  a random number from Poisson distribution where the generated Poisson variable will be of parameter lambda*t.
pogen <- function(lambda, t){
  if(t == 0){
    return(0)
  }
  nums <- 0
  time <- 0
  while(time < t){
    time <- time + expgen(lambda, 1)
    nums <- nums + 1
  }
  
  return(nums - 1)
  
}

#' This function plots a distribution cantainig n Possian random variables with parameter lambda*t using ggplot2
#' @param numbers: number of random numbers produced using pogen with parameter p
#' @param lambda: the parameter of underlying exponential random variable 
#' @param t: the length of time interval
#' @return a plot of produced random numbers
plotPo <- function(numbers, lambda, t){
  library(ggplot2)
  vec <- vector()
  for(i in 1:numbers){
    vec[i] <- pogen(lambda, t)
  }
  ggplot(data.frame(vec),aes(x=vec)) + geom_density()
}