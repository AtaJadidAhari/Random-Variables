#' @title Gamma random generator
#' @description   a method to create Gamma random variables
#' @param number: the number of random Gamma numbers we need
#'@param landa: a float number as the parameter of the underlying exponential distribution
#'@param k: the number of exponentials needed
#'@return  a vector of  random numbers following Gamma distribution with parameters k and landa.
gagen=function(number,k,landa){
  answer=c()
  for(i in 1:number){
    answer=c(answer,sum(expgen(k,landa)))
  }
  return(answer)
}

#' This function plots a distribution cantainig n gamma random variables with parameters k and landa  using ggplot2
#' @param numbers: number of random numbers produced using gagen with parameters k and landa
#' @param landa: the parameter of underlying exponential random variable 
#' @param k: the number of exponencials needed
#' @return a plot of produced random numbers
gaplot=function(number,k,landa){
  library(ggplot2)
  x=gagen(number,k,landa)
  ggplot(data.frame(x),aes(x))+geom_density()
}