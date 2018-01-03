#' @title Exponential random generator
#' @description   a method to create Exponential random variables
#'@param landa: a float number as the parameter(rate) of desired exponential distribution
#'@param number:  the number of exponentials needed
#'@return  a vector of random numbers from Exponential distribution with parameter landa.
expgen=function(landa,number=1){
  rand=cugen(number)# replace cugen
  answer=c()
  for(i in 1:number){
    answer=c(answer,-log(rand[i])/landa)
  }
  return(answer)
}
#' This function plots a distribution cantainig n exponential random variables with parameters landa  using ggplot2
#' @param landa: the parameter of exponential random variable 
#' @param numbers: number of random numbers produced using expgen with parameter landa
#' @return a plot of produced random numbers
expplot=function(landa,number=1){
  
  library(ggplot2)
  x=expgen(number,landa)
  ggplot(data.frame(x),aes(x))+geom_density()
}