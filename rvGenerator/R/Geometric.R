#'a method to create geometric random variables 
#' @param p: the parameter of desired geometric random variable 
#'@return a random number following Geometric distribution with parameter p which is equal to floor(ln(u)/ln(1-p) )
#' where u is a unifor number between 0 and 1
geometric <- function(p){
  u <- cugen(1)
  floor(log(u)/log(1 - p))
}


#'creates a Geometric random variable
#'@param p: the parameter of geometric random variable which is the probability of being successful
#'@return a random number following Geometric distribution with parameter p
gegen <- function(p){
  sum <- 0
  while(TRUE){
    u <- brgen(p)
    if(u == 0){
      sum <- sum + 1
    }
    else{
      return(sum)
    }
  }
}

#' This function plots a distribution cantainig n geometric random variables with parameter p using ggplot2
#' @param p: the parameter of desired geometric random variable 
#' @param numbers: number of random numbers produced using gegen with parameter p
#' @return a plot of produced random numbers
plotGeo <- function(numbers, p){
  library(ggplot2)
  vec <- vector()
  for(i in 1:numbers){
    vec[i] <- geometric(p)
  }
  ggplot(data.frame(vec),aes(x=vec)) + geom_density()
}

