#' @title uniform estimator
#' @description a method to estimate bounds of the interval that a uniform number is chosen from, and then produces 
#' another number following this distribution and then plots them.
#' @param fileName: the name of a .txt file containig the data that we are going to estimate with
#' @return prints a random number with estimated parameters and plots them using ggplot2
uniEst <- function(fileName) {
  
  input <- read.table(fileName , sep = " ")
  max = input[,1]
  min = input[,1]

  for(i in 1:length(input)){    
    if(input[, i] > max){
      max = input[,i]
    }
    if(input[, i] < min){
      min = input[,i]
    }  
  }
  ans <- "the interval of this uniform distribution is: ("
  ans <- paste0(ans, min)
  ans <- paste0(ans, ",")
  ans <- paste0(ans, max)
  ans <- paste0(ans, ")")
  print(ans)
  toPlot <- unlist(input, use.names = FALSE)
  toPlot <- append(toPlot, dugen(min, max))
  library(ggplot2)
  ggplot(data.frame(toPlot),aes(toPlot)) + geom_density()
}
#uniformEstimator("test.txt")

#' @title Bernoulli estimator
#' @description a method to estimate parameter of the Bernoulli distribution , and then produces 
#' another number following this distribution and then plots them.
#' @param fileName: the name of a .txt file containig the data that we are going to estimate with
#' @return prints a random number with estimated parameters and plots the distribution using ggplot2
beEst <- function(fileName) {
  input <- read.table(fileName , sep = " ")
  k <- 0
  for(i in 1:length(input)){
    if(input[,i] == 1){
      k <- k +1
    }
  }
    
  ans <- "the parameter of this Bernoulli distribution is: "
  ans <- paste0(ans, (k/length(input)))
  print(ans)
  toPlot <- unlist(input, use.names = FALSE)
  toPlot <- append(toPlot, brgen((k/length(input))))
  library(ggplot2)
  ggplot(data.frame(toPlot),aes(toPlot)) + geom_density()              
}

#' @title Binomial estimator
#' @description a method to estimate parameter of the Binomial distribution , and then produces 
#' another number following this distribution and then plots them.
#' @param fileName: the name of a .txt file containig the data that we are going to estimate with
#' @return prints the parameter of this distribution and then
#'  prints a random number with estimated parameters and plots the distribution using ggplot2
biEst <- function(fileName){
  input <- read.table(fileName , sep = " ")
  k <- 0
  for(i in 1:length(input)){
    if(input[,i] == 1){
      k <- k +1
    }
  }
  ans <- "the parameters of this Binomial distribution is: ("
  ans <- paste0(ans, length(input))
  ans <- paste0(ans, ",")
  ans <- paste0(ans, (k/length(input)))
  ans <- paste0(ans, ")")
  print(ans)
  toPlot <- unlist(input, use.names = FALSE)

  toPlot <- append(toPlot, bigen((k/length(input)), length(input)))
  library(ggplot2)
  ggplot(data.frame(toPlot),aes(toPlot)) + geom_density()  
}
#binomialEstimator("test.txt")




#' @title Geometric estimator
#' @description a method to estimate parameter of the Geometric distribution , and then produces 
#' another number following this distribution and then plots them.
#' @param fileName: the name of a .txt file containig the data that we are going to estimate with
#' @return prints the parameter of this distribution and then
#'  prints a random number with estimated parameters and plots the distribution using ggplot2
geoEst <- function(fileName){
  input <- read.table(fileName , sep = " ")
  sum <- 0
  for(i in 1 : length(input)){
    sum <- sum + input[, i]
  }
  p <- (length(input)/sum)
  ans <- "the parameter of this Geometric distribution is: "
  ans <- paste0(ans, p)
  print(ans)
  
  toPlot <- unlist(input, use.names = FALSE)
  toPlot <- append(toPlot, geogen(p))
  library(ggplot2)
  ggplot(data.frame(toPlot),aes(toPlot)) + geom_density()
  
}




#' @title Exponential estimator
#' @description a method to estimate parameter of the Exponential distribution , and then produces 
#' another number following this distribution and then plots them.
#' @param fileName: the name of a .txt file containig the data that we are going to estimate with
#' @return prints the parameter of this distribution and then
#'  prints a random number with estimated parameters and plots the distribution using ggplot2
expEst <- function(fileName){
  input <- read.table(fileName , sep = " ")
  sum <- 0
  for(i in 1 : length(input)){
    sum <- sum + input[, i]
  }
  lambda <- (sum / length(input))
  ans <- "the parameter of this Exponential distribution is: "
  ans <- paste0(ans, lambda)
  print(ans)
  
  toPlot <- unlist(input, use.names = FALSE)
  toPlot <- append(toPlot, expgen(lambda))
  library(ggplot2)
  ggplot(data.frame(toPlot),aes(toPlot)) + geom_density()
  
  
}



#' @title Gamma estimator
#' @description a method to estimate parameter of the Gamma distribution , and then produces 
#' another number following this distribution and then plots them.
#' @param fileName: the name of a .txt file containig the data that we are going to estimate with
#' @return prints the parameter of this distribution and then
#'  prints a random number with estimated parameters and plots the distribution using ggplot2
gammaEst <- function(fileName){
  input <- read.table(fileName , sep = " ")
  sum <- 0
  for(i in 1 : length(input)){
    sum <- sum + input[, i]
  }
  ln <- 0
  for(i in 1: length(input)){
    ln <- ln + log(input[, i])
  }
  ln <- -ln + log(sum/(length(input)))
  a <- (0.5 / ln)
  
  b <- (sum / (length(input)*a))
  
  ans <- "the parameters of this Gamma distribution is: ("
  ans <- paste0(ans, a)
  ans <- paste0(ans, ",")
  ans <- paste0(ans, b)
  ans <- paste0(ans, ")")
  print(ans)
  
   
}


#' @title Poisson estimator
#' @description a method to estimate parameter of the Poisson distribution , and then produces 
#' another number following this distribution and then plots them.
#' @param fileName: the name of a .txt file containig the data that we are going to estimate with
#' @return prints the parameter of this distribution and then
#'  prints a random number with estimated parameters and plots the distribution using ggplot2
poEst <- function(fileName){
  input <- read.table(fileName , sep = " ")
  sum <- 0
  for(i in 1 : length(input)){
    sum <- sum + input[, i]
  }
  lambda = (sum/length(input))
  ans <- "the parameter of this Poisson distribution is: "
  ans <- paste0(ans, lambda)
  print(ans)
  
  toPlot <- unlist(input, use.names = FALSE)
  toPlot <- append(toPlot, rpois(1,lambda))
  library(ggplot2)
  ggplot(data.frame(toPlot),aes(toPlot)) + geom_density()
  
}




#' @title Normal estimator
#' @description a method to estimate parameters of the Normal distribution , and then produces 
#' another number following this distribution and then plots them.
#' @param fileName: the name of a .txt file containig the data that we are going to estimate with
#' @return prints the parameter of this distribution and then
#'  prints a random number with estimated parameters and plots the distribution using ggplot2
noEst <- function(fileName){
  input <- read.table(fileName , sep = " ")
  sum <- 0
  for(i in 1 : length(input)){
    sum <- sum + input[, i]
  }
  mu <- (sum/length(input))
  sigma <- 0
  for(i in 1 : length(input)){
    mu <- mu + (mu - input[, i])**2
  }
  sigma <- (sigma/length(input))
  
  ans <- "the parameters of this Normal distribution is: ("
  ans <- paste0(ans, mu)
  ans <- paste0(ans, ",")
  ans <- paste0(ans, sigma)
  ans <- paste0(ans, ")")
  print(ans)
  
  toPlot <- unlist(input, use.names = FALSE)
  toPlot <- append(toPlot, nogen(mu, sigma))
  library(ggplot2)
  ggplot(data.frame(toPlot),aes(toPlot)) + geom_density() 
}
