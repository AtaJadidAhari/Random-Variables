require("bitops")


MT <-setClass(
  "MT",
  
  #slots
  slots = c(
    seed = "numeric",
    next_ = "numeric",
    state = "numeric"
  ),
  
  prototype = list(
    seed = 5489,
    state = vector("numeric", 624),
    next_ = 0
  )
)

setGeneric(name = "init",
           def = function(this){
             standardGeneric("init")
           }
)

setMethod(f="init",
          signature = "MT",
          definition = function(this){
            this@state[1] = this@seed;
            temp = 0
            testVec <- c()
            for (i in 2:624) {
              s = bitXor(this@state[i - 1], bitShiftR(this@state[i - 1], 30));
              temp = bitAnd(s, 0xffff0000)
              temp = bitShiftR(temp, 16)
              temp = temp * 1812433253
              temp = temp %% (2^32)
              temp = bitShiftL(temp, 16)
              temp = temp + bitAnd(s, 0x0000ffff) * 1812433253
              temp = temp %% (2^32)
              temp = temp + i - 1
              this@state[i] = temp
              
            }   
            this <- twist(this)
            return(this)
          }
)


setGeneric(name = "twist",
           def = function(this){
             standardGeneric("twist")
           }
)

setMethod(f="twist",
          signature = "MT",
          definition = function(this){
            temp1 = 0
            temp2 = 0
            for(i in 1:227){
              bits = bitOr(bitAnd(this@state[i] , 0x80000000), bitAnd(this@state[i + 1] , 0x7fffffff));
              temp1 = bitXor(this@state[i + 397], (bits / 2));
              temp2 = bitAnd(bits, 1) * 0x9908b0df
              this@state[i] = bitXor(temp1, temp2)
            } 
            for(i in 228:623){
              bits = bitOr(bitAnd(this@state[i] , 0x80000000), bitAnd(this@state[i + 1] , 0x7fffffff));
              temp1 = bitXor(this@state[i - 227], (bits / 2));
              temp2 = bitAnd(bits, 1) * 0x9908b0df
              this@state[i] = bitXor(temp1, temp2)
            }
            bits = bitOr(bitAnd(this@state[624] , 0x80000000), bitAnd(this@state[1] , 0x7fffffff));
            temp1 = bitXor(this@state[397], (bits / 2));
            temp2 = bitAnd(bits, 1) * 0x9908b0df
            this@state[624] = bitXor(temp1, temp2)
            this@next_ = 1
            return(this)
          }
)

setGeneric(name = "random",
           def = function(this){
             standardGeneric("random")
           }
)


setMethod(f="random",
          signature = "MT",
          definition = function(this){
            x = this@state[this@next_]
            x = bitXor(x, bitShiftR(x, 11))
            x = bitXor(x, bitAnd(0x9d2c5680, bitShiftL(x, 7)))
            x = bitXor(x, bitAnd(0xefc60000, bitShiftL(x, 15)))
            x = bitXor(x, bitShiftR(x, 18))
            return(x)            
          }
)

#' @title Merssene Twister Random number generator
#' @description   a method to generate 32 bit unsinged int random number
#'@param seed: a numeric input as the seed of random generator
#'@param numOfReapeat: a numeric for count of generated random numbers
#'@return  a 32 bit unsinged int random number.
rgenerator <- function(seed, numOfReapeat){
  mt <- MT(seed=seed)
  mt <- init(mt)
  res <- c()
  for(i in 1:numOfReapeat){
    res <- c(res, random(mt))
    mt@next_ = mt@next_ + 1
    if(mt@next_ >= 625){
      mt <- twist(mt)
    }
  }
  return(res)
}

#' @title Uniform random variable generator
#' @description   a method to create Uniform random variables
#'@param n: a numeric as count of uniform random variables
#'@param min: a numeric as minimum of uniform distribution
#'@param max: a numeric as minimum of uniform distribution
#'@return  a random number from uniform distribution where the generated uniform variable will be of parameter (min, max).
dugen <- function(n, min= 0, max= 1) {
  random_nums <- rgenerator(seed = as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31), numOfReapeat = n)
  v <- min +  random_nums * (max - min) / 4294967296.0  	
  return(v)
}

#' @title Uniform random variable generator
#' @description   a method to create Uniform random variables
#'@param n: a numeric as count of uniform random variables
#'@return  a random number from uniform distribution where the generated uniform variable will be of parameter (0, 1)
cugen <- function(n){  
  random_nums <- rgenerator(seed = as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31), numOfReapeat = n)
  v <- random_nums / 4294967296.0 
  return(v)
}
