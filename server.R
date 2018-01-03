library(shiny)
require("ggplot2")
pkgs <- c("VGAM")
pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs)) install.packages(pkgs,repos="http://cran.cs.wwu.edu/")
library(VGAM)
library(DT)

load("samplingApp.RData", envir=.GlobalEnv)

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
              temp = temp %% (2^31)
              temp = bitShiftL(temp, 16)
              temp = temp + bitAnd(s, 0x0000ffff) * 1812433253
              temp = temp %% (2^31)
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

random_nums <- rgenerator(seed = as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31), numOfReapeat = 1000)
w <- 0

dugen <- function(n, min= 0, max= 1) {
  w <<- w + n
  v <- min +  sample(random_nums, size = n) * (max - min) / 4294967296.0
  if(w > 1000){
    random_nums <<- rgenerator(seed = as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31), numOfReapeat = 1000)
    w <<- 0
  }
  return(v)
}

cugen <- function(n){
  w <<- w + n  
  v <- sample(random_nums, size = n) / 4294967296.0 
  if(w > 1000){
    random_nums <<- rgenerator(seed = as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31), numOfReapeat = 1000)
    w <<- 0
  }
  return(v)
}


brgen <- function(p){
  x <- cugen(1)
  
  if(x <= p)
    return(0)
  else
    return(1)
}

brgen_n <- function(n, bern.prob = 0.5){
  return(replicate(n, brgen(bern.prob)))
}

bigen <- function(n, p){
  
  
  res = 0
  
  for(i in 1:n){
    res = res + brgen(p)
  }
  
  return(res)
}

bigen_n <- function(n, binom.size = 10, binom.prob = 0.5){
  return(replicate(n, bigen(binom.size, binom.prob)))
}

nogen <- function(u, s){
  
  x <- rpois(1, 100000) 
  x = x - 100000
  x = x * sqrt(s)
  x = x + u
  
  return(x)
}

nogen_n <- function(n, mean= 0, sd= 1){
  return(replicate(n, nogen(mean, sd)))
}

expgen=function(n, exp.rate= 1){
  rand = cugen(n)
  answer=c()
  for(i in 1:n){
    answer=c(answer,-log(rand[i])/exp.rate)
  }
  return(answer)
}

gagen=function(n, gam.shape= 5, gam.rate= 1){
  answer=c()
  for(i in 1:n){
    answer=c(answer,sum(expgen(gam.shape, gam.rate)))
  }
  return(answer)
}

geometric <- function(p = 0.5){
  u <- cugen(1)
  floor(log(u)/log(1 - p))
}

geom_n <- function(n = 500, geom.prob=0.5){
  return(replicate(n,geometric(geom.prob)))
}

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

dugen_n <- function(n= 500, min= 0, max= 1) {
  random_nums <- rgenerator(seed = as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31), numOfReapeat = n)
  v <- min +  random_nums * (max - min) / 4294967296.0  	
  return(v)
}


shinyServer(function(input,output){
  dat <- reactive({
    dist <- switch(input$dist,
                   bern=brgen_n, bin=bigen_n,
                   geom=geom_n, poi=rpois2, # remaining
                   exp=expgen, gam=gagen, # continuous
                   norm=nogen_n, unif=dugen_n
    )
    
    def.args <- switch(input$dist,
                       # discrete
                       bern=c(input$bern.prob),
                       bin=c(input$binom.size,input$binom.prob),
                       geom=c(input$geom.prob),
                       poi=c(input$poi.lambda),
                       # continuous
                       exp=c(input$exp.rate),
                       gam=c(input$gam.shape,input$gam.rate),
                       norm=c(input$mean,input$sd),
                       unif=c(input$min,input$max)
                       
    )
    
    f <- formals(dist)
    f <- f[names(f)!="nn" & names(f)!="n"]
    
    len <- min(length(f),3-1);
    f <- f[1:len]
    
    argList <- list(n=input$n)
    for(i in 1:len)
      argList[[names(f)[i]]] <- def.args[i]
    
    return(list(do.call(dist, argList),
                names(f))
           )
  })
  
  output$dist1 <- renderUI({
    input$dist
    isolate({
      lab <- switch(input$dist,
                    bern="Probability:", bin="Size:", geom="Probability:",poi="Mean and Variance:", # discrete
                    exp="Rate:", gam="Shape:",
                    norm="Mean:", unif="Minimum:"
      )
      ini <- switch(input$dist,
                    bern=0.5, bin=10, geom=0.5, poi=10,	# discrete
                    exp=1, gam=1, norm=0, unif=0 # continuous
      )
      numericInput(dat()[[2]][1],lab,ini)
    })
  })
  
  output$dist2 <- renderUI({
    input$dist
    isolate({
      lab <- switch(input$dist,
                    bin="Probability:", # discrete
                    gam="Rate:", # continuous
                    norm="Standard deviation:",unif="Maximum:"
      )
      ini <- switch(input$dist,
                    bin=0.5,
                    gam=1, norm=1, unif=1
      )
      if(any(input$dist==c("bin","dunif","hgeom","nbin","cauchy","lap","logi","pareto","weib",
                           "beta","F","gam","lognorm","norm","unif"))) numericInput(dat()[[2]][2],lab,ini)
    })
  })
  
  output$dist3 <- renderUI({
    input$dist
    isolate({
      lab <- switch(input$dist,
                    dunif="Step size:",	hgeom="K:")
      ini <- switch(input$dist,
                    dunif=1, hgeom=5)
      if(any(input$dist==c("dunif","hgeom"))) numericInput(dat()[[2]][3],lab,ini)
    })
  })
  
  output$dldat <- downloadHandler(
    filename = function() { paste(input$dist, '.csv', sep='') },
    content = function(file) {
      write.csv(data.frame(x=dat()[[1]]), file)
    }
  )
  
  is_discrete <- function(str){
    if(str == "bern") return(TRUE)
    if(str == "bin") return(TRUE)
    if(str == "geom") return(TRUE)
    if(str == "poi") return(TRUE)
    return(FALSE)
  }
  
  a <- ""
  doPlot <- function(){
    d <- dat()[[1]]
    dist <- input$dist
    a <<- dist
    n <- input$n
    expr <- get(paste("expr", dist, sep="."));
    
    
    if(is_discrete(dist)){
      d <- table(d)
      d <- data.frame(x = factor(names(d), levels = names(d)), y = as.numeric(d) / n)
      ymx <- 1.25*max(d$y)
      ggplot(d, aes(x, y)) + geom_col(colour = "black", fill = "#FF9999") + 
        labs(x = "Observations", y = "Density") +
        annotate("text", x = -Inf, y = ymx, hjust = -1, vjust = 1.5, size = 7, label = as.character(expr), parse = TRUE) +
        scale_y_continuous(expand = c(0, 0))  +
        #theme_gray(base_size = 18)
        theme_light(base_size = 18) 
        
    } else {
      d <- data.frame(x = d)
      ggplot(d, aes(x, y=..density..)) + geom_histogram(colour = "black", fill = "#FF9999", bins = 15) + 
        geom_line(stat = "density", adjust = 2) +
        labs(x = "Observations", y = "Density") +
        annotate("text", x = -Inf, y = Inf, hjust = -1, vjust = 1.5, size = 7, label = as.character(expr), parse = TRUE) +
        theme_light(base_size = 18)  +
        theme(panel.background = element_rect(fill = 'white', colour = 'white'))
    }
  }
  
  output$plot <- renderPlot({
    doPlot()
  },
  height=750, width=1000
  )
  
  output$summary <- renderPrint({
    summary(dat()[[1]])
    
  })
  
  output$dldat <- downloadHandler(
    filename = function() { paste(input$dist, '.csv', sep='') },
    content = function(file) {
      write.csv(data.frame(x=dat()[[1]]), file)
    }
  )
  
  doplot2 <- function(){
    inFile <- input$esUp
    
    if(is.null(inFile))
      return(NULL)
    
    input <- read.table(inFile$datapath, sep = " ")
    dist <- input$dist
    toPlot <- "They will not control us, we will be victorious"
    ans <- "Muse - Uprising, And nothing else"
    
    print(a)
    if(a == "bern"){
      k <- 0
      for(i in 1:length(input)){
        if(input[,i] == 1){
          k <- k +1
        }
      }
      
      ans <- "the parameter of this Bernoulli distribution is: "
      ans <- paste0(ans, (k/length(input)))
      
      toPlot <- unlist(input, use.names = FALSE)
      toPlot <- append(toPlot, brgen((k/length(input))))
    }
    else if(a == "bin"){
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
      
      toPlot <- unlist(input, use.names = FALSE)
      toPlot <- append(toPlot, bigen((k/length(input)), length(input)))
    }
    else if(a == "geom"){
      sum <- 0
      for(i in 1 : length(input)){
        sum <- sum + input[, i]
      }
      p <- (length(input)/sum)
      ans <- "the parameter of this Geometric distribution is: "
      ans <- paste0(ans, p)
      toPlot <- unlist(input, use.names = FALSE)
      toPlot <- append(toPlot, gegen(p))
    }
    else if(a == "poi"){
      sum <- 0
      for(i in 1 : length(input)){
        sum <- sum + input[, i]
      }
      lambda = (sum/length(input))
      ans <- "the parameter of this Poisson distribution is: "
      ans <- paste0(ans, lambda)
      toPlot <- unlist(input, use.names = FALSE)
      toPlot <- append(toPlot, rpois(1,lambda))
    }
    else if(a == "unif"){
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
      
      toPlot <- unlist(input, use.names = FALSE)
      toPlot <- append(toPlot, dugen(1, min, max))
    }
    else if(a == "norm"){
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
      
      toPlot <- unlist(input, use.names = FALSE)
      toPlot <- append(toPlot, nogen(mu, sigma))
    }
    else if(a == "gam"){
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
      notList <- unlist(input, use.names = FALSE)
      
      m <- mean(notList)
      v <- var(notList)
      
      a <- m^2/v
      b <- m/v
      
      ans <- "the parameters of this Gamma distribution is: ("
      ans <- paste0(ans, a)
      ans <- paste0(ans, ",")
      ans <- paste0(ans, b)
      ans <- paste0(ans, ")")
      
      newnum <- gagen(1,a,b)
      
      toPlot <- unlist(input, use.names = FALSE)
      toPlot <- append(toPlot, newnum)
    }
    else if(a == "exp"){
      sum <- 0
      for(i in 1 : length(input)){
        sum <- sum + input[, i]
      }
      lambda <- (sum / length(input))
      ans <- "the parameter of this Exponential distribution is: "
      ans <- paste0(ans, lambda)
      
      toPlot <- unlist(input, use.names = FALSE)
      toPlot <- append(toPlot, expgen(lambda))
    }
    
    
    ggplot(data.frame(toPlot),aes(toPlot)) + geom_density() + theme_light(base_size = 18) + 
      ggtitle(ans)
    
  }
  
  output$esPlot <- renderPlot({
    doplot2()
  },
  height=750, width=1000)
  
})
