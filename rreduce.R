#Mustafa Anjrini on 28.07.2025

rm(list = ls())

rreduce<-function(x,y){ # x here is the vector of numbers and y is the direction of the calculation whether "right" or "left"
  
  func<- function(a,b){
    return(1/a+b^2) # please change the variable "a" and "b" of the function of interest accordingly
  }
  
  #implementation
  if(y=="left"){
    z=x[1]
    for (i in 2:length(x)) {
      z=func(z,x[i])
    }
  }
  
  if(y=="right"){
    z=x[length(x)]
    for (i in (length(x)-1):1) {
      z=func(x[i],z)
    }
  }
  
  return(z)
}

#let's do an example using the right calculation

#right
#1st using our function rreduce
rreduce(c(2, 1, 2, 1, -1),"right")

#2nd using the implemented function Reduce in R 
cfrac <- function(x){ Reduce(function(u,v) 1/u + v^2, x,right = TRUE)}
cfrac(c(2, 1, 2, 1,-1))

# let's do an example using the right calculation
rreduce(c(2, 1, 2, 1, -1),"left")

cfrac <- function(x){ Reduce(function(u,v) 1/u + v^2, x,right = FALSE)}
cfrac(c(2, 1, 2, 1,-1))
