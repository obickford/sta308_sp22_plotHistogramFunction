######################################################
##
## plotRandomFunction.R
##
## Olivia Bickford
## March 18, 2022
##    Ammended on March 28, 2022
##        To do some error checking
##
##  A function that generates
##    a sequence of random normal variables
##  and makes a histogram
##
##  The function expects the follwing inputs
##    n- the sample size (default with n=1)
##    mu- mean of normal distribution (default, mu=0)
##    sigma- sd of normal distribution (default, sigma=1)
##    hist_breaks- number of break points in histogram (default 6)
##
##  This function is based on the code from
##  Friday, March 11, 2022
##
# plotRandom <- function(n=1) {
#   hist(rnorm(n=n,mean=0,sd=1) )
# }

##Today we will add to it

##is.wholenumber() is from the help documentation
## on is.integer -- help(is.integer)
##it simply checks if a number is an integer or not
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {
  abs(x - round(x)) < tol
}

plotRandom <- function(n=1, mu=0, sigma=1, hist_breaks=6) {
  if(n<0) {
    stop("Sample size n must be positive")
  } 
  if( !is.wholenumber(n) ) {
    warning(paste("Sample size n must be a whole number\n   R is using n=floor(n)=",
                  floor(n) ) )
  }
  hist(rnorm(n=n,mean=mu,sd=sigma),
       breaks=hist_breaks)
}



