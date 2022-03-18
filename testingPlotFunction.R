
source("plotRandomFunction.R")

## Line 2 above, tells R to open and run ALL of the 
##    code in the file, plotRandomFunction.R

plotRandom(n=20)
plotRandom(n=1000)
plotRandom(n=53)

plotRandom(n=60, mu=70, sigma=2.3)
plotRandom(n=600000, mu=70, sigma=2.3)

plotRandom(n=60000, mu=70, sigma=2.3, hist_breaks = 30)

set.seed(20220318)
plotRandom(n=20)

## Playing around with bin sizes to understand how hist()
##    works
set.seed(1)
x <- rnorm(n=200)

hist(x)
hist(x, breaks = 1)
hist(x, breaks = 0)  ## cant do this - get an error
hist(x, breaks = 3)
hist(x, breaks = 12)
hist(x, breaks = 50)
