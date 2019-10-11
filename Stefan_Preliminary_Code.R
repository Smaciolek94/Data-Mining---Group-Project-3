# Group Assignment 3
#generating some random data
x <- rnorm(100)
y <- rnorm(100)
data <- data.frame(x,y)

#fitting a spline
#x is the predictor
#y is the response
#cv = true means that the smoothing parameter is found via leave-one-out cross validation
#nknots = the number of knots
out <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) # creating an empty vector for the output

for (i in 5:20) { #starting at 5 because I get errors when I start at 1
  q <- as.numeric(i)
output <- smooth.spline(x,y,cv=TRUE, nknots=q)
out[(i-4)] <- output$pen.crit
}

index <- (1:20)
outc <- c(0,0,0,0,out)
RSS <- data.frame(index,outc)

########################testing section:

lm(y~(x+x^2),data)