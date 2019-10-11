# Group Assignment 3
#generating some random data
x <- rnorm(10000,10,100)
y <- x^3 + x^2 + x + rnorm(10000,0,1000000) #y is a polynomial function of x with some noise
data <- data.frame(x,y)
plot(x,y)

#this fits a cubic polynomial model
nsplines <- 20 # put the number of splines here
width <- (max(data$x) - min(data$x)) / nsplines
min <- min(x)
model <- 0
dim(model) <- nsplines

#fitting a cubic mondel for each slice
for(i in 1:nsplines){
  subdata <- subset(data, (data$x > (min+width*(i-1) && data$x < (min+width*i)))) # subset data
    #run model on subsetted data and assign it here
    model[i] <- lm(subdata$y~poly(subdata$x,3)) #polynomial model of degree 3
}



