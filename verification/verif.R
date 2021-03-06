library(verification)
# # binary/binary example
# obs<- round(runif(100))
# pred<- round(runif(100))
# 
# # binary/binary example
# # Finley tornado data.
# 
# obs<- c(28, 72, 23, 2680)
# A<- verify(obs, pred = NULL, frcst.type = "binary", obs.type = "binary")
# 
# summary(A)
# 
# 
# # categorical/categorical example
# # creates a simulated 5 category forecast and observation.
# obs <- round(runif(100, 1,5) )
# pred <- round(runif(100, 1,5) )
# 
# A<- verify(obs, pred, frcst.type = "cat", obs.type = "cat" )
# summary(A)
# 
# #  probabilistic/ binary example
# 
# pred<- runif(100)
# A<- verify(obs, pred, frcst.type = "prob", obs.type = "binary")
# summary(A)
# 
# # continuous/ continuous example
# obs<- rnorm(100)
# pred<- rnorm(100)
# baseline <- rnorm(100, sd = 0.5) 
# 
# A<- verify(obs, pred, baseline = baseline,  frcst.type = "cont", obs.type = "cont")
# summary(A)

## Data from Wilks, table 7.3 page 246.
y.i   <- c(0,0.05, seq(0.1, 1, 0.1))
obar.i <- c(0.006, 0.019, 0.059, 0.15, 0.277, 0.377, 0.511, 
            0.587, 0.723, 0.779, 0.934, 0.933)
prob.y<- c(0.4112, 0.0671, 0.1833, 0.0986, 0.0616, 0.0366,
           0.0303,  0.0275, 0.245, 0.022, 0.017, 0.203) 
obar<- 0.162


attribute(y.i, obar.i, prob.y, obar, main = "Sample Attribute Plot")  

## Function will work with a ``prob.bin'' class objects as well.
## Note this is a random forecast.
obs<- round(runif(100))
pred<- runif(100)

A<- verify(obs, pred, frcst.type = "prob", obs.type = "binary")
attribute(A, main = "Alternative plot", xlab = "Alternate x label" )
## to add a line from another model
obs<- round(runif(100))
pred<- runif(100)

B<- verify(obs, pred, frcst.type = "prob", obs.type = "binary")
lines.attrib(B, col = "green")


## Same with confidence intervals
attribute(A, main = "Alternative plot", xlab = "Alternate x label", CI =
            TRUE)

#### add lines to plot
data(pop)
d <- pop.convert()
## internal function used to
## make binary observations for
## the pop figure.

### note the use of bins = FALSE
mod24 <- verify(d$obs_rain, d$p24_rain,
                bins = FALSE)

mod48 <- verify(d$obs_rain, d$p48_rain,
                bins = FALSE)
plot(mod24, freq = FALSE)

lines.attrib(mod48, col = "green",
             lwd = 2, type = "b")

