
#Question 1a) 
lognpars <- function(mn=1, sd=1) {
  ## Moment matching for the lognormal distn
  ## Formulas derived from H&H eqns (3.4.27-28, p58-59)
  ## and checked on Wikipedia
  c <- sd/mn
  alpha <- log(mn/sqrt(1 + c^2)) # mean of log(x)
  beta  <- sqrt(log(1 + c^2))    #  sd  of log(x)
  list(alpha=alpha, beta=beta)   # returned value
}

Lpars <- lognpars(10, 10)

## Get N samples
set.seed(123)
N <- 1000
x <- rlnorm(n=N, meanlog=Lpars$alpha, sdlog=Lpars$beta)

h <- hist(x, breaks= 100, plot=FALSE);
b <- cut(1, h$breaks);
clr <- ifelse(h$breaks < 20, "white", "lightblue")[-length(h$breaks)]
hist(x, col=clr, breaks=100, freq=FALSE, xlab = 'N') 
lines(dlnorm(0:1000, meanlog = Lpars$alpha, sdlog = Lpars$beta, log = FALSE), col="red")

#THIS WORKS!!! Calculates percent error. 
true <- 1 - plnorm(20, mean = Lpars$alpha, sd = Lpars$beta, log = FALSE)
xtest <- sum(x>20)/length(x)

abs(((xtest-xtrue)/xtrue)*100) #Answer to Question 1b compare integral to sum f samples. 

################################################################################
#How many licks to the center of the lolly pop
sequ = seq(20, 10000, by = 1)

d <- sum(dlnorm(20:5000, mean = Lpars$alpha, sd = Lpars$beta, log = FALSE)) #Dependent on interval BLAH
p <- 1 - plnorm(20, mean = Lpars$alpha, sd = Lpars$beta, log = FALSE)
q <- qlnorm(p, mean = Lpars$alpha, sd = Lpars$beta, log = FALSE, lower.tail=FALSE)

error <- rep(NA, length(sequ))
error2 <- rep(NA, length(sequ))
xtrue = p
j <- 0

for (i in sequ) {
  j <- j + 1
  N <- i
  x <- rlnorm(n=N, meanlog=Lpars$alpha, sdlog=Lpars$beta)
  
  xtest <- sum(x>20)/length(x)
  error[j] <- ((xtest-xtrue)/xtrue)*100
}

sensor1 <- data.frame(y=error, t=as.numeric(seq(1, length(error))))
sensor2 <- data.frame(y=abs(error), t=as.numeric(seq(1, length(error2))))

ggplot() + 
  geom_point(aes(sensor1$t, sensor1$y))+
  theme_classic()+
  geom_hline(yintercept = 1, size=0.5, color="lightgrey")+
  geom_hline(yintercept = -1, size=0.5, color="lightgrey")+
  labs(x="Length of N", y="Percent Error (%)", title="How many licks")

#fit <- nls(y ~ SSasymp(t, yf, y0, log_alpha), data = sensor2) #Doesnt work for super large data sets could cut it down
#ym <- augment(fit)
min(ym$.fitted) #last known intercept

#qplot(t, y, data = augment(fit), alpha=0.2) + geom_line(aes(y = .fitted), col='red') + theme_classic()

################################################################################

quant <- matrix(c(0.25, 0.75,0.025,0.98), nrow = 2)
errors <- matrix(c(NA, NA, NA, NA), nrow = 2)

for (i in 1:2) 
  {
  q25 <- qlnorm(quant[1, i], mean = Lpars$alpha, sd = Lpars$beta, log = FALSE, lower.tail=FALSE) #gives the number knowing the area
  q75 <- qlnorm(quant[2 ,i], mean = Lpars$alpha, sd = Lpars$beta, log = FALSE, lower.tail=FALSE) #gives the number knowing the area
  
  xtrue25 <- 1 - plnorm(q25, mean = Lpars$alpha, sd = Lpars$beta, log = FALSE, lower.tail=FALSE) #gives the area under the curve, check tails
  xtrue75 <- 1 - plnorm(q75, mean = Lpars$alpha, sd = Lpars$beta, log = FALSE) #gives the area under the curve
  
  xtest25 <- sum(x<q25)/length(x)
  xtest75 <- sum(x>q75)/length(x)
  
  errors[i,1] <- abs((xtest25-xtrue25)/xtrue25)*100
  errors[i,2] <- abs((xtest75-xtrue75)/xtrue75)*100
  }

#ANSWER: I get that the errors are different. But very similar

################################################################################
#EXCERSISE 3
x <- rlnorm(n=N, mean=10, sd=2)

rate <- 2
shape <- 10

xlog <- dlnorm(1:1000, mean = 10, sd = 2, log = FALSE)
xgamma <- dgamma(1:1000, shape=10, rate = 2, log = FALSE) #Probs wrong

hist(xlog, breaks=100, xlim=c(0, 1e-04))
hist(xgamma, breaks=1000, xlim=c(0, 0.01))

#mean, should equal 10
#E(X) = a*s 
#variance 
#Var(X) = a*s^2.







################################################################################

#EXCERSISE 2
#x is the same as the x above. N = 1000
f = 10*(x^2) - x^3
hist(f, col=clr, breaks=300, freq=FALSE, xlab = 'X', xlim=c(-1e+05, 1e+05))

h <- hist(f, breaks= 100, plot=FALSE);
b <- cut(1, h$breaks);
clr <- ifelse(h$breaks < 20, "white", "lightblue")[-length(h$breaks)]
hist(f, col=clr, breaks=100, freq=FALSE, xlab = 'N', xlim=c(-1e+05, 1e+05)) 
#lines(dlnorm(0:1000, mean = mean(f), sd = sd(f), log=FALSE), col="red", xlim=c(-1e+05, 1e+05))

quant <- 1 - plnorm(100, mean = mean(f), sd = sd(f), lower.tail=FALSE) #gives the area under the curve
area <- qlnorm(quant, mean = mean(f), sd = sd(f)) #gives the number knowing the area, checking answer

#ANSWER: The chance F is greater than 100 is 55% given by the quant variable. The expected value of f is the most likely value of
#f which is the mode or more complicatedly
densityf <- dlnorm(1:1000, mean = mean(f), sd = sd(f)) #gives the area under the curve

f[densityf == max(densityf)] #Most likely value of f
median(f)
#Answer: 109.44 is the most likely value of f. I could be messing my initial dlnorm up though. 
#Compared to the median this is not too far off. Mean is no good because of skewness

#densityf <- max(densityf)
#areaf <- plnorm(densityf, mean = mean(f), sd = sd(f))
#fvalue <- qlnorm(areaf, mean = mean(f), sd = sd(f)) #this just returns the density not the value
#I want the inverst of dlnorm!


################################################################################
##Land of lost code
#This would be a fine time for a while loop

x2 <- x
x2[x<20] <- NA

hist(x, breaks = 100, col = 'white', freq=FALSE)
lines(density(x),col="red")
hist(x2, col = 'lightblue', freq=FALSE, add=TRUE, breaks = 100)

#a) Use the samples to find the probability that the next sample x will be greater than 20
#because each sample is independent 
sum(x>20)/length(x)

plnorm(q, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE)

h <- hist(x, breaks= 100, plot=FALSE);
b <- cut(1, h$breaks);
clr <- ifelse(h$breaks < 20, "white", "lightblue")[-length(h$breaks)]
hist(x, col=clr, breaks=100, freq=FALSE)
lines(density(x),col="red")

x2 <- h$density
sum(x2[x>20], na.rm=TRUE) #This might not be right becaue its binned into 100 
x2 <- h$counts
sum(x2[x>20], na.rm=TRUE)/length(x) #This might not be right but same
#plot(h, col=clr);

plnorm(q, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE)

#Another way to do it
xtest <- h$counts
sum(xtest[x>20], na.rm=TRUE)/length(x) #This might not be right but same
sum(x>20)/length(x) #upset this does not match

#plnorm(0.94, meanlog = Lpars$alpha, sdlog = Lpars$beta, lower.tail = TRUE, log.p = FALSE)


Lpars <- lognpars(10, 10)
set.seed(123)
error <- rep(NA, length(10:1000))
j <- 0 
for (i in 10:1000) {
  j <- j + 1
  N <- i
  x <- rlnorm(n=N, meanlog=Lpars$alpha, sdlog=Lpars$beta)
  
  xtest <- sum(x>20)/length(x)
  xtrue <- dlnorm(0:i, meanlog = Lpars$alpha, sdlog = Lpars$beta, log = FALSE)
  xtrue <- sum(xtrue[x>20]) #This is what it should be, now do this until it reached less than 1%
  
  error[j] <- (abs(xtrue-xtest)/xtrue)
}

logit <- function(p) log(p/(1 - p))
expit <- function(x) 1/(1 + exp(-x))  

#Comparing integrated values to one another
xtrue <- dlnorm(0:1000, meanlog = Lpars$alpha, sdlog = Lpars$beta, log = FALSE)
sum(xtrue[x>20]) #This is what it should be, now do this until it reached less than 1%
xtest <- sum(x>20)/length(x)
xtest

plot(pnorm(dlnorm(0:1000, meanlog = Lpars$alpha, sdlog = Lpars$beta, log = TRUE)))
cdf <- pnorm(dlnorm(0:1000, meanlog = Lpars$alpha, sdlog = Lpars$beta, log = FALSE))
sum(cdf[x>20])

# What is the Z-score of the 96th quantile of the normal distribution? Chance it lies outside of the mean. Good for part c
qnorm(.25, upper.tail = TRUE) #set mean and standard dev. to be log normal distribution. 
qnorm(.75, mean= Lpars$alpha, sd=Lpars$beta,) #set mean and standard dev. to be log normal distribution. Not quit
qnorm(0.95, mean= Lpars$alpha, sd=Lpars$beta, log = FALSE)
#integrate(dnorm, mean= Lpars$alpha, sd=Lpars$beta, log = FALSE, 20, Inf)$value
#integrate(dnorm(0:10), mean = Lpars$alpha, sd = Lpars$beta, log = FALSE, -Inf, Inf); #I could intigrate it the old fationed way
#integrate(dnorm, mean= Lpars$alpha, sd=Lpars$beta, log = FALSE, 20, Inf)$value
#integrate(dnorm, mean= Lpars$alpha, sd=Lpars$beta, log = FALSE, lower= 20, upper= Inf, abs.tol = 0)$value
#integrate(Vectorize(dnorm), mean= Lpars$alpha, sd=Lpars$beta, vec = 0:10, log = FALSE, -1.96, 1.96)
#d <- dlnorm(seq(20,100, by=0.1), mean = Lpars$alpha, sd = Lpars$beta, log = FALSE)
#d <- dlnorm(20:1000, mean = Lpars$alpha, sd = Lpars$beta, log = FALSE) #Dependent on interval BLAH
#qnorm(d, lower.tail = FALSE)
#pnorm(d, lower.tail=FALSE)
#plnorm(0.94, meanlog = Lpars$alpha, sdlog = Lpars$beta, lower.tail = TRUE, log.p = FALSE)

#xtest <- h$density
#sum(xtest[x>20], na.rm=TRUE) #This might not be right becaue its binned into 100

theta.0 <- min(data.df$y) * 0.5  

# Estimate the rest parameters using a linear model
model.0 <- lm(log(y - theta.0) ~ x, data=data.df)  
alpha.0 <- exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

# Starting parameters
start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
start

#plot(error)
#I need to find a way to measure the error of the rnnorm to the plnorm then loop it upwards t see how many samples it takes
#nls(y ~ yf + (y0 - yf) * exp(-alpha * t),  #Believe I'm running this to set starting parameters. Gives an error because they are bad
#    data=sensor1,
#    start = list(y0 = 30, yf = 4 , alpha = 1)) 

alpha = -5.560
yf = 6.093
y0 = 28.858
t = sensor1$t
(y0-yf) * exp(alpha * t)

#hist(x, breaks=100)
#lines(xlog, col="red")
#hist(x, breaks=100)
#lines(xgamma, col="red")
#hist(x, breaks=100, xlim = c(0, 1e06))
#hist(xgamma, breaks=1000, xlim=c(0, 0.01))

#h <- hist(x, breaks=100, plot=FALSE) 
#xfit<-seq(min(x),max(x),length=1000)
#yfit<-dlnorm(xfit, mean=10, sd=2, log = FALSE)
#gfit<-dgamma(xfit,shape=10, rate = 2)
#yfit <- yfit*diff(h$mids[1:2])*length(x)

#hist(x, breaks=100, col="white", xlab="Random Numbers", main="Histogram with Log-Normal Curve", xlim = c(0, 2e06))
#lines(xfit, yfit, col="red", lwd=1)
#lines(xfit, gfit, col="blue", lwd=1)
#plot1 <- ggplot() + 
#  geom_line(aes(x, xgamma), col = 'grey69') + 
#  geom_line(aes(x, xlog), col='black') + 
#  theme_classic()

#plot2 <- ggplot() + 
#  geom_line(aes(x, xgamma2), col='black') + 
#  geom_line(aes(x, xlog2), col='grey69') + 
#  theme_classic()
#grid.arrange(plot1, plot2, ncol=2)

#######################################################################################








