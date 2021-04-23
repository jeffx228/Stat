#stat 111 pset 1

#plotting X

lambda <- 1
y_rexp <- rexp(10^4, lambda)

hist(rexp(10^4, lambda), xlim = c(0,10))
quantile(y_rexp)


y_unif <- runif(10^4)
quantile(y_unif)
hist(y_unif, xlim = c(0,1))


#plotting the Pois

lambda <- 6
p <- 1/3

N = rpois(10^4, lambda)
X = rpois(10^4, lambda*p)
Y = rpois(10^4, lambda*(1-p))

plot(X, Y, main = "Correlations")

cor.test(X,Y)

n <- 100

lambda <- 1/n

Y <- rgamma(n, 1, 1)

hist(rgamma(10^4, n, lambda))


hist(rnorm(10^4))
plot(pnorm, from = -5, to = 5, main = "normal cdf")

#3 (f)

plot(data$Expt, data$Speed, main = 
       "comparing light speed by experiment", 
     xlab = "batch number")





n <- 100

T_n <- c()

for (i in 1:10^4){
  x <- sum(log(rexp(n, 1)))/n
  T_n <- c(T_n, x)
}

hist(T_n, main = "Histogram for log expo n=100", breaks = 20)



n <- 100

T_n <- c()

for (i in 1:10^4){
  x <- log(sum(rexp(n, 1))/n)
  T_n <- c(T_n, x)
}

hist(T_n, main = "Log of sample mean of expo n=100", breaks = 20)


# Problem 3

print(getwd())
data <- read.csv("/Users/jeffxu/Work/Stat/morley.csv")
print(data)
hist(data$Speed, main = "Histogram of results of speed light")
ecdf(data$Speed)
plot(ecdf(data$Speed))

mean(data$Speed)
median(data$Speed)
sd(data$Speed)


pdf(data$Speed)
sigma <- 79.01055
mu <- seq(1,1000)

func <- 1
print(data$Speed)

for (x in data$Speed){
  print(func)
  func <- func*1/(sigma*sqrt(2*pi))*exp(-0.5*(x - mu)^2/(sigma^2))
}
plot(2)
plot(type = "l", func, xlim = c(600, 1000), main = "Likelihood Function")

plot(log(func), type = "l", xlim = c(600, 1000), main = "Log of Likelihood Function")

optimize(log(func), interval = c(700, 1000), maximum = TRUE)

match(log(func), c(400,900), max(log(func)))

plot(pnorm(1))

for (j in 1:1000){
  if (log(func)[j] == max(log(func))){
    print(j)
  }
}

x <- seq(-1000000,1000000, by = .2)
y <- pnorm(x, mean = 852.4, sd = 79.01)

plot(x,y)

x <- seq(5, 15, length=1000)
y <- dnorm(x, mean=10, sd=3)
plot(x, y, type="l", lwd=1)

hist(rnorm(100, mean = 852.4, sd = 72))






