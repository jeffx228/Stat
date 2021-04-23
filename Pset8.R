
set.seed(111)

data <- read.csv("/Users/jeffxu/Work/Stat/ar1.csv")
y_vector <- c()
y_vector <- c(y_vector, data$x)
theta_vector <- c()
sum_y <- 0
sum_yprod <- 0
sigma_squared <- 1
for (i in 2:100){
  sum_y <- sum_y + y_vector[i-1]^2
  sum_yprod <- sum_yprod + y_vector[i]*y_vector[i-1]
}
m_0 <- 0.1
tau_0 <- 0.5
tau_1 <- sqrt(1/(1/(sigma_squared) * sum_y + 1/(tau_0^2)))
m_1 <-tau_1^2*(1/(sigma_squared) * sum_yprod + m_0/tau_0^2)
print(m_1)=
print(tau_1)
B <- 10^6
theta_vector <- rnorm(B, mean = m_1, sd = tau_1)
print(theta_vector)
john_vector <- c()
variance_vector <- c()
lambda_vector <- c()
for (i in 1:B){
  if (-1 + 10^(-8) < theta_vector[i] & theta_vector[i] < 1- 10^(-8)){
    temp <- theta_vector[i]
    john_vector <- c(john_vector, temp)
    variance_vector <- c(variance_vector, sigma_squared/(1-temp^2))
    lambda_vector <- c(lambda_vector, sigma_squared*(1+temp)/(1-temp))
  }
}
mean(john_vector)
quantile(john_vector, probs = c(0.05, 0.5, 0.95))
mean(variance_vector)
quantile(variance_vector, probs = c(0.05, 0.5, 0.95))
mean(lambda_vector)
quantile(lambda_vector, probs = c(0.05, 0.5, 0.95))


1-pnbinom(8, 3, 0.5)


a <- 1
b <- 1
t <- 7
y_1 <- 111
y_2 <- 97
draws <- 10^4

lambda1_post <- rgamma(draws, shape = a+ y_1, rate = b+t)
lambda2_post <- rgamma(draws, shape = a+y_2, rate = b+t)
sum(lambda1_post/lambda2_post > 1.1)


pbeta(0.5, 10, 4)






















