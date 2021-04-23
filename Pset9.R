set.seed(111)
library(ggplot2)


#1
games = 627
seats = 398

## e
lambdas <- seq(1e-7, 1e-4, 1e-8)
prior_df <- data.frame(lambdas,y=dgamma(lambdas, .01, .01))
likelihood_df <- data.frame(lambdas, y = exp(-games * seats * lambdas) * (lambdas)^10)
posterior_df <- data.frame(lambdas, y = dgamma(lambdas, 10.01, .01 + games * seats))

ggplot() + geom_line(data=likelihood_df, aes(x=lambdas, y=y, color="Likelihood"), size = 0.8) + ylab("Likelihood")+
  ggtitle("Likelihood Function") + xlab("Lambda")

ggplot() + geom_line(data=prior_df, aes(x=lambdas, y=y, color="Prior"), size = 0.8) + ylab("Density")+
  ggtitle("Prior Density") + xlab("Lambda")

ggplot() + geom_line(data=posterior_df, aes(x=lambdas, y=y, color="Posterior"), size = 0.8) + ylab("Density")+
  ggtitle("Posterior Density") + xlab("Lambda")


## f
qgamma(.025, 10.01, .01 + games* seats)
qgamma(.975, 10.01, .01 + games* seats)


## g
b = 10^4
lambdas <- rgamma(b, 10.01, .01 + games* seats)
thetas <- 1 - exp(-lambdas)
thetas <- sort(thetas)
thetas[.025 * 1000]
thetas[.975 * 1000]
###

# 4d Risk Plots
set.seed(111)
b = 10^6
n = 20
s = 1
t = 1

u_vector = rnorm(b, 0, s / sqrt(n))

mu_fn <- function(x){
  d = sqrt(2)*s^2/t
  ans = ifelse(x > d/n, x - d/n, ifelse(x < -d/n, x + d/n, 0))
  return(ans)
}
r_mubar_mu <- function(mu_vector){
  ans = c()
  for (i in 1:length(mu_vector)){
    sum_b = 0
    sum_b = sum((mu_fn(mu_vector[i] + u_vector) - mu_vector[i])^2)
    ans = c(ans, sum_b / b)
  }
  return(ans)
}
r_ybar_mu <- function(mu){
  return (s^2/n)
}
c_n = n * t^2 / (n * t^2 + s^2)
r_mutilde_mu <- function(mu){
  return (mu^2 * (c_n - 1)^2 + c_n^2 * s^2 / n)
}
mus = seq(-3, 3, 0.01)
df1 <- data.frame(mus, y=r_mubar_mu(mus))
df2 <- data.frame(mus, y=r_ybar_mu(mus))
df3 <- data.frame(mus, y = r_mutilde_mu(mus))
ggplot() +
  geom_line(data=df1, aes(x=mus, y=y, color="Mu bar risk"), size = 0.8) +
  geom_line(data=df2, aes(x=mus, y=y, color="Y bar risk"), size = 0.8) +
  geom_line(data=df3, aes(x=mus, y=y, color="Mu tilde risk"), size = 0.8) +
  scale_colour_manual(values=c("skyblue","red","green")) +
  ylab("Risk")+
  ggtitle("Risk")+
  xlab("Mu")

