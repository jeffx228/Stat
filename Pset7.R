set.seed(111)

# 1
c_10 <- qgamma(0.95,10,30)
c_50 <- qgamma(0.95,50,150)

beta_10 <- c()
beta_50 <- c()

for (i in 1:7){
  beta_10[i] <- 1 - pgamma(c_10,10, 10*(i-1))
  beta_50[i] <- 1 - pgamma(c_50, 50, 50*(i-1))
}
plot(c(0:6), beta_10, main="Power Function for n=10", xlab="lambda", ylab="Power")
plot(c(0:6), beta_50, main="Power Function for n=50", xlab="lambda", ylab="Power")






# 3
qhyper(0.95, 100,400,50)

beta <- c()
for (i in 100:700){
  beta[i-99] = 1-phyper(16,100,i-100,50)
}

plot(c(100:700), beta, main="Power Function", xlab ="N", ylab="Power")
