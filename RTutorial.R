set.seed(111)
x <- rpois(20, 3)
x
likelihoods <- c()

lambda <- 0.1

for (i in 1:50){
  lambda <- 0.1*i
  func <- 1
  for (k in x){
    func <- func*exp(-lambda)*lambda^k/(factorial(k))
  }
  likelihoods <- c(likelihoods, func)
}
length(likelihoods)
plot(type = "l", likelihoods)

for (i in 1:50){
  if (likelihoods[i] == max(likelihoods)){
    print(i/10)
  }
}




