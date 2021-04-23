set.seed(111)

n = 10^4
theta_star <- .7

x_vector <- rnorm(n, mean = 0, sd=2)

y_vector <- c()
for(i in 1:n){
  current_y <-rnorm(1, mean = x_vector[i] * theta_star, sd = 10)
  y_vector <- c(y_vector, current_y)
}

theta_1 = x_vector[1] * y_vector[1] / (x_vector[1])^2

j_values <- (1:n)

thetas <- c(theta_1)

U_tildes <- c(y_vector[1])

for (i in 2:n){
  U_tilde_i <- y_vector[i] - thetas[length(thetas)]*x_vector[i]
  U_tildes <- c(U_tildes, U_tilde_i)
  d_i = x_vector[i] / sum(x_vector[1:i]^2)
  theta_current = thetas[length(thetas)] + d_i * U_tilde_i
  thetas <- c(thetas, theta_current)
}

plotted_thetas <- thetas[5:length(thetas)]
plotted_js <- j_values[5:length(thetas)]
plot(plotted_js, plotted_thetas, main = "Theta hat V.S. j", ylab = "Theta_j value", xlab = "j")




