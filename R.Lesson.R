# understand the fundamentals to learn everything else

# some arithmetic 
4+4


# saving variables

x <- 5
x
x/2

# concatenating variables
y <- c(3,6)
y
y[1]
y[2]

# R is vectorized

y+x

#if statements

3>4

5>2

5 == 5

if (3 > 4){
  print("nice!")
}

if (1>5) {
  print("hi!")
}

# functions do things
y <- c(5,6)
sum(y)
mean(y)

FUN_doublesum <- function(x){
  return(2*sum(x))
}

FUN_doublesum(y)

FUN_divide <- function(x,y){
  return (x/y)
}

FUN_divide(6,3)
FUN_divide(6)

# multi-line
FUN_divide <- function(x,y){
  z <- x/y
  return (z)
}

# 110 funtions
?rnorm
rnorm(10, mean = 7, sd = 30)

# for loop
n <- 10
x <- rep(0, n)
x

for (i in 2:n){
  x[i] <- x[i-1] + rnorm(1)
}
x



# while loop
x <- 0
x

while(max(x) < 10){
  x <- c(x, x[length(x)] +rexp(1))
}
x

# combined!
n <- 10
x <- rep(0,n)
for(i in 2:n){
  
  while (TRUE){
    const <- rnorm(1)
    if (const >1) {
      break
    }
  }
  x[i] <- x[i-1] + const
}
x

# replicate
set.seed(111)
rnorm(1)
rnorm(1)

# deja vu
set.seed(111)
rnorm(1)

#replicate
set.seed(111)
nsims <- 100000

switch_win <- stay_win <- rep(0, nsims)
doors <- 1:3

# monty function
FUN_monty <- function(){
  return(sample(1:3, 1))
}


# iterate: assume we pick door 1
for (i in 1:nsims){
  car_door <- FUN_monty()
  
  # monty door 
  if (car_door ==1){
    monty_door <- sample(2:3, 1)
  }
  
  if (car_door == 2){
    monty_door <- 3
  }
  if (car_door == 3){
    monty_door <- 2
  }
  # get switch and stay doors
  stay_door <- 1
  if (monty_door == 2){
    switch_door <- 3
  }
  
  if (monty_door == 3){
    switch_door <- 2
  }
  
  #see if we won
  if (stay_door == car_door){
    stay_win[i] <- 1
  }
  
  if (switch_door == car_door){
    switch_win[i] <- 1
  }
}

head(switch_win, 10)

mean(stay_win)
mean(switch_win)


n <- 10
x <- 1:n

for (i in 2:n){
  y[i] <- y[i-1] +rnorm(1)
}

# basic plot
plot(x,y)
plot(x,y, type = "l")
plot(x,y, 
     type = "l"
     lty = 2,
     col = "red"
     lwd = 3
     xlab = "x-value"
     ylab = "y-value"
     main = "Nice plot")


























