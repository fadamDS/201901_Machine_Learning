row <- 100
col <- 100
x <- matrix(rnorm(row*col,0,1),nrow = row,ncol = col)

# Defining Vector norm function
vec_norm <- function(x){
  sqrt(sum(x^2))
}



n <- 10

# Simulating variance of the squared norm of the uniform
result <- rep(NA,sims)

for(i in 1:sims){
  temp <- rep(NA,100)
  for(j in 1:100){
    x <- runif(n,-1,1)
    temp[j] <- (vec_norm(x))^2
  }
  result[i] <- (sd(temp))^2
}


hist(result)
n*4/45

n <- 100
# Simulating distribution of squared norm of the uniform
result <- rep(NA,sims)
for(i in 1:sims){
  x <- runif(n,-1,1)
  result[i] <- (vec_norm(x))^2
}
hist(result)
(sd(result))^2

# Defining cosine function
cos_vec <- function(x,y){
  x %*% y / (vec_norm(x) * vec_norm(y))
}

sims <- 1000
n <-1000

result <- rep(NA,sims)
for( i in 1:sims){
  x <- runif(n,-1,1)
  x_prime <- runif(n,-1,1)
  result[i] <- cos_vec(x,x_prime)
}

hist(result)

plot(c(1:100),sqrt(c(1:100)),type ="l")

cos(x_prime,x)

# Simulating Gaussian Case
sims <- 1000
n <-100

result <- rep(NA,sims)


for( i in 1:sims){
  x <- rnorm(n,0,1)
  x_prime <- rnorm(n,0,1)
  result[i] <- cos_vec(x,x_prime)
}

hist(result)
1/sqrt(n)