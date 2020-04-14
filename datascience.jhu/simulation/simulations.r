# simulation: linear models

set.seed(20)
x <- rnorm(100)
e <- rnorm(100,0,2)
y <- 0.5  + 2 * x + e
summary(y)
hist(y)
plot(x,y)

# simulation: binomial distribution

set.seed(10)
x <- rbinom(100,1,0.5)
e <- rnorm(100,0,2)
y <- 0.5 + 2*x + e
summary(y)
plot(x,y)

# simulation: Poisson distribution

set.seed(1)
x <- rnorm(100)
log.mu <- 0.5 + 0.3*x
y <- rpois(100, exp(log.mu))
summary(y)
plot(x,y)

# simulation: ramdom sampling

set.seed(1)
sample(1:10,4)
sample(1:10,4)
sample(letters,5)
sample(1:10)
sample(1:10)
sample(1:10, replace=TRUE)
