add2 <- function(x,y) {
  x + y
} 

above10 <- function(v) {
   v[v>10]
}

above <- function(v,f = 10) {
  v[v>f]
}

calcColMean <- function(m, removeNA) {
  a = vector(length = ncol(m))
  for (j in 1:ncol(m)) {
    a[j] <- mean(y[,j], na.rm = removeNA)
  }
  a
}

cube <- function(x, n) {
  x^3
}

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}
