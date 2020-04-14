
corr <- function(directory, threshold = 0) {
  cv <- numeric()

  for (fn in list.files(directory)) {
    fn <- sprintf("%s/%s", directory, fn)
    df <- read.csv(fn)
    
    cc <- df[complete.cases(df),]
    if (nrow(cc) <= threshold) {
      next
    }
    
    x <- cc[,"nitrate"]
    y <- cc[,"sulfate"]
    cv <- c(cv,cor(x,y))
  }
  cv
}