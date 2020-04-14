
complete <- function(directory, vID = 1:332) {
  # non-optimized way; ideally, we should just 
  # calculate complete.cases() while traversing the list 
  # of files
  df <- getDataFrame(directory, vID)
  
  cc <- df[complete.cases(df),]
  
  ccr <- data.frame(matrix(ncol=2))
  colnames(ccr) <-  c("id","nobs")
  
  # print(ccr)
  
  for (i in vID) {
    # print(sprintf("%.3d: %d", i, nrow(cc[cc$ID == i,])))
    ccr <- rbind(ccr, c(i, nrow(cc[cc$ID == i,])))
  }
  
  # data.frame() creates a NULL row at first; this removes it
  ccr <- ccr[-c(1),]
  
  ccr
}