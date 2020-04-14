getDataFrame <- function(directory, vID) {
  df <- data.frame()
  
  for (id in vID) {
    filePollutants <- sprintf("%s/%.3d.csv", directory,id)
    # print(filePollutants)

    if (!file.exists(filePollutants)) {
      stop(paste("Error: ", filePollutants))
    }
    
    df <- rbind(df, read.csv(filePollutants))
  }
  df
}

pollutantmean <- function(directory, pollutantName, vID = 1:322) {
  df <- getDataFrame(directory, vID)
  
  mean(df[[pollutantName]], na.rm = TRUE)
}
