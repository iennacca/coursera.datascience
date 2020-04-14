# for test purposes only
# tdd <- NULL
# tcn <- NULL
# tss <- NULL

rankall <- function(outcome,num="best") {
  DD <- NULL
  CN <- NULL
  HN <- "Hospital.Name"
  SN <- "State"
  
  createOutcomeColumnName <- function(outcome) {
    colOutcome <- "Hospital.30.Day.Death..Mortality..Rates.from."
    outcome <- gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", outcome, perl=TRUE)
    outcome <- gsub("\\s+", ".", outcome, perl=TRUE)
    paste(colOutcome, outcome, sep="")
  }
  
  isCorrectOutcome <- function() {
    CN %in% colnames(DD)
  }
  
  convertFactorToNumeric <- function(cf) {
    cf <- as.numeric(cf)
    # cf <- as.numeric(levels(cf)[cf])
  }
  
  subsetByOutcome <- function() {
    sso <- DD[ ,c(HN, SN, CN)]
    sso <- sso[complete.cases(sso), ]
    
    sso[,CN] <- convertFactorToNumeric(sso[,CN])
    sso
  }
  
  orderSetByStateOutcome <- function(sso) {
    sso <- sso[order(sso[,SN], sso[,CN], sso[,HN]), ]
    sso <- split(sso$Hospital.Name, sso$State)
  }  
  
  getHospitalNamesByNum <- function(sso, num) {
    if (num == "worst") {
      lapply(sso, function(sl) tail(sl,1))
    } else if (num == "best") {
      lapply(sso, function(sl) head(sl,1))
    } else {
      lapply(sso, function(sl) sl[num])
    }
  }
  
  # main function
  DD <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  CN <- createOutcomeColumnName(outcome)
  if (!isCorrectOutcome()) stop("Invalid outcome")
  
  sso <- subsetByOutcome()
  sso <- orderSetByStateOutcome(sso)
  sso <- getHospitalNamesByNum(sso, num)

  sso <- data.frame(hospital=unlist(sso), state=names(sso), row.names=names(sso))
  
  # for test purposes only
  # tdd <<- DD
  # tss <<- sso
  # tcn <<- CN
  
  sso
}