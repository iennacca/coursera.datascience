# for test purposes only
# tdd <- NULL
# tcn <- NULL
# tss <- NULL

rankhospital <- function(state,outcome,num) {
  DD <- NULL
  CN <- NULL
  HN <- "Hospital.Name"
  SN <- "State"
  
  isCorrectState <- function(state) {
    state  %in% unique(DD[,SN])
  }
  
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
  
  subsetByStateOutcome <- function(state) {
    sso <- DD[ DD$State == state, c(HN, SN, CN)]
    sso <- sso[complete.cases(sso), ]
    
    sso[,CN] <- convertFactorToNumeric(sso[,CN])
    sso
  }

  orderSetByOutcomeAndName <- function(sso) {
    sso <- sso[order(sso[,CN], sso[,HN]), ]
    sso <- cbind(sso,hrank=seq_len(nrow(sso)))
  }  

  getHospitalNameByNum <- function(sso, num) {
    if (num == "worst") {
      as.character(tail(sso[, HN], 1))
    } else if (num == "best") {
      as.character(head(sso[, HN], 1))
    } else {
      as.character(sso[num, HN])
    }
  }
  
  # main function
  DD <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  if (!isCorrectState(state)) stop("Invalid state")
  CN <- createOutcomeColumnName(outcome)
  if (!isCorrectOutcome()) stop("Invalid outcome")
  
  sso <- subsetByStateOutcome(state)
  sso <- orderSetByOutcomeAndName(sso)
  
  # for test purposes only
  # tdd <<- DD
  # tss <<- sso
  # tcn <<- CN
  
  getHospitalNameByNum(sso, num)
}