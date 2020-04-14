# for test purposes only
# tdd <- NULL
# tcn <- NULL
# tss <- NULL

best <- function(state,outcome) {
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
  
  subsetStateOutcome <- function(state) {
    sso <- DD[ DD$State == state, c(HN, SN, CN)]

    sso[,CN] <- convertFactorToNumeric(sso[,CN])

    # for test purposes only
    # tss <<- sso
    sso
  }
  
  vectorMin <- function(sso) {
    mcn <- min(sso[,CN], na.rm=TRUE)
    smn <- sso[ sso[[CN]] == mcn, ]
  }
  
  getFirst <- function(vm) {
    head(sort(vm[[HN]]), 1)
  }

  # main function
  DD <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  if (!isCorrectState(state)) stop("Invalid state")
  CN <- createOutcomeColumnName(outcome)
  if (!isCorrectOutcome()) stop("Invalid outcome")

  sso <- subsetStateOutcome(state)
  vmn <- vectorMin(sso)
  
  ret <- as.character(getFirst(vmn))
  
  # for test purposes only
  # tdd <<- DD
  # tcn <<- CN
  
  ret
}