rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  location <- "rprog_data_ProgAssignment3-data/"
  outcome_file <- "outcome-of-care-measures.csv"
  outcome_loc <- paste(location, outcome_file, sep = "")
  data <- read.csv(outcome_loc, na.strings="Not Available")
  ## Check that state and outcome are valid
  if(!state %in% data[,7]){
    stop("invalid state")
  }
  outcomes <- c('heart attack','heart failure','pneumonia')
  if(!outcome %in% outcomes){
    stop('invalid outcome')
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if(outcome == 'heart attack')
    results <- data[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name, na.last = NA),]
  else if(outcome == 'heart failure')
    results <- data[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name, na.last = NA),]
  else if(outcome == 'pneumonia')
    results <- data[order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name, na.last = NA),]
  #subset state and outcome
  statedata <- subset(results, State == state)
  if (num == "best")
    return (statedata$Hospital.Name[1])
  else if (num == "worst")
    return (statedata$Hospital.Name[nrow(statedata)])
  else
    return (statedata$Hospital.Name[num])
}