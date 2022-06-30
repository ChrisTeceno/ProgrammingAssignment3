rankall <- function(outcome, num = "best") {
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
    states <- unique(data[,7])
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if(outcome == 'heart attack')
      results <- data[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name, na.last = NA),]
    else if(outcome == 'heart failure')
      results <- data[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name, na.last = NA),]
    else if(outcome == 'pneumonia')
      results <- data[order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name, na.last = NA),]
    #subset state and outcome
    resultsdf <- data.frame(matrix(ncol = 2, nrow = 0))
    x <- c("hospital", "state")
    colnames(resultsdf) <- x
    for (state in states){
      statedata <- subset(results, State == state)
      if (num == "best")
        hospital <- (statedata$Hospital.Name[1])
      else if (num == "worst")
        hospital <-  (statedata$Hospital.Name[nrow(statedata)])
      else
        hospital <-  (statedata$Hospital.Name[num])
      new_result <- data.frame(hospital,state)
      resultsdf<- rbind(resultsdf, new_result)
    }
    return (resultsdf[order(resultsdf$state),])
}