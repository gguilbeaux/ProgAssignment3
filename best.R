##
## best.R
##
## This function reads the outcome-of-care-measures.csv file and returns a 
## character vector with the name of the hospital that has the best 
## (i.e. lowest) 30-day mortality for the specified outcome in that state. 
## The hospital name is the name provided in the Hospital.Name variable. 
## The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”. 
## Hospitals that do not have data on a particular outcome are excluded 
## from the set of hospitals when deciding the rankings.
##
## If there is a tie for the best hospital for a given outcome, then the 
## hospital names are sorted in alphabetical order and the first hospital 
## in that set is chosen (i.e. if hospitals “b”, “c”, and “f” are tied for
## best, then hospital “b” is returned).

best <- function(state, outcome) {
  ## Read outcome data
  data_table <- read.csv("outcome-of-care-measures.csv")
  
  ## Check if arguments are valid
  
  ## Check if the state specified is a valid US state
  if (!is.element(state, state.abb)) {
    stop("invalid state")
  }

  ## Change outcome to correct column name 
  ## If not one of the allowed outcomes, then stop
  if (outcome == "heart attack") {
    outcome <- "Heart.Attack"
  } else
  if (outcome == "heart failure") {
    outcome <- "Heart.Failure"
  } else
  if (outcome == "pneumonia") {
    outcome <- "Pneumonia"
  } else {
    stop("invalid outcome")
  }

  ## Find row number of the hospital in that state with lowest 30-day death rate
    outcomeCol <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep = "")
    subTable <- subset(data_table, State == state & outcomeCol != "Not Available")
    rowMin <- which.min(subTable[, outcomeCol])
    
  ## Return Hospital Name
    subTable[rowMin, "Hospital.Name"]
}
