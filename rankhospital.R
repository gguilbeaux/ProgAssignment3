#
# rankhospital.R
#
# This function reads the outcome-of-care-measures.csv file and returns a 
# character vector with the name of the hospital that has the ranking 
# specified by the num argument. 
#
# For example,
#   rankhospital("MD", "heart failure", 5) 
#     returns a character vector containing the name of the hospital with
#     the 5th lowest 30-day death rate for heart failure. 
# 
# If the number given by num is larger than the number of hospitals in that
# state, then the function should return NA. Hospitals that do not have
# data on a particular outcome should be excluded from the set of hospitals
# when deciding the rankings.
# 
# Handling ties. It may occur that multiple hospitals have the same 30-day
# mortality rate for a given cause of death. In those cases ties are
# broken by using the hospital name. 

rankhospital <- function(state, outcome, num) {
  # state = two letter state abbreviation
  # outcome = "heart attack”, “heart failure”, or “pneumonia”
  # num = “best”, “worst”, or an integer indicating the ranking
  #   (smaller numbers are better).
  # 
  # Read outcome data
  data_table <- read.csv("outcome-of-care-measures.csv")
  
  # Check if the state specified is a valid US state
  states <- unique(data_table[, "State"])
  if (!is.element(state, states)) {
    stop("invalid state")
  }

  # Change outcome to correct column name
  # If not one of the allowed outcomes, then print invalid and stop
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

  # Check num argument
  if (!is.element(num, c("best", "worst")) & !is.numeric(num)) {
    stop("invalid num")
  }

  # Filter data by state and remove missing values
  outcomeCol <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep = "")
  data_table[, outcomeCol] <- as.numeric(data_table[, outcomeCol])
  
  # Order data by outcome then by hospital name to handle ties
  state_data <- data_table[data_table[, "State"] == state, ]
  state_data <- state_data[!is.na(state_data[, outcomeCol]), ]
  ordered <- state_data[order(state_data[, outcomeCol], state_data[, "Hospital.Name"]), ]
  
  ## Determine hospital ranking
  if (num == "best") {
    rank <- 1
  } else if (num == "worst") {
    rank <- nrow(ordered)
  } else {
    rank <- num
  }
  
  ## Return hospital name if valid rank, otherwise NA
  if (rank <= nrow(ordered)) {
    ordered[rank, "Hospital.Name"]
  } else {
    NA
  }    
}  
