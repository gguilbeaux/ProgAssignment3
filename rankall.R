#
# rankall.R
#
# This function reads the outcome-of-care-measures.csv file and returns a 
# 2-column data frame containing the hospital in each state that has the 
# ranking specified in num. 
# 
# For example,
#   rankall("heart attack", "best") returns a data frame containing the names 
#   of the hospitals that are the best in their respective states for 30-day 
#   heart attack death rates. The function returns a value for every state 
#   (some may be NA). The first column in the data frame is named hospital, 
#   which contains the hospital name, and the second column is named state, 
#   which contains the 2-character abbreviation for the state name. Hospitals 
#   that do not have data on a particular outcome are excluded from the 
#   set of hospitals when deciding the rankings.
# 
# Handling ties. It may occur that multiple hospitals have the same 30-day
# mortality rate for a given cause of death. In those cases ties are
# broken by using the hospital name. 

rankall <- function(outcome, num = "best") {
  # outcome = "heart attack”, “heart failure”, or “pneumonia”
  # num = “best”, “worst”, or an integer indicating the ranking
  #   (smaller numbers are better).
  
  # Read outcome data
  data_table <- read.csv("outcome-of-care-measures.csv")
  
  # Change outcome to correct column name
  # If not one of the allowed outcomes, then print invalid and stop
  outcomeArg <- outcome  ## save original value to use later
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
  
  ## Determine hospital ranking
  if (num == "best") {
    rank <- 1
  } else if (num == "worst") {
    rank <- nrow(ordered)
  } else {
    rank <- num
  }
  
  ## Apply rankhospital to all states in the data set
  states <- unique(data_table[, "State"])
  hospitalsRanked <- sapply(states, rankhospital, outcome = outcomeArg, num = num)
  
  ## Create & return ordered data frame
  unordered <- data.frame(hospital = hospitalsRanked, state = states)
  ordered <- unordered[order(unordered$"state"), ]
  ordered
} 
