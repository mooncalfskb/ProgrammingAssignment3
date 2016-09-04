#need tools library for title case
library(tools)

best <- function(state, outcome) {
  ## Read outcome data
  ocm <- read.csv("/Users/mooncalf/Dropbox/skb/coursera/ProgrammingAssignment3/outcome-of-care-measures.csv")

  ##Notes:
  ##options for outcome
  ##\heart attack", \heart failure", or \pneumonia".
  #outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  #outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  #outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  
  ## Check that state and outcome are valid
  #set a vector of states. Check if user entered state is in the vector
  state_v <- ocm$State
  try(if(!state %in% state_v) stop("invalid state"))
  
  #process outcomes and convert to syntax of column header
  #I decided to to it this way so that if more outcomes were added to the data
  #you wouldn't have to rewrite function.
  #you could also write a vector of acceptable results and match to that
  outcome_prefix <- "Hospital.30.Day.Death..Mortality..Rates.from."
  #have to load the tools library for title case
  outcome <- toTitleCase(outcome)
  #replace spaces with . in outcome name
  outcome <- gsub(" ",".",outcome)
  #add prefix
  outcome <- paste(outcome_prefix,outcome,sep="")
  #check if outcome in data
  try(if(!outcome %in% colnames(ocm)) stop("invalid outcome"))
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  #pull out just the state data
  ocm_my_state <- subset(ocm,ocm$State == state)
  #convert factor to 
  outcome_data <- ocm_my_state[,eval(outcome)]
  
  # convert to numeric
  outcome_data <- as.numeric(levels(outcome_data))[outcome_data]
  head(outcome_data)
  
  #str(ocm_my_state)
  #best_h <- min(ocm_my_state[,eval(outcome)])
  #head(best_h)
  
}

best("CA","heart attack")
best("CC","heart attack")
best("CA","pneumonia")
best("CA","pneumoniaa")