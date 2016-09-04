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
  #pull out the state data
  ocm_my_state <- subset(ocm,ocm$State == state)
  ##print(head(ocm_my_state))

  ##pull out just mortality and hostpital name 
  ##originally did this so I could use complete cases but it didn't work for some reason.
  ocm_my_outcome <- subset(ocm_my_state,select=c(eval(outcome),"Hospital.Name"))
  
  ##rename the column because sick of this eval thing
  colnames(ocm_my_outcome)[which(names(ocm_my_outcome) == eval(outcome))] <- "ThirtyDayMort"
  #print(head(ocm_my_outcome))
  
  ## god tried a thousand things and finally did this to get rid of Not Availables
  ## tried complete.cases and is.na but weird results
  ocm_my_outcome <- subset(ocm_my_outcome,ocm_my_outcome$ThirtyDayMort != "Not Available")
  ## convert factor to numeric
  ocm_my_outcome$ThirtyDayMort <- as.numeric(as.character(ocm_my_outcome$ThirtyDayMort))
  #subset by minimum
  min_mort <- subset(ocm_my_outcome,ocm_my_outcome$ThirtyDayMort == min(ocm_my_outcome$ThirtyDayMort))
  #sort by hospital name
  min_mort <- min_mort[order(min_mort$Hospital.Name),]
  # convert from factor to character
  best_h <- as.character(min_mort[1,"Hospital.Name"])
  best_h
  
}

#examples to run
best("MD","heart attack")
#best("CC","heart attack")
#best("MD","pneumonia")
#best("CA","pneumonia")
#best("MI","heart failure")