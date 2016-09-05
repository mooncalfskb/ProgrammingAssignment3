#need tools library for title case
library(tools)


rankhospital <- function(state, outcome, num = "best") {
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
  
  
  ## Return hospital name in that state with the given rank
  #pull out the state data
  ocm_my_state <- subset(ocm,ocm$State == state)
  
  ##pull out just mortality and hostpital name 
  ##originally did this so I could use complete cases but it didn't work for some reason.
  ocm_my_outcome <- subset(ocm_my_state,select=c("State",eval(outcome),"Hospital.Name"))
  
  ##rename the column because sick of this eval thing
  colnames(ocm_my_outcome)[which(names(ocm_my_outcome) == eval(outcome))] <- "ThirtyDayMort"
  
  ## god tried a thousand things and finally did this to get rid of Not Availables
  ## tried complete.cases and is.na but weird results
  ocm_my_outcome <- subset(ocm_my_outcome,ocm_my_outcome$ThirtyDayMort != "Not Available")
  
  ## convert factor to numeric. forgetting to do this resulted in a bad sort the first time.
  ocm_my_outcome$ThirtyDayMort <- as.numeric(as.character(ocm_my_outcome$ThirtyDayMort))
  
  #sort outcome by 30 Day Mort first and Name second
  ocm_my_outcome <- ocm_my_outcome[order(ocm_my_outcome$ThirtyDayMort,ocm_my_outcome$Hospital.Name),]
  
  
  #make a vector of rank length of dataframe
  rank_v <- c(1:nrow(ocm_my_outcome))
  # add rank column
  ocm_my_outcome["rank"] <- rank_v
  #print(ocm_my_outcome)

  if(num == "best"){
    h <- as.character(ocm_my_outcome[1,"Hospital.Name"])
    #print("This is the best hospital")
  }else if(num == "worst"){
    h <- as.character(ocm_my_outcome[nrow(ocm_my_outcome),"Hospital.Name"])
    #print("This is the worst hospital")
  }else if(!num %in% ocm_my_outcome$rank){
    h <- NA
    #print("Num is not in the rankings")
  }else{
    h <- as.character(ocm_my_outcome[num,"Hospital.Name"])
    #print(paste("This is number ",num))
  }
  
  h
  
}

#examples to run
rankhospital("MD","heart attack","worst")
rankhospital("TX","heart failure",4)
