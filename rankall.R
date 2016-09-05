#need tools library for title case
library(tools)


rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ocm <- read.csv("/Users/mooncalf/Dropbox/skb/coursera/ProgrammingAssignment3/outcome-of-care-measures.csv")
  
  
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
  
  
  ## For each state, find the hospital of the given rank  
  
  my_states <- unique(ocm$State)
  all_states <- data.frame()
  
  for (this_state in my_states){
    
    ## Return hospital name in that state with the given rank
    #pull out the state data
    ocm_my_state <- subset(ocm,ocm$State == this_state)
    
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
    ocm_my_outcome <- ocm_my_outcome[order(ocm_my_outcome$State,ocm_my_outcome$ThirtyDayMort,ocm_my_outcome$Hospital.Name),]
    #print(ocm_my_outcome)
    
    #make a vector of rank length of dataframe
    rank_v <- c(1:nrow(ocm_my_outcome))
    # add rank column
    ocm_my_outcome["rank"] <- rank_v
    #print(str(ocm_my_outcome))

    #make a column for worst
    #I decided to do it this way because other ways seemed to involve running this loop again further in the script
    #seemed like this was an adequate if ham-handed solution
    worst_v <- rep("-",nrow(ocm_my_outcome)-1)
    worst_v <- as.character(worst_v)
    worst_v <- c(worst_v, "worst")
    # add rank column
    ocm_my_outcome["worst"] <- worst_v    
    
    all_states <- rbind(all_states, ocm_my_outcome)
    
    
  }
  
  #resort by state
  all_states <- all_states[order(all_states$State,all_states$rank),]
  

  if(num == "best"){
    
    h <- subset(all_states,all_states$rank == 1)

    #print("This is the best hospital")
  }else if(num == "worst"){
    #seemed like a second loop would be required here unless I solved problem above
    #I'm sure there's a fancier way to do this, but needed to finish.
    h <- subset(all_states,all_states$worst == "worst")
    
    #print("This is the worst hospital")
  }else if(!num %in% all_states$rank){
    h <- NA
    #print("Num is not in the rankings")
  }else{
    h <- subset(all_states,all_states$rank == num)
    #print(paste("This is number ",num))
  }
  
  # cut down to two columns
  if (is.data.frame(h)){
    h <- subset(h,select=c("State","Hospital.Name"))
    
  }
  h
}

#examples to run
#rankall("heart failure","worst")
#rankall("heart failure",4)
tail(rankall("pneumonia", "worst"), 5)
head(rankall("heart attack", 20), 10)
tail(rankall("heart failure"), 10)