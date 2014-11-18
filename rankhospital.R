
rankhospital <- function (state, outcome, num = "best")
  
{
  
  library(dplyr)
  

  all_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state_db    <- as.matrix(distinct(select(all_outcome, 7)))
  disease     <- c("heart attack", "heart failure", "pneumonia")
  
  
  #check if input state and disease are valid
  check_state   <- ifelse(state %in% state_db,1,0)
  check_disease <- ifelse(outcome %in% disease,3,0)
  
  
  
  if(check_state == 0) {stop("invalid state")}
  else if(check_disease == 0) {stop("invalid outcome")}
  else
      
  {
    
    #if input is valid then execute the rest of the function
    #column number of disease to select
    
    disease_col <- if(outcome == "heart attack") {11}
    else if(outcome == "heart failure") {17}
    else {23}
    
    
    
    #extract the hospital name, state and disease columns only
    specific_outcome <- all_outcome[c(2,7,disease_col)]
    
    
    
    #Filter out missing values and pick only the hospitals in the state you are interested in
    specific_outcome <- filter(specific_outcome, specific_outcome[+c(3)] != "Not Available", specific_outcome[+c(2)] == state)
    
    ranking             <- as.numeric(specific_outcome[,3])
    specific_outcome    <- cbind(specific_outcome, ranking)
    
    
    #specific_outcome <- as.matrix(all_outcome[c(7,disease_col)])  
    #Sort the dataframe by outcome
    
    specific_outcome <- arrange(specific_outcome, specific_outcome[,4] , specific_outcome[,1])
    
        
    records <- NROW(specific_outcome)
    
    
    
    if(num == "best") {return(specific_outcome[1,1])}
       else if(num == "worst") {return(specific_outcome[records,1])}
         else if(num > records) {return("NA")}
           else {return(specific_outcome[num,1])}
    
    
  }
  
}


rankhospital("TX", "heart failure", 4)



