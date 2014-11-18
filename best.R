
best <- function (state, outcome)
  
{
  library(dplyr)
  
  
  all_outcome   <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state_db      <- as.matrix(distinct(select(all_outcome, 7)))
  disease       <- c("heart attack", "heart failure", "pneumonia")
  
  
  #check if input state and disease are valid
  check_state   <- ifelse(state %in% state_db, 1, 0)
  check_disease <- ifelse(outcome %in% disease,3,0)
  
  
  if(check_state == 0) {stop("invalid state")}
    else if(check_disease == 0) {stop("invalid outcome")}
         else
           
       {
         
         
         
         
   #      state   <- "NY"
  #       outcome <- "pneumonia"
         
         
         
         
         
         
  #if input is valid then execute the rest of the function

  #column number of disease to select
         x<-data.frame(Disease = c("heart attack", "heart failure", "pneumonia"), Col_No = c(11,17,23))
         
         vlookup     <- function(val, df, col) {df[df[1] == val, col]} 
         disease_col <- vlookup(outcome, x, 2)
         
         
#  disease_col <- if(outcome == "heart attack")  {11}
#                    else if(outcome == "heart failure") {17}
#                            else {23}



 #extract the hospital name, state and disease columns only 
 specific_outcome    <- all_outcome[c(2,7,disease_col)]



 #Filter out missing values and pick only the hospitals in the state you are interested in 
 specific_outcome <- filter(specific_outcome, specific_outcome[+c(3)] != "Not Available", specific_outcome[+c(2)] == state)


ranking             <- as.numeric(specific_outcome[,3])
specific_outcome    <- cbind(specific_outcome, ranking)


 #Sort the dataframe by outcome
 specific_outcome  <- arrange(specific_outcome, specific_outcome[,4] , specific_outcome[,1])
 
 
      }

 #Return the hospital name in row 1
 return(specific_outcome[1,1])
 

}


#best("TX", "heart attack")
best("TX", "heart failure")
#best("MD", "heart attack")
#best("MD", "pneumonia")
#best("BB", "heart attack")
