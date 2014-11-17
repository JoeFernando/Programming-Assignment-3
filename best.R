
best <- function (state, outcome)
{
  library(dplyr)
  
  
  all_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state       <- distinct(select(all_outcome, 7))
  disease     <- c("heart attack", "heart failure", "pneumonia")
  
  
  #check if input state and disease are valid
  
  
  
  
  #if input is valid then execute the rest of the function
  #column number of disease to select
  outcome     <- "heart attack"
  #disease_col <- 11
  state       <- "TX"
  
  
  disease_col <- if(outcome == "heart attack")  {11}
  
                    else if(outcome == "heart failure") {17}
  
                            else {23}
  
  
  
 #extract the hospital name, state and disease columns only 
 specific_outcome <- all_outcome[c(2,7,disease_col)]
 
 #Filter out missing values and pick only the hospitals in the state you are interested in 
 specific_outcome <- filter(specific_outcome, specific_outcome[+c(3)] != "Not Available", specific_outcome[+c(2)] == state)
  
 #specific_outcome <- as.matrix(all_outcome[c(7,disease_col)])
 
 #ranking          <- (specific_outcome[+c(2)])
 #ranking          <- as.numeric(as.matrix(ranking))
 
 #specific_outcome <- cbind(specific_outcome, ranking)
 
 #str(specific_outcome)

 #Sort the dataframe by outcome
 specific_outcome  <- arrange(specific_outcome, specific_outcome[,3] , specific_outcome[,1])
 
 #Return the hospital name in row 1
 return(specific_outcome[1,1])
 
}