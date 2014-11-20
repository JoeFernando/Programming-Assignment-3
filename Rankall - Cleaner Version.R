rankall <- function (outcome, num = "best")
{
  library(dplyr)
  all_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state_db <- as.matrix(distinct(select(all_outcome, 7)))
  disease <- c("heart attack", "heart failure", "pneumonia")
  counter <- NROW(state_db)
  #output <- data.frame(hospital = character(0), state = character(0))
  output <- data.frame()
  output1 <- data.frame()
  output2 <- data.frame()
  #Sort state_db
  state_db <- sort(state_db)
  #check if input disease is valid
  check_disease <- ifelse(outcome %in% disease,3,0)
  if(check_disease == 0) {stop("invalid outcome")}
  else
  {
    #if input is valid then execute the rest of the function
    #column number of disease to select
    disease_col <- if(outcome == "heart attack") {11}
    else if(outcome == "heart failure") {17}
    else {23}
   
    #figure out the ranking number to read for each state
    num <- if(num == "best") {1} else {num}
    
    #extract the hospital name, state and disease columns only
    specific_outcome <- all_outcome[c(2,7,disease_col)]
    
    #Filter out missing values and pick all the hospitals with a valid "outcome"
    specific_outcome <- filter(specific_outcome, specific_outcome[+c(3)] != "Not Available")
    #Convert to Numeric
    
    ranking <- as.numeric(specific_outcome[,3])
    specific_outcome <- cbind(specific_outcome, ranking)
    
    #extract the hospital names for the spcified rank and rbind then to the output data frame
    for(i in 1:counter)
    {
      state <- state_db[i]
      state_outcome <- filter(specific_outcome, specific_outcome[+c(2)] == state)
      #Sort the dataframe by outcome
      
        if(num == "worst")
        {state_outcome <- arrange(state_outcome, desc(state_outcome[,4]), state_outcome[,1])
           }
          else
          {state_outcome <- arrange(state_outcome, state_outcome[,4], state_outcome[,1])}

      
        hospital <- if(!is.null(state_outcome[num,1])) {state_outcome[ifelse(num =="worst",1,num),1]} else {"<NA>"}
        output1 <- rbind(output1, as.data.frame(hospital))
        output2 <- rbind(output2, as.data.frame(state))
     }
    }

  output <- cbind(output1, output2)
}

head(rankall("heart attack", 20), 10)

tail(rankall("pneumonia", "worst"), 3)
