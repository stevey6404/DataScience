rankhospital <- function(chosenstate, chosenoutcome, chosennum){
  
  ## set directory
  setwd("/Users/stevenliu/DataScience/Coursera/R Programming/Week 4/Assignment_3")
  ##load outcome data
  bigdata<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## creates outcome list to compare invalid outcomes later on
  outcomelist <- c("heart attack","heart failure", "pneumonia")
  
  ## convert figures from character to numeric.
  suppressWarnings(bigdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(bigdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
  suppressWarnings(bigdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(bigdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
  suppressWarnings(bigdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(bigdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))

  
  ## selecting the right outcome and picking the ranked hospital
  if(chosenoutcome =="heart attack"){
    statedata <-bigdata[ ,c(2,7,11)]
    statedata <-subset(statedata,statedata$State==chosenstate) ##filtering via state
    statedata <-statedata[order(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,statedata$Hospital.Name),] ##ranking the hospital
    statedata <- na.omit(statedata) ##omitting NA hospitals
    statedata$Rank <- 1:nrow(statedata) ##ranking hospital
    if (chosennum =="best"){  ##condition for best
      statedata[1,1]
    }
    else if (chosennum == "worst"){ ##condition for worst
      statedata[max(statedata$Rank),1]
    }
    else if (chosennum > max(statedata$Rank)){ ##condition where rank exceeds number of ranked hospital in that state
      stop("invalid ranking")
    }
    else{
      statedata[chosennum,1]} ##chosen rank
  }
  
  else if(chosenoutcome =="heart failure"){
    statedata <-bigdata[ ,c(2,7,17)]
    statedata <-subset(statedata,statedata$State==chosenstate)
    statedata <-statedata[order(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,statedata$Hospital.Name),]
    statedata <- na.omit(statedata)
    statedata$Rank <- 1:nrow(statedata)
    if (chosennum =="best"){
      statedata[1,1]
    }
    else if (chosennum == "worst"){
      statedata[max(statedata$Rank),1]
    }
    
    else if (chosennum > max(statedata$Rank)){
      stop("invalid ranking")
    }
    else{
      statedata[chosennum,1]}
  }
  
  else {
    statedata <-bigdata[ ,c(2,7,23)]
    statedata <-subset(statedata,statedata$State==chosenstate)
    statedata <- statedata[order(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,statedata$Hospital.Name),]
    statedata <- na.omit(statedata)
    statedata$Rank <- 1:nrow(statedata)
    if (chosennum =="best"){
      statedata[1,1]
    }
    else if (chosennum == "worst"){
      statedata[max(statedata$Rank),1]
    }
    
    else if (chosennum > max(statedata$Rank)){
      stop("invalid ranking")
    }
    else{
      statedata[chosennum,1]}
  }
  
  
} ## function}


##### Test  ######
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
rankhospital("AK", "heart attack", "best")