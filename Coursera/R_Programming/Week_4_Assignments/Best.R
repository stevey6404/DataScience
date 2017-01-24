best <- function(chosenstate,chosenoutcome) {

## set directory
  setwd("XXXXXX")
  ##load outcome data
  bigdata<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## creates outcome list to compare invalid outcomes later on
  outcomelist <- c("heart attack","heart failure", "pneumonia")
  
  ## convert figures from character to numeric.
  suppressWarnings(bigdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(bigdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
  suppressWarnings(bigdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(bigdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
  suppressWarnings(bigdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(bigdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))

  ## take necessary columns
  statedata <-bigdata[ ,c(2,7,11,17,23)]

  ## if statement for invalid state and outcome inputs
  if((chosenstate %in% statedata$State)==FALSE){
    stop("Invalid state!")
  }
  else if((chosenoutcome %in% outcomelist)==FALSE){
    stop("Invalid outcome!")
  }
  else {
    statedata <-subset(statedata,statedata$State==chosenstate) ##filters for selected state
  }

  ## selecting the right outcome and picking the best hospital with respect to state
  if(chosenoutcome =="heart attack"){
    statedata <-statedata[order(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,statedata$Hospital.Name),]
    head(statedata[1,1])
  }
  else if(chosenoutcome =="heart failure"){
    statedata <-statedata[order(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,statedata$Hospital.Name),]
    head(statedata[1,1])
  }
  else {
    statedata <- statedata[order(statedata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,statedata$Hospital.Name),]
    head(statedata[1,1])
  }
}

#### test code

best("AK","heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

