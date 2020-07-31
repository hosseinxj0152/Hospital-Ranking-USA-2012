install.packages("stringr")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
require(stringr)
rankall <- function(outcome, num = "best"){
  #Reading outcome data
  hospitalData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  checkHospitalData(hospitalData, outcome) 
  #Finding hospitals with the given rank in each state
  states <- unique(hospitalData$State)
  states <- str_sort(states)
  hospital <- sapply(states, rankhospital,outcome=outcome,num=num,hospitalData=hospitalData)
  hospital <- as.data.frame(hospital)
  cbind(hospital, states)
  }
rankhospital <- function(state, outcome, num="best", hospitalData){
  #Ranking hospitals in a given state based on the number of Deaths by an outcome
  stateHospitals <- subset(hospitalData, hospitalData[,7] == state)
  outcomeColumn <- createColumnName(outcome)
  validStateHospitalsWithOutcome <- prepareDataForAnalysis(stateHospitals, outcomeColumn)
  validStateHospitalsWithOutcome <- as.data.frame(validStateHospitalsWithOutcome)
  validStateHospitalsWithOutcome[,2] <- as.numeric(validStateHospitalsWithOutcome[,2])
  sortedHospitalData <- validStateHospitalsWithOutcome[order(validStateHospitalsWithOutcome[,2], validStateHospitalsWithOutcome[,1]),]
  #Assigning a number to the variable num
  num <- assignNumberToNum(num,nrow(sortedHospitalData))
  if(is.na(num)){
    return(NA)
  }
  else{
    sortedHospitalData[,1][num]
  }
}
checkHospitalData <- function(hospitalData, outcome){
  #This function checks whether the outcome is valid or not
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
    stop("invalid outcome")
  }
}
createColumnName <- function(outcome){
  #This function creates the column name using the outcome
  outcomeWords <- strsplit(outcome, " ")
  outcomeWords <- sapply(outcomeWords, str_to_title)
  outcomeColumn <- "Hospital.30.Day.Death..Mortality..Rates.from"
  for(word in outcomeWords){
    outcomeColumn <- paste(sep = ".",outcomeColumn, word)
  }
  outcomeColumn
}
assignNumberToNum <- function(num,numberOfSortedHospitalDataRows){
  if(num=="best"){
    return(1)
  }
  else if(num=="worst"){
    num <- numberOfSortedHospitalDataRows
  }
  else if(num>numberOfSortedHospitalDataRows){
    NA
  }
  else{
    num
  }
}
prepareDataForAnalysis <- function(stateHospitals, outcomeColumn){
  stateHospitalsWithOutcome <- cbind(Hospital.Name=stateHospitals[,"Hospital.Name"], "Deaths by outcome" = stateHospitals[,outcomeColumn])
  stateHospitalsWithOutcome[,2][stateHospitalsWithOutcome[,2]=="Not Available"] <- NA
  na.omit(stateHospitalsWithOutcome)
}
r <- rankall("heart attack", 4)
as.character(subset(r, state == "WV")$hospital)
r <- rankall("pneumonia", "worst")
r <- rankall("heart failure", 10)