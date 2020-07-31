install.packages("stringr")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
require(stringr)
best <- function(state, outcome){
  #Reading outcome data
  hospitalData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  checkHospitalData(hospitalData, state, outcome)
  #Finding the best hospital in the state based on their 30-day deaths
  stateHospitals <- subset(hospitalData,hospitalData$State == state)
  outcomeColumn <- createColumnName(outcome)
  validStateHospitalsWithOutcome <- prepareDataForAnalysis(stateHospitals, outcomeColumn)
  #Finding the minimum value for Deaths by outcome
  validStateHospitalsWithOutcome[,2] <- as.numeric(validStateHospitalsWithOutcome[,2])
  minimumDeathsByOutcome <- min(validStateHospitalsWithOutcome[,2])
  bestHospitals <- validStateHospitalsWithOutcome[validStateHospitalsWithOutcome[,2]==minimumDeathsByOutcome]
  #Handling Ties
  finalBestHospitals <- as.data.frame(matrix(bestHospitals, nrow = length(bestHospitals)/2))
  colnames(finalBestHospitals) <- c("Hospital", paste("Deaths by", outcome))
  finalBestHospitalsSortedByName <- finalBestHospitals[order(finalBestHospitals[,1]),]
  finalBestHospitalsSortedByName[1,1]
}
createColumnName <- function(outcome){
  #This function creates the column name using the outcome
  if(outcome=="pneumonia"){
    outcomeColumn <- paste(sep=".", "Hospital.30.Day.Death..Mortality..Rates.from",str_to_title(outcome) )
  }
  else{
    outcomeWords <- strsplit(outcome, " ")
    outcomeWords <- sapply(outcomeWords, str_to_title)
    outcomeColumn <- "Hospital.30.Day.Death..Mortality..Rates.from"
    for(word in outcomeWords){
      outcomeColumn <- paste(sep = ".",outcomeColumn, word)
    }
    outcomeColumn
  }
}
checkHospitalData <- function(hospitalData, state, outcome){
  #This function checks whether the state and the outcome are valid or not
  if(!(state %in% unique(hospitalData$State))){
    stop("invalid state")
  }
  else if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
    stop("invalid outcome")
  }
}
prepareDataForAnalysis <- function(stateHospitals, outcomeColumn){
  stateHospitalsWithOutcome <- cbind(Hospital.Name=stateHospitals[,"Hospital.Name"], "Deaths by outcome" = stateHospitals[,outcomeColumn])
  stateHospitalsWithOutcome[,2][stateHospitalsWithOutcome[,2]=="Not Available"] <- NA
  na.omit(stateHospitalsWithOutcome)
}
best("AL","heart attack")
best("NY","heart attack")
best("MD", "heart attack")
best("NY", "pneumonia")
best("SC", "heart attack")
