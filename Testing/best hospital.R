outcome<-read.csv("C:/Users/patzerl/Documents/Coursera/R Homework 3/outcome-of-care-measures.csv", colClasses="character", header=T)

outcome[,11]<-as.numeric(outcome[,11])
outcome[,17]<-as.numeric(outcome[,17])
outcome[,23]<-as.numeric(outcome[,23])
##hist(outcome[,11])

outcome.data<-outcome  ## changed name of data set because of naming problems


best<-function(state, outcome){
  outcomes = c("heart attack", "heart failure", "pneumonia")
  if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
  states <- outcome.data[,7]
  states <- unique(states)
  if( state %in% states == FALSE ) stop("invalid state")
  best.hospital2<-c()
  if(outcome=='heart attack'){
    subset<-na.omit(outcome.data[outcome.data[,7]==state,])
    where.small3<-which.min(subset[,11])
    best.hospital2<-subset[where.small3, 2]
    return(best.hospital2)}
  if(outcome=='heart failure'){
    subset<-na.omit(outcome.data[outcome.data[,7]==state,])
    where.small3<-which.min(subset[,17])
    best.hospital2<-subset[where.small3, 2]
    return(best.hospital2)}
  if(outcome=='pneumonia'){
    subset<-na.omit(outcome.data[outcome.data[,7]==state,])
    where.small3<-which.min(subset[,23])
    best.hospital2<-subset[where.small3, 2]
    return(best.hospital2)}
}

##best("TX", "heart failure")  tested


###################   PART 2 ANSWERS


data <- read.csv("C:/Users/patzerl/Documents/Coursera/R Homework 3/outcome-of-care-measures.csv", colClasses="character", header=T)

rankhospital <- function(state, outcome, num) {
  ## Read outcome data
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  ## Validate the outcome string
  outcomes = c("heart attack", "heart failure", "pneumonia")
  if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
  
  ## Validate the state string
  states <- data[, 2]
  states <- unique(states)
  if( state %in% states == FALSE ) stop("invalid state")
  
  ## Validate the num value
  if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
  
  ## Grab only rows with our state value    
  data <- data[data$state==state & data[outcome] != 'Not Available', ]
  
  ## Order the data
  data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
  data <- data[order(data$name, decreasing = FALSE), ]
  data <- data[order(data[outcome], decreasing = FALSE), ]
  
  ## Process the num argument
  vals <- data[, outcome]
  if( num == "best" ) {
    rowNum <- which.min(vals)
  } else if( num == "worst" ) {
    rowNum <- which.max(vals)
  } else {
    rowNum <- num
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  data[rowNum, ]$name
}

###rankhospital("MD", "heart attack", 2131)  testing

################## PART 3

data <- read.csv("C:/Users/patzerl/Documents/Coursera/R Homework 3/outcome-of-care-measures.csv", colClasses="character", header=T)

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ##data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  ## Validate the outcome string
  outcomes = c("heart attack", "heart failure", "pneumonia")
  if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
  
  ## Validate the num value
  if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
  
  ## Grab only rows with data in our outcome
  data <- data[data[outcome] != 'Not Available', ]
  
  ## Order the data
  data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
  data <- data[order(data$name, decreasing = FALSE), ]
  data <- data[order(data[outcome], decreasing = FALSE), ]
  
  ## Helper functiont to process the num argument
  getHospByRank <- function(df, s, n) {
    df <- df[df$state==s, ]
    vals <- df[, outcome]
    if( n == "best" ) {
      rowNum <- which.min(vals)
    } else if( n == "worst" ) {
      rowNum <- which.max(vals)
    } else {
      rowNum <- n
    }
    df[rowNum, ]$name
  }
  
  ## For each state, find the hospital of the given rank
  states <- data[, 2]
  states <- unique(states)
  newdata <- data.frame("hospital"=character(), "state"=character())
  for(st in states) {
    hosp <- getHospByRank(data, st, num)
    newdata <- rbind(newdata, data.frame(hospital=hosp, state=st))
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  newdata <- newdata[order(newdata['state'], decreasing = FALSE), ]
  newdata
}

tail(rankall("pneumonia", "worst"), 3)