
best<-function(state,outcome) {
    
    oldw <- getOption("warn")
    options(warn = -1)
    
    outcome_data<-read.csv("outcome-of-care-measures.csv",colClasses = 'character')
    newdata<-outcome_data[,c(2,7,11,17,23)]
    
    names(newdata)<-c("Hospital","State","heart attack","heart failure","pneumonia")
    
    if(!(state %in% newdata[,2]))stop("invalid state")
    
    if(!(outcome %in% names(newdata))) stop("invalid outcome")
    
    selectedrows<-(newdata[,2]==state)
    
    Final_State_Data<-newdata[selectedrows,]
    
    good<-is.na(as.numeric(Final_State_Data[,outcome]))
    
    Final_df<-Final_State_Data[!good,c("Hospital",outcome)]
    
    Final_df<-Final_df[order(as.numeric(Final_df[,outcome]),Final_df[,"Hospital"]),]
    
    options(warn = oldw)
    
    Final_df[which.min(as.numeric(Final_df[,outcome])),"Hospital"]
    
    
    
}