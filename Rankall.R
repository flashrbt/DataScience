rankall<-function(outcome,num="best") {
    
    outcome_data<-read.csv("outcome-of-care-measures.csv",colClasses = 'character')
    
    newdata<-outcome_data[,c(2,7,11,17,23)]
    
    states_data<-unique(outcome_data[,7])
    
    names(newdata)<-c("Hospital","State","heart attack","heart failure","pneumonia")
    
    #if(!(state %in% newdata[,2]))stop("invalid state")
    
    if(!(outcome %in% names(newdata))) stop("invalid outcome")
    
    vector <- data.frame()
    
    for (state in states_data)
    {
   
        selectedrows<-(newdata[,2]==state)
        
        Final_State_Data<-newdata[selectedrows,]
        
        good_rows<-is.na(as.numeric(Final_State_Data[,outcome]))
        
        Final_df<-Final_State_Data[!good_rows,c("Hospital","State",outcome)]
        
        final_outcome<-as.numeric(Final_df[,outcome])
        
    
        if(num=="best"){
            
            result<-Final_df[which.min(as.numeric(Final_df[,outcome])),c(1,2)]
            
        }
        else if(num=="worst"){
            
            result<-Final_df[which.max(as.numeric(Final_df[,outcome])),c(1,2)]
        }
        
        else if(num>nrow(Final_df))   {
            
            result<-c("<NA>",state)
        } 
        else {
            Final_df<-Final_df[order(as.numeric(Final_df[,outcome]),Final_df[,"Hospital"]),]
            
            result<-Final_df[num,c(1,2)]
        }
        
        
        vector<-rbind(vector,result)
    
    
    }
  
    vector<-vector[order(vector[,2]),]
    vector
}

rankall2<-function(outcome,num="best") {
    
    outcome_data<-read.csv("outcome-of-care-measures.csv",colClasses = 'character')
    
    newdata<-outcome_data[,c(2,7,11,17,23)]
    
    states_data<-unique(outcome_data[,7])
    
    names(newdata)<-c("Hospital","State","heart attack","heart failure","pneumonia")
    
    #if(!(state %in% newdata[,2]))stop("invalid state")
    
    if(!(outcome %in% names(newdata))) stop("invalid outcome")
    
    vector <- data.frame()
    
    for (state in states_data)
    {
        
        selectedrows<-(newdata[,2]==state)
        
        Final_State_Data<-newdata[selectedrows,]
        
        good_rows<-is.na(as.numeric(Final_State_Data[,outcome]))
        
        Final_df<-Final_State_Data[!good_rows,c("Hospital","State",outcome)]
        
        final_outcome<-as.numeric(Final_df[,outcome])
        
        Final_df<-Final_df[order(final_outcome,Final_df[,"Hospital"]),]
        
        rownum<-nrow(Final_df)
        
        if(num=="best"){
            
            result<-Final_df[1,c(1,2)]
            
        }
        else if(num=="worst"){
            
            result<-Final_df[rownum,c(1,2)]
        }
        
        else if(num>rownum)   {
            
            result<-c("<NA>",state)
        } 
        else {
           
            result<-Final_df[num,c(1,2)]
        }
        
        
        vector<-rbind(vector,result)
        
        
    }
    
    vector<-vector[order(vector[,2]),]
    vector
}