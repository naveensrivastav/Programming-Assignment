rankhospital <- function(state, outcome, num = "best")
{
  
  my_data <-read.csv("C:/Users/Admin/Documents/Coursera_Assignment/outcome-of-care-measures.csv", header = TRUE, sep = ",")
  if(!any(state==my_data$State) )
    stop("invalid state")
  else 
  {
   
    i<- col_num(outcome)
    if(i == 0)  
      stop("invalid outcome")  
    else {
      
      
      ##splitiing data frame for the specified state  with hospitale name and death rate 
      temp <- my_data[ my_data$State == state , c(2,i)]
      
      ## removing "NA from the data frame
      temp <- temp[!is.na(temp[,2]), ]
      
      temp[,1] <- as.character(temp [,1])
      temp[,2] <- as.numeric(temp[,2])
      
    host_temp <- tapply(temp[,1], temp[,2], FUN = sort)
    
    ## l <- length(host_temp)
    hlist<- as.character()
        l<- 0
      
        
        
        
          for(j in 1: length(host_temp))
       {
       if(length(host_temp[[j]]==1))
       {
         l=l+1
         hlist[l] <- host_temp[[j]] 
         
       }
       else{
           t <- host_temp[[j]]
              ## for(count in 1 : length(t))
              ## {hlist[l]<- t[[count]]     l=l+1 }
           hlist[l+1: l+length(t)] <- t  
           l <- l+length(t) 
          }
              if(l>= num)
              break()
            
        }
     
    
    if( num == "worst")
     {
      num <- as.numeric(length(hlist))
      hlist[num]
    }
    else 
      if(num =="best")
       { num<- as.numeric(1)
         hlist[num]
      }
       
      else
     {
       
      num<- as.numeric(num)
     if(num > length(hlist) )
       message(" Rank exceeded the  last...")
    else 
      hlist[num]
     } 
    } 
  }
}





col_num<- function (outcome)
{
  if(outcome == "heart attack")
    return(11)
  else
  {
    if(outcome == "heart failure")
      return(17)
    else 
    {
      if( outcome == "pneumonia")
        return(23)
      else 
        return(0)
    }
  }
  
}


