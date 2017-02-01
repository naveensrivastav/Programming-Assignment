hospital <- function(state, outcome, num = "best")
{
  mdata <-read.csv("C:/Users/Admin/Documents/Coursera_Assignment/outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors=FALSE)

  if(!any(state==mdata$State) )
    stop("invalid state")
  else 
  {   i<- col_num(outcome)
      if(i == 0)  
      stop("invalid outcome")
      else
      {
        mytemp<-mdata[mdata$State== state,c(2,i)]
        
        mytemp <- mytemp[complete.cases(mytemp), ]
        apply_temp <- tapply(mytemp[,1],mytemp[,2], FUN =  sort)
        p <- 0
        q<- 0
        ##return(apply_temp)
        if (num == "best")
          num<- as.numeric(1)
        if(num=="worst")
          num<- as.numeric(nrow(mytemp))
        
        
        for ( i in 1: length(apply_temp))
        {
          q<- length(mytemp[[i]])
           if( num > p+q)
           {p<- p+q}
          else {
            t<- mytemp[[i]]
            if(num== p+q)
              {
            return(t[q])
              }
            else
              {return(t[num-p])}
            }
         }
      }
   }
}