pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
 
  SpecFiles<-list.files(directory)
  meanval<-0
  size<-0
  framectr<-0
  j<-0
  z<-0
  for (j in id)
  {
    mydata<-read.csv(paste(directory,"/",SpecFiles[j],sep=""))
    ##x<-is.na(mydata[,pollutant])
    x<-complete.cases(mydata[pollutant])
    size<-size+nrow(mydata[x,])
    framectr<-nrow(mydata[x,])
     if(framectr!=0)
     {
      for(z in 1:framectr)
      {
        meanval<- meanval + (mydata[x,pollutant][z])
    
       }
     }
    
  }
  round(meanval/size,3)
}

