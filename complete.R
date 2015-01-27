complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  SpecFiles<-list.files(directory)
  df<-data.frame()
  casecount<-0
  rowcounter<-0
  for (j in id)
  {
    x<-read.csv(paste(directory,"/",SpecFiles[j],sep=""))
    y<-complete.cases(x)
    casecount<-nrow(x[y,])
    rowcounter<-rowcounter+1
    df[rowcounter,1]<- j
    df[rowcounter,2]<- casecount
    
  }
  names(df)<-c("id","nobs")
  
  df


}