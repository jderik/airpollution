corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations

  ##return all files
  SpecFiles<-list.files(directory)
  df<-data.frame()

  if(length(SpecFiles)>0)
  {    
      
    ##Data Frame that will hold all the complete case numbers for each file
    df<-complete(directory,1:length(SpecFiles))
    ##Data Frame that will read ids from df that have compelte cases more than threshold
    thresholdcaseidno<-df[df$nobs>threshold,1] 
      ##initialise  a vector for correlations holding, Fill with NA
      corrdata<-vector(mode="numeric", length=0)
                      #rep(NA, length(thresholdcaseidno))
    for(i in thresholdcaseidno)
    {
      ##Read full file corresponding to each matching thresold case id
      readfile<- read.csv(paste(directory,"/",SpecFiles[i],sep=""))
      ##Take out complete cases from the read file and then pick out the nitrate and sufate readings 
      ##from the complete cases and add to the corrdata vector
      y<-complete.cases(readfile)
      sulfateread<-readfile[y,2]
      nitrateread<-readfile[y,3]
      #corrdata[i]<-round(cor(sulfateread,nitrateread),5)
      corrdata<-append(corrdata,cor(sulfateread,nitrateread))
    }
      
    corrdata
    ##[complete.cases(corrdata)]
  }
  
  

}