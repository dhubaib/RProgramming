corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  ## Build table with # of complete records
  observationCount = complete(directory);
  
  ## Index observations above threshold
  aboveThreshold = observationCount[,"nobs"] > threshold;
  
  ## Subset data by "complete" criteria for each id
  cvector = vector("numeric",sum(aboveThreshold));  
  id = observationCount[aboveThreshold,"id"]; # Grab corresponding ids
  
  if(length(id) > 0) # Only if matches to criteria return 0
  {
    ## Read in data from relevant files
    filenames = sprintf("%s/%03d.csv",directory,id)
    dat = lapply(filenames,read.table,sep = ",", header = TRUE, row.names=NULL);
    dat = do.call(rbind,dat); # Combine all tables into one
  
    for(i in 1:length(cvector))
    {
      # Subset by complete entries matching id
      subs = subset(dat, ID == id[i] & !is.na(nitrate) & !is.na(sulfate));
      cvector[i] = cor(subs[,"nitrate"],subs[,"sulfate"])
    }
  }
  
  cvector
}