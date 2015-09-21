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
  
  ## Parse filenames and read to table
  filenames = sprintf("%s/%03d.csv",directory,id)
  
  dat = lapply(filenames,read.table,sep = ",", header = TRUE, row.names=NULL);
  dat = do.call(rbind,dat); # Combine all tables into one
  
  ## Subset data by "complete" criteria for each id
  ccount = vector("integer",length(id));
  
  completeNitrate = !is.na(dat[,"nitrate"]);
  completeSulfate = !is.na(dat[,"sulfate"]);
  
  for(i in 1:length(ccount))
  {
    ccount[i] = sum(dat[,"ID"] == i & completeNitrate & completeSulfate);
  }
  
  ## Build data frame & name columns
  observationCount = data.frame(id, ccount)
  colnames(observationCount) <- c("id","nobs");
  
  observationCount
}