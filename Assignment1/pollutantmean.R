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
  ## NOTE: Do not round the result!
  
  ## Parse filenames and read to table
  filenames = sprintf("%s/%03d.csv",directory,id)
  
  dat = lapply(filenames,read.table,sep = ",", header = TRUE, row.names=NULL);
  
  dat = do.call(rbind,dat); # Combine all tables into one
  
  ## Subset data by specified pollutant
  subset <- dat[,pollutant];
  
  # Idenify non-missing data & return mean
  missing <- is.na(subset); 
  mean(subset[!missing]); # Return non-missing data
}