complete <- function(directory, id = 1:332, 
                     verbose = FALSE) {
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## 'verbose' is a logical; set to TRUE to print verbose messages
  
  ###########################################################################
  ############################### globals ###################################
  ###########################################################################
  
  ## save working directory
  wd = getwd()
  
  ###########################################################################
  ################### validate arguments to function ########################
  ###########################################################################
  
  if ( verbose ) {
    print("In verbose mode.")
  }
  
  # check for problems
  if ( class(directory) != "character" ) {
    stop("directory must be a character string!")
  }
  
  if ( !file_test("-d", directory) ) {
    stop("directory argument is not a directory!")
  }
  
  if ( verbose ) {
    print(paste(sep = "", "directory: ",directory))
  }
  
  # try to set working directory
  setwd(directory)
  
  # check for problems
  if ( class(id) != "integer" && class(id) != "numeric") {
    # restore original directory
    setwd(wd)
    stop("id must be an integer or numeric vector!")
  }
  
  if ( min(id) < 1 || max(id) > 332 ) {
    # restore original directory
    setwd(wd)
    stop("id values must be between 1 and 332 inclusive!")
  }
  
  if ( verbose ) {
    print(paste(sep = "", "id: ",id))
  }
  
  ###########################################################################
  ################# end validation of function arguments ####################
  ###########################################################################
  
  ###########################################################################
  ####################### begin function main line ##########################
  ###########################################################################

  # iterate through id vector
  i <- 0
  nobs.frame <- NULL
  names.of.rows <- NULL
  # cycle through vector of integers
  while (i < length(id) ) {
    
    #increment i to make sure loop finishes at some point
    i <- i + 1
    
    # coerce the next id to integer in case it's numeric
    this.id = as.integer(id[i])
    
    if ( verbose ) {
      # print first status message
      print(paste("processing ",this.id,".", sep = ""))
    }
    
    #compose a file name
    fn <- paste(this.id, ".csv", sep = "")
    
    # add leading zeros if necessary 
    while(nchar(fn) < 7) {
      fn <- paste("0", fn, sep="")
    }
    
    # test to see if the file exists; if not, print message and skip
    # to next integer in id vector
    if ( !file_test("-f",fn) ) {
      if ( verbose ) {
        print(paste("File '",fn,"' does not exist in directory ",getwd(),
                    " or is not a file.", sep = ""))
      }
      next
    }
    
    if ( verbose ) {
      print(paste(sep = "","filename is ",fn))
    }
    
    # read the csv file into a data frame
    csv <- read.csv(fn)

    sulfate.observations <- csv[["sulfate"]]
    nitrate.observations <- csv[["nitrate"]]
    
    j.nobs <- 0
    for ( j in 1:length(sulfate.observations)) {
      # iff both observations are not NA, increment the number of complete
      # observations
      if ( !is.na(sulfate.observations[j]) && !is.na(nitrate.observations[j]) ) {
        j.nobs <- j.nobs + 1
      }
    }
    
    # add i to list of row names
    names.of.rows <- c(names.of.rows,i)
    
    # add row to the data.frame

    nobs.frame <- rbind(nobs.frame,data.frame(id = this.id, nobs = j.nobs))
  }
  # set row names
  # row.names(nobs.frame) <- names.of.rows
  
  # restore original working directory
  setwd(wd)  
  
  # return data.frame
  return(nobs.frame)
}