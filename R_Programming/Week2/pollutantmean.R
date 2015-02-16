pollutantmean <- function(
  directory, 
  pollutant, 
  id = 1:332, 
  verbose = FALSE,
  raw.vector = FALSE
) {
  
  ## Return the mean of the pollutant across all monitors listed
  ## in the 'id' vector (ignoring NA values)
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  # 'verbose' is a logical; set to TRUE to print verbose messages
  
  # set 'raw.vector' to TRUE to return the RAW value vector as well
  # as the mean value
  
  
  
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
  
  # check for problems
  if ( class(pollutant) != "character" ) {
    # restore original directory
    setwd(wd)
    stop("pollutant must be a character string!")
  }
  
  if ( pollutant != "sulfate" && pollutant != "nitrate" ) {
    # restore original directory
    setwd(wd)
    stop("pollutant must be either sulfate or nitrate!")
  }
  
  if ( verbose ) {
    print(paste(sep = "", "pollutant: ",pollutant))
  }
  
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
  
  # try to set working directory
  setwd(directory)
  
  # iterate through id vector
  i <- 0
  aggregate.vector <- NULL
  
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
    
    # get raw vector for this pollutant
    the.raw.vector <- csv[[pollutant]]
    
    # get vector sans NA value
    cleaned <- the.raw.vector[!is.na(the.raw.vector)]
    
    # if this is the first time we have clean values, initialize the 
    # aggregate vector; otherwise, concatenate this iteration's value
    # to the aggregate vector.
    if ( i == 1 ) {
      aggregate.vector <- cleaned
    }
    else {
      aggregate.vector <- c(aggregate.vector,cleaned)
    }
    if ( verbose ) {
      print(paste("Now at ",length(aggregate.vector)," values.", sep = ""))
    }
  }
  
  # compute the mean
  mean.value <- mean(aggregate.vector)
  
  # restore original directory
  setwd(wd)
  
  # if raw.vector is TRUE, return both the mean and the raw vector,
  # otherwise just the mean.
  
  if ( raw.vector ) {
    return (list(mean = mean.value, values = the.raw.vector))
  }
  else {
    return (mean.value)
  }
  
}

