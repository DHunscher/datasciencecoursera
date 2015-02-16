source("pollutantmean.R")
source("complete.R")

corr <- function(directory, threshold = 0, verbose = FALSE) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        # 'verbose' is a logical; set to TRUE to print verbose messages
        
        ## Return a numeric vector of correlations
        
        ## save working directory
        wd = getwd()
        
        ##################################################################
        ################# validate arguments to function #################
        ##################################################################
        
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
                cat(sep = "", "directory: ",directory,"\n")
        }
        
        
        # check for problems
        if ( (class(threshold) != "integer" && class(threshold) != "numeric")
             || threshold < 0 ) {
                # restore original directory
                setwd(wd)
                stop("threshold must be a non-negative integer or numeric!")
        }
        
        if ( verbose ) {
                cat(sep = "", "threshold: ",threshold,"\n")
        }
        
        ##################################################################
        ############### end validation of function arguments #############
        ##################################################################
        
        
        ##################################################################        
        ###################### helper functions ##########################
        ##################################################################
        
        ########################### intx #################################
        
        intx <- function(s) {
                ## given a string, convert the first three characters
                ## to an integer and return it
                
                as.integer(substr(s,1,3))
        }
        
        ####################### get.valid.ids ############################
        
        get.valid.ids <- function(directory, the.files) {
                
                ## return a vector of integer ids of observation files
                ## that contain sufficient number of complete observations,
                ## i.e., in which both the sulfate and nitrate values are
                ## valid numerics and not NA.
                
                # files are assumed to be of the form 'nnn.csv' where nnn  
                # can be converted to an integer in the range 0:999. nnn is the 
                # identifier of the observation station.
                
                # get list of files
                the.files <- list.files(directory)
                
                if ( length(the.files) == 0 ) {
                        setwd(wd)
                        stop(paste(sep = "", "Could not get list of files from ",
                                   directory,"!"))
                }
                
                if ( verbose ) {
                        cat(sep = "", "Obtained list of ", length(the.files)
                            , " files from ",
                            directory,".\n")
                }
                
                ids <- sapply(the.files, intx, simplify = TRUE,
                              USE.NAMES = FALSE)
                
                # use 'complete' to obtain a data frame containing the number
                # of valid observations (i.e. where neither sulfate nor nitrate 
                # were NA) in each file.
                
                all.obs <- complete(directory = directory, id = ids)
                
                # get a vector of the number of complete observations 
                # at each station
                
                all.nobs <- all.obs$nobs
                
                # get a logical vector with TRUEs in the ordinal position of the ids of 
                # files containing a number of observations greater than the threshold
                
                valid.obs <- all.nobs > threshold
                
                # filter the list of ids using the logical vector
                
                valid.ids <- all.obs$id[valid.obs]
                
                if ( verbose ) {
                        cat(sep = "", "Reduced to a list of ", length(valid.ids)
                            , " valid station IDs.\n")
                }
                
                return( valid.ids )
        }
        
        ####################### get.value.vectors ##########################
        
        get.value.vectors <- function(directory,id) {
                ## get a pair of named vectors, one for each 
                #  of the two pollutants, in which the nth row of each
                #  corresponds to the nth row of the other and both
                ## are valid numerics.
                
                # get csv
                
                #compose a file name
                fn <- paste(id, ".csv", sep = "")
                
                # add leading zeros if necessary 
                while(nchar(fn) < 7) {
                        fn <- paste("0", fn, sep="")
                }
                
                fn <- paste(directory,"/",fn,sep = "")
                
                csv <- read.csv(fn)
                
                # get sulfate values
                
                sulfates <- csv$sulfate
                
                # get logical vector showing which values are non-NA
                
                non.na.sulfates <- !is.na(sulfates)
                
                # get nitrate values
                
                nitrates <- csv$nitrate
                
                # get logical vector showing which values are non-NA
                
                non.na.nitrates <- !is.na(nitrates)
                
                # reduce to a logical vector where both values are
                # non-NA
                
                valid.pairs <- non.na.sulfates & non.na.nitrates
                
                # reduce the pollutant vectors to valid pairs
                
                s <- sulfates[valid.pairs]
                n <- nitrates[valid.pairs]
                
                return( list(sulfates = s, nitrates = n))
        }
        
        ##################################################################
        ##################### begin function main line ###################
        ##################################################################
        
        the.valid.ids <- get.valid.ids(directory, the.files)
        
        # initialize the return vector
        
        correlation.values <- vector(length = 0, mode = "integer")
        
        # cycle through the list of valid ids
        
        for (id in the.valid.ids) {
                if ( verbose ) {
                        cat("Processing id ",id,".\n", sep = "")
                }
                
                # get value vectors using helper function
                
                ret <- get.value.vectors(directory,id)
                
                sulfates <- ret$sulfates
                nitrates <- ret$nitrates
                
                # get correlation value
                this.corr.value <- cor(sulfates, nitrates)
                
                # add to return vector
                correlation.values <- c(correlation.values, this.corr.value)
        }
        
        setwd(wd)
        
        return( correlation.values )
}

