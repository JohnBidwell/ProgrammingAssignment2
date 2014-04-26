## These two functions take advantage of R scoping rules to cache the value of an inverted
## matrix and to check for and return it as an alternative to re-calculating it


## Takes an invertible matrix and returns a list of four functions for managing the 
## caching of the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {

        s <- NULL                        # Initialise s

        set <- function(y) {             # set (of x) takes input y 
                x <<- y                  # pushes the y value to x in the cache
                s <<- NULL               # and resets s in the cache
        }

        get <- function() x              # get (of x) gets x from the cache

        ## setinv (of x) pushes the supplied value solve to s in the cache

        setinv <- function(solve) s <<- solve
        getinv <- function() s          # getinv (of x) gets s from the cache 
        
        ## bundle the functions into a list

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve checks the cache for a solved (inverted) copy of matrix x and, if not 
## found, calculates it and adds it to the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        s <- x$getinv()                      # check the cache

        if(!is.null(s)) {                    # if not null
                message("getting cached data")
                return(s)                    # stop here and return the
        }                                    # discovered value
                                             #    otherwise...
        data <- x$get()                      # read x into data
        s <- solve(data, ...)                # make s the inverse of data
        x$setinv(s)                          # push s to the cache
        s                                    #     ...function returns the
}                                            # value of the last expression
