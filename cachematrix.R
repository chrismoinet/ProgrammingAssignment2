######################################################################################################
## This program computes the inverse of a matrix in an optimized way                                ##
######################################################################################################
## for a big matrix, if it has to be computed repeatedly (e.g. in a loop)                           ##
## it may take too long to compute the inverse                                                      ##
## If the contents of a matrix are not changing, it may make sense to cache the value of the inverse## 
## so that when we need it again, it can be looked up in the cache rather than recomputed           ##
######################################################################################################

## makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {         
        i <- NULL
        # initialisation of set, get, setsolve, getsolve as functions
        ## assignment <<- causes a search to be made through parent environments 
        ##     for an existing definition of the variable being assigned
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        ## creates a special list of functions
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
	i <- x$getsolve()
	## it first checks to see if the inverse has already been calculated.
	if(!is.null(i)) {
			## If so, it gets the inverse from the cache and skips the computation
            message("getting cached data")
            return(i) 
	}
	## Otherwise, it calculates the inverse of the data 
	## and sets the value of the inverse in the cache using the setsolve function.
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i     
}
