## Series of functions that support the creation and caching of the 
## inverse of a matrix to avoid costly computation from repeated execution
## of caching operations

## Function to create a special case structure with embedded
## get/set functions to store a matrix and cache a matrix inverse output
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function to query the inverse cache for a special case matrix.
## If an inverse result is already cached, print 'getting cached data'
## and return the inverse, else use the cache setter function to calculate,
## set and return the inverse result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
