## Functions implemented for coursera class R Programming by Tim Williams
## on April 25, 2015. These functions demonstrate the ability to cache a result
## so that a calculation (taking the inverse of a matrix) does not need to be
## repeated if the function is called again on the same data.


# Creates a list of four functions to store a matrix (set), return the stored
# matrix (get), store the inverse of the matrix (setInverse), 
# and return that inverse (getInverse).
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  # create a variable to store a cached inverse, initially nonexistant.
    # stores a matrix in x, retaining the value between calls to the function
    set <- function(y) {
        x <<- y
        i <<- NULL  # when a new value is set, the cached inverse is reset
    }
    # returns the stored matrix
    get <- function() x
    # stores a value, presumably the inverse of x, in our cache variable i
    setInverse <- function(inverse) i <<- inverse
    # returns the cached variable i, presumably the inverse of the matrix in x
    getInverse <- function() i
    # return a list of four functions as the return value of makeCacheMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# Given a matrix, use makeCacheMatrix to determine if the inverse of the matrix
# has been calculated already. If it has, return the cached value instead
# of doing a calculation. If not (if the cache is NULL), then calculate the
# inverse and return it (as well as storing it in the cache for future use).
cacheSolve <- function(x, ...) {
    # Retrieve the cached inverse
    i <- x$getInverse()
    # if that cached value is not Null, the inverse has already been calculated
    # so just return the cached value instead of recalculating it
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # Cached value did not exist, so we need to calculate it. 
    data <- x$get()  # retrieve the matrix in question
    # calculate the inverse and then store it in the cache for future use
    i <- solve(data, ...)
    x$setInverse(i)
    # return the calculated inverse matrix
    i
}