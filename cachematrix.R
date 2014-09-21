## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 # holds the cached value or NULL if nothing is cached
        # initially nothing is cached so set it to NULL
        cache <- NULL
        
        # store a matrix
        setMat <- function(newValue) {
                x <<- newValue
                # since the matrix is assigned a new value, flush the cache
                cache <<- NULL
        }

        # returns the stored matrix
        getMat <- function() x

        # cache the given argument 
        cacheInv <- function(solve) cache <<- solve

        # get the cached value
        getInv <- function() cache
        
        # return a list. Each named element of the list is a function
        list(setMat = setMat, getMat = getMat, cacheInv = cacheInv, getInv = getInv)
}


## Write a short comment describing this function

## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         # get the cached value
        inverse <- y$getInv()
        # if a cached value exists return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        data <- y$getMat()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # return the inverse
        inverse
}
