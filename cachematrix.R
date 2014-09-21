## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The puspose of the code is to store a martix and a cached value of the inverse of the 
# matrix.

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
        cacheInverse <- function(solve) cache <<- solve

        # get the cached value
        getInverse <- function() cache
        
        # return a list. Each named element of the list is a function
        list(setMat = setMat, getMat = getMat, cacheInverse = cacheInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## The following function calculates the inverse  of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         # get the cached value
        inverse <- y$getInverse()
        # if a cached value exists return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # return the inverse
        inverse
}
