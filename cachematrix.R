## cachematrix.R is a R program which has two functions makeCacheMatrix and cacheSolve of which makeCacheMatrix is used to cache the inverse matrix.
## cacheSolve sets the inverse of the matrix object returned by the makeCacheMatrix function. If the inverse has already been calculated (and the matrix has not ## changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix creates an object of the matrix x and will cache the inverse of matrix x
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse  <<- NULL
        }
        get <- function() x
        setInverse <- function(inversed) inverse  <<- inversed
        getInverse  <- function() inverse 
        list(set = set, get = get,
             setInverse  = setInverse ,
             getInverse  = getInverse )
}


## cacheSolve returns the inverse of the already cached matrix 

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached inverse matrix")
                return(inverse)
        }
        matrixData <- x$get()
        inverse <- solve(matrixData, ...)
        x$setInverse(inverse)
        inverse
      ## Return a matrix that is the inverse of 'x'
}


