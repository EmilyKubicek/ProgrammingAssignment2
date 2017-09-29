## A function that takes a square matrix and returns a list of functions
# (set/get matrix, set/get inv) for cacheSolve to use.
makeCacheMatrix <- function(x = matrix()) {
     
     # creating the value of inv    
     inv <- NULL
     
     # set the value of the matrix in this environment
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     # get the value of the matrix
     get <- function() x
     
     # set th value of the inverse
     setinv <- function(inverse) inv <<- inverse 
     
     # get the value of the inverse
     getinv <- function() inv
     
     # create a list of established functions
     list(set = set, get = get,
          setinv = setinv, getinv = getinv)
}

## Calculate the inverse of the original matrix created in makeCacheMatrix
cacheSolve <- function(x=matrix, ...) {
     # inv is equal to the inverse of the matrix
     inv <- x$getinv()
     
     # if the inverse has already been calculated retrieve it from the "cache"
     if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     
     # if the mean cannot be found in the cache, run the calculation again
     data <- x$get()
     inv <- solve(mat.data, ...)
     
     # sets the value of the inverse in the cache via the setinv function.
     x$setinv(inv)
     inv
}