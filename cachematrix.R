

## Put comments here that give an overall description of what your
## functions do

## This function creates a list containing a function to 
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
              cacheInv<- NULL
              set <- function(y) {
                      x <<- y
                      cacheInv = NULL
              }
              get <- function() x 
              setInverse <- function(inverse) cacheInv <<- inverse
              getInverse <- function() cacheInv
              list(set = set, get = get, setInverse = setInverse getInverse = getInverse)
}

## Checks if the inverse has already been calculated. If the inverse has already been calculated, it gets the inverse from the cache and skips the computation. If it has not been calculated, it calculates the inverse of the matrix using solve() and sets the value of the mean in the cache via the setInverse function. 

cacheSolve <- function(x, ...) {
        cacheInv <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(cacheInv)
                
        }
        data <- x$get()
        cacheInv <- solve(data, ...)
        x$setInverse(cacheInv)
        cacheInv
}
