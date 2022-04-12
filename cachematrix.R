## Create 2 functions: 
#1st function: creates matrix object that can cache its inverse 
#2nd function: computes inverse of matrix object from 1st function (called makeCacheMatrix) and returns the inverse from the cache if previously calculated
 

## makeCacheMatrix: creates matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}


## Computes inverse of matrix object from 1st function and returns the inverse from the cache if previously calculated


cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
          }

          data <- x$get()
          m <- solve(data, ...) #use solve function to compute inverse of square matrix
          x$set_inverse(m)

        ## Return a matrix that is the inverse of 'x'
        m
}


