##The following functions serve to cache the inverse of an array.

## makeCacheMatrix: this function creates a special "matrix" object
## that can cache its inverse

## set the value of the vector
## get the value of the vector
## set the value of the reverse
## get the value of the reverse

makeCacheMatrix <- function(x = matrix()) {
  
  MatrixInverse <- NULL
  set <- function(y) {
    x <<- y
    MatrixInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) MatrixInverse <<- inv
  getInverse <- function() MatrixInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve: this function calculates the inverse of the special "matrix" returned
##by makeCacheMatrix above.If the inverse has already been calculated (and the array 
##has not changed), then the cache solution should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  MatrixInverse <- x$getInverse()
  if(!is.null(MatrixInverse)) {
    message("getting cached data")
    return(MatrixInverse)
  }
  data <- x$get()
  MatrixInverse <- solve(data, ...)
  x$setInverse(MatrixInverse)
  MatrixInverse
  
}
