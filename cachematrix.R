## makeCacheMatrix is a function that cache a Matrix and it's inverse
## cacheSolve computes matrix's inverse and use makeCacheMatrix to cache it

## cache the matrix and it's inverse
## then provide get/set

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## get X's inverse from cache or calculate it then put into cache
## while X is makeCacheMatrix's return value

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(x)
  x$setInverse(i)
  i
}
