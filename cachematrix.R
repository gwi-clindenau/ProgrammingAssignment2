## 2 functions to calculate the inverse of a matrix and retrieve it from the cache if calculated before
## instead of compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve(x) #calculate inverse and store in setInverse environment
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##This function computes the inverse of the "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) { #check if inv can be retrieved from cache
    message("getting cached data")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr,...)
  x$setInverse(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
