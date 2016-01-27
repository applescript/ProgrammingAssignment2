# makeCacheMatrix allows a matrix and its inverse to be set/cached
# cacheSolve checks if the inverse of a matrix is cached and proceeds to calculate the inverse should a cached version be not found

#this function allows the inverse of a matrix to be calculated and stored in cache
makeCacheMatrix <- function(x = matrix()) {
  #by default the inverse is not calculated
  inv <- NULL
  
  #seter and getter functions for matrix and its inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# this function checks if the inverse of a matrix is cached and proceeds to calculate the inverse should a cached version be not found
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
