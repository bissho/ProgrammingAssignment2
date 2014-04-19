## Funtions in order to create the cacheMatrix list
## and the method that returns the cached inverse matrix

## Function that create list of matrix object
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Funcion that use the list matrix object to return cached inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
#Testing
#x=makeCacheMatrix(matrix(c(1,0,0,1),2,2))
#cacheSolve(x)
#cacheSolve(x)