## The basic objective is to construct a function that creates a cache of the inverse matrix 
##and another function that checks fot the cahced values and calculates the required values accordingly

## Creates a list of functions to set,accquire the desired values and cache them

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Checks if the values have been cached, if not it calculates it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i ## Return a matrix that is the inverse of 'x'
}

