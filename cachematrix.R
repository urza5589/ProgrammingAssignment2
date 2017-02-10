## Assignment 2 for coursera which takes in a matrix calculates its inverse and caches it to return if requested again. 

## Takes in a matrix and stores it alone with it's inverse if it has been calculated already

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
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


## Checks for a chached inverse of a given makeCacheMatrix to return else calculates, caches and returns the inverse of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(list(value = i, message =  "getting cached data"))
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
