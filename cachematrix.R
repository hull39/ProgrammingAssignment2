##
## R programming assignment 2 -- Lexical Scoping
##    author:  Jonathan Hull
##    date:  Oct. 14, 2014
##
## Overall description:  Cache potentially time consuming computations.  This is done with the the <<- operator 
##    that assigns values to objects in an environment that's different from the current one.
##
## Implementation -- Two functions are provided that create a "matrix" object (makeCacheMatrix) that includes the data for a normal
##    R matrix and methods that calculate the inverse and retrieve it.  Another function (cacheSolve) is given
##    one of our matrix objects and returns the cached value, if it exists, or calculates the inverse and
##    stores it for future reference.

##
## makeCacheMatrix(x:matrix) -- create a "matrix" object that includes the orignal data for x and the following methods:
##    x$set(y) -- set x in the sub envirnoment to y and m to NULL
##    x$get()  -- return the value of the original data
##    x$setinverse(solve)  -- set m in the sub environment to the inverse of the original data
##    x$getinverse() -- return the inverse of the original data
##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 
## cacheSolve(x) -- where is a "matrix" object of our new "matrix" type
##    It calls the methods of our object so that the cached version of the inverse is returned, if it
##    exists, otherwise it creates the inverse and stores it in the sub environment.
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
