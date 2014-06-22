## The pair of functions here adds custom functions to matrix object so that its inverse (if exist) will be stored in
## Global Environment for retrival and thus saving the same matrix multiple times.

## makeCacheMatrix is similar to a constructor function creating a custom matrix object from regular R matrix object.
## get and set functions are defined for a matrix object and its inverse, and all the variables stored will be
## accessible to the sister function cacheSolve defined below through lexical scoping

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a custom Matrix object constructed through makeCacheMatrix and returns its inverse, assuming
## the matrix object is reversible. The returned inverse can either be accessed via getinverse function of 
##makeCacheMatrix, or computed with solve() if the inverse is absent in the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
