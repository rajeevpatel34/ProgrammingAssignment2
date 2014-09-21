## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## Function: makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
## The input should be a matrix
## The return value will be a ([1] vector of a) list containing four functions:
## 1. a 'set' function to set the value of the matrix
## 2. a 'get' function to get the value of the matrix
## 3. a 'setInverse' function to set the value of inverse of the matrix
## 4. a 'getInverse' function to get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## Function: cacheSolve
## The cacheSolve function calculates the mean of the special "vector"
## created with the makeCacheMatrix function.
## However, it first checks to see if the mean has already been calculated.
## If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the mean of the data and sets the value
## of the mean in the cache via the setmean function.
## The input should be a list created from the 'makeCacheMatrix' function
## the output will be a matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  ##check if inverse matric already exists
  if(!is.null(inverseMatrix)) {
    message("getting cached matrix.")
    return(inverseMatrix)
  }
  ##if inverse doesnt exist calculate it and
  ##use the setInverse to store it for later
  matrix <- x$get()
  inverseMatrix <- solve(matrix)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
