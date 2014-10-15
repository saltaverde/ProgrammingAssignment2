## The functions in this .R file solve and cache an invertible matrix

## makeCacheMatrix() constructs several other functions:
## set() superassigns the value of the matrix for use in cacheSolve()
## get() returns the input matrix
## setinverse() superassigns the value of the inverted matrix for use in cacheSolve() 
## getinverse() computes and returns the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() takes a matrix x and first looks for a cached version of its inverse, then solves for the inverse of x
## if no cached inverse exists.  In either case, the inverse of x is returned.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached matrix. . .")
            return(i)
            }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
      }

