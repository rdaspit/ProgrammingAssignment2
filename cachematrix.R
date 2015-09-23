
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## cacheSolve:: continued: If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix:: Function Description
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  
      get <- function() x
      seti <- function(inverse) inv <<- inverse 
      geti <- function() inv
      list(set=set, get=get, seti=seti, geti=geti)
}


## cacheSolve:: Function Description
## return the inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  
      inv <- x$geti()
  
      if (!is.null(inv)){
      message("getting cached data")
      return(inv)
  }
  
      mat.data <- x$get()
      inv <- solve(mat.data, ...)
  
      x$seti(inv)
  
      return(inv)
}
