## Matrix inversion is usually a costly computation and thus it is
## better to caching the inverse of a matrix rather than computing it 
## again. This is what the two function do.

## makeCacheMatrix.R creates a special "matrix" object that can 
## cache its inverse, whereby in real a list is created containing a
## function to:
## 1.set the the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      invmatr <- NULL
      set <- function(y) {
            x <<- y
            invmatr <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) invmatr <<- solve
      getinverse <- function() invmatr
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve.R computes the inverse of the special "matrix" 
## returned by makeCacheMatrix.R. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmatr <- x$getinverse()
      if(!is.null(invmatr)) {
            message("getting cached data")
            return(invmatr)
      }
      data <- x$get()
      invmatr <- solve(data, ...)
      x$setinverse(invmatr)
      invmatr
}
