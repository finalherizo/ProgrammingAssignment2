## The function makeCacheMatrix allows to cache a matrix representation and its 
## inverse after it has been computed.
## The function cacheSolve uses makeCacheMatrix object to use cached data if
## it exists or to compute one and cache it after.

## makeCacheMatrix creates an object that maintains a matrix and its inverse.
## The function allows to save the inverse of the matrix using an internal state.
## 
## makeCacheMatrix returns a list of functions allowing to change its internal
## state
## $set allows to update the internal matrix and in the same time clears the
##    cache
## $get allows to get the stored matrix
## $setinverse allows to save the inverse of the matrix by caching it.
## $getinverse allows to retrieve the value of the internal cache.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
  
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
  
        get <- function() x
  
        setinverse <- function(inverse) {
              i <<- inverse
        }
  
        getinverse <- function() i
  
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve takes a makecacheMatrix object and tries to use and return
## a previously cached value if it exists. A calculation is performed otherwise 
## and then the new result is cached and returned.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
  
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
  
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
  
        i
}
