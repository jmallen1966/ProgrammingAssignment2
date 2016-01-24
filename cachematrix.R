##The first function, `makeCacheMatrix` creates a special "maxtrix", which is
##really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse of the matrix
##4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
              x <<- y
              inv <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) inv <<- solve
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}



##The following function calculates the inverse of the special "matrix"
##created with the above `makeCacheMatrix` function. However, it first checks
##to see if the inverse has already been calculated. If so, it `get`s the
##inverse from the cache and skips the computation. Otherwise, it calculates
##the inverse of the data and using the solve function and sets the value
##of the inverse in the cache via the `setInverse` function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
