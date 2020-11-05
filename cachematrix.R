# These two functions create an object that stores a matrix and caches its inverse.

# The makeCacheMatrix function creates a special “matrix”, which is essentially a list containing a function to:
#
# 1. Set the value of the matrix.
# 2. Get the value of the matrix.
# 3. Set the value of the matrix inverse.
# 4. Get the value of the matrix inverse.
#
# It also checks whether the matrix is invertible right at the beginning and returns and error if not.

makeCacheMatrix <- function(x = matrix()) {
  if (ncol(x) != nrow(x) || det(x) == 0) {
    return(message("ERROR: This matrix is not invertible."))
  }
  
  else {
    inv <- NULL
    
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
}


# The cacheSolve function calculates the inverse of the special "vector" created with the makeCacheMatrix function above.
# But before calculating the inverse it checks whether the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# If not, it calculates the inverse and sets it in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("Getting the cached data.")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}