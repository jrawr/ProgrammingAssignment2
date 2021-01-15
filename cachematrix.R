## Matrix Inversion is quiet costly. Computing the matrix repeatedly 
## takes a lot of time and effort, thus caching a matrix is beneficial. 

## Initializing the Inverse matrix which is set to 'm' as NULL

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
## Setting the function for the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Solving the cache of the inverse of the special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
