## makeCacheMatrix takes a matrix as a parameter and creates a list that contains a function to set the value of the matrix,
## get the value of the matrix, set the value of the inverse, and get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
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


## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix.  It first checks to see if the inverse has
## already been calculated.  If so, it gets the inverse from the cache.  Otherwise it calculates the inverse using the solve
## function

cacheSolve <- function(x, ...) {
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
