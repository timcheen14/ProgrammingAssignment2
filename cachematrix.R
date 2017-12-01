## The following functions are used to take the inverse of a
## given matrix and store it in a cache for retrieval

# makeCacheMatrix takes an input matrix and creates a list of
# functions that 1) sets the value of a matrix; 2) gets the
# value of a matrix; 3) sets the inverse of a matrix and
# 4) gets the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


# cacheSolve takes an object created by makeCacheMatrix. It
# checks first if the value of m is NULL and then calculates the
# inverse of matrix x and store it to m. If m is not null, it
# displays the current value of m

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  x$setinv(m)
  m
}
