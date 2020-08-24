## My functions take a matrix, cache its inverse, and later use this
## if possible

## It caches the inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(Solve) s <<- Solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Returns a matrix that is the inverse of 'x', from cache if available

cacheSolve <- function(x, ...) {
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}
