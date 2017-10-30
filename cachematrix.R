## Cashe and scoping programming assignment for coursera

## makeCasheMatrix will serve as the creation and storage of an object like state, which includes a matrix as well as it's cashe'd inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve will return the inverse of x, a matrix stored by makeCacheMatix

cacheSolve <- function(x, ...) {
# if (!is.square.matrix(x$get)) {
#   print("Only square matrixes have inverses")
#   return()
#  }
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
