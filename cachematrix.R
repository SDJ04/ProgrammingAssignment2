## This function takes up a matrix, sets the values of the matrix and calculates its inverse.
## The idea behind this is that each matrix whose inverse has been calcluated once can be cached
## whenever required without actually calculating it again.

## makeCacheMatrix receives data from a matrix, sets it & returns its inverse into the Cache memory

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
  set_matrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  get_matrix <- function() x
  set_inverse <- function(solve) m <<- solve
  get_inverse <- function() m
  list(set = set_matrix, get = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}


## cacheSolve returns the inverse of a matrix returned by the makeCacheMatrix above. In case it has already been computed, it is cached.

cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}
