## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  matrix_set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  matrix_set_solve <- function(solve) m <<- solve
  solve_matrix <- function() m
  list(matrix_set = matrix_set, get = get,
       matrix_set_solve = matrix_set_solve,
       solve_matrix = solve_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$solve_matrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$matrix_set_solve(m)
  m
}
