

# The first function, makeVector creates a special "matrix", which is really a list containing a function to
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

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


# The following function calculates the mean of the special "matrix" created with the above function. 
# Firstly it checks to see if the mean has already been calculated and if it has it gets the mean from the cache and skips the computation. 
# eElse it calculates the mean of the data and sets the value of the mean in the cache with aid of the above function.

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
