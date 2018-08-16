## makeCacheMatrix() creates an R object that stores a Matrix and its inverse
## it also creates an object of type list() with functions (getters and setters)
## that, when used through cacheSolve, will allow to access the values of x or i

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
  

## cacheSolve() requires an argument that is returned by makeCacheMatrix ()
## in order to retrieve the inverse from the cached value that is stored
## in the makeCacheMatrix() object's environment.One has to use cacheSolve function twice to confirm
## the use of cached data (message "getting cached data")

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## testing passed using example_matrix <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

