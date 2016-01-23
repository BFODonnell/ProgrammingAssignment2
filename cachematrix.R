# makeCacheMatrix creates a special "matrix" object 
# that can cache its inverse and is a list containing
# a function to:
#
#     set the value of the matrix
#     get the value of the matrix
#     set the value of the inverse
#     get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
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


# The following function calculates the inverse of the special "matrix" 
# created by makeCacheMatrix. However, it first checks to see 
# if the inverse has already been calculated (and the matrix 
# has not changed). If so, it gets the inverse from the cache and skips 
# the computation. Otherwise, it calculates the inverse of the data and 
# sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv) && identical(x,x$get)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}



# Test the above code
# Generate a 4x4 matrix of random numbers.
runtest <-FALSE
if (runtest){
  b = matrix( rnorm(16, mean=0, sd=1), 4, 4) 
  # Cache this matrix.
  my_var2 <- makeCacheMatrix(b)
  # Compute the inverse matrix.
  res<-cacheSolve(my_var2)
  res
}
