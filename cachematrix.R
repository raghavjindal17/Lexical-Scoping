# Function to create a special "matrix" that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse when a new matrix is set
  }
  
  get <- function() x  # Retrieve the matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
  getInverse <- function() inv  # Retrieve the cached inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the "matrix" or retrieve the cached value
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  
  if (!is.null(inv)) {
    message("getting cached data")  # If cached, return it
    return(inv)
  }
  
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the inverse
}
