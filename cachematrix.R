# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix(sample(1:100, 9), 3, 3)) {
  # Initialize the cache for the inverse matrix
  s <- NULL
  
  # Set a new matrix and clear the cached inverse
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  # Get the current matrix
  get <- function() x
  
  # Cache the inverse matrix
  setsolve <- function(solve) s <<- solve
  
  # Get the cached inverse matrix
  getsolve <- function() s
  
  # Return a list of the methods
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# Function to compute the inverse of the special "matrix"
# If the inverse is already cached, it retrieves it
cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  s <- x$getsolve()
  if (!is.null(s)) {
    message("getting cached inverse matrix")
    return(s)
  }
  
  # Compute the inverse if not cached
  data <- x$get()
  s <- solve(data, ...)
  
  # Cache the computed inverse
  x$setsolve(s)
  
  # Return the inverse
  s
}

