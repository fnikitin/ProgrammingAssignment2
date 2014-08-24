## makeCacheMatrix: 
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	
  # cached inverse matrix of x
  xinv <- NULL
  
  ## set:
  # a function that set the matrix data
  set <- function(y) {
  	x <<- y
  	xinv <<- NULL
  }
  
  ## get:
  # a function that get the matrix data
  get <- function() x
  
  ## setinv:
  # a function that set the inverse matrix
  setinv <- function(inv) xinv <<- inv
  
  ## get:
  # a function that return the inverse matrix
  getinv <- function() xinv
  
  # return the list of methods defined above
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve: 
# This function computes the inverse of the special "matrix" x
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve method should retrieve the inverse from the cache with 
# a message that notify it.
cacheSolve <- function(x, ...) {
	
  # compute inverse of matrix x 
  inv <- x$getinv()
  
  # get cached inverse
  if(!is.null(inv)) {
  	
  	message("getting cached inverse matrix")
  } 
  # calculate inverse
  else {
  	
    # get matrix data
    data <- x$get()
    
    # calculate the inverse
    inv <- solve(data, ...)
    
    # cache the calculated inverse matrix to x
    x$setinv(inv)
}

  # return inverse matrix 
  inv
}
