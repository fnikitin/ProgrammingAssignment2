## makeCacheMatrix: 
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

	inverse <- NULL

	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	
	get <- function() x
	
	setinv <- function(inv) inverse <<- inv
	
	getinv <- function() inverse
	
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: 
# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cacheSolve method should retrieve the inverse from the cache with 
# a message that notify it.
cacheSolve <- function(x, ...) {

	inv <- x$getinv()
	
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
