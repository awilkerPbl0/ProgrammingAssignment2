## These functions cache the inverse of a matrix rather than repeatedly computing it.

## Provide cache management for x and its inverse

makeCacheMatrix <- function(x = matrix()) {
## (matrix) -> list(function, function, function, function) 

	inverse <- NULL
	
	set <- function(y) {
		# matrix changed
		x <<- y
		
		# reset inverse
		inverse <<- NULL
	}
	get <- function() x
	
	setsolve <- function(solve) inverse <<- solve
	getsolve <- function() inverse
	
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)

}


## Retrieve the inverse of a matrix from the cache, if available; otherwise computes the inverse matrix.
## ASSUMES: supplied matrix is invertible

cacheSolve <- function(x, ...) {
## (<returned from makeCacheMatrix>, ...) -> matrix

        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getsolve()
        
        if (!is.null(inverse)) {
        	message("getting cached data")
        	return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data, ...)
        x$setsolve(inverse)
        inverse
}
