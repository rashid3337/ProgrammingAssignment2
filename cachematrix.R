## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL 				
	set_matrix <- function(y) {			
		x <<- y 				
		inverse <<- NULL
	}
	get_matrix <- function() x 				
	set_inverse <- function(solve) inverse <<- solve 	 
	get_inverse <- function() inverse 			
	list(set_matrix = set_matrix, get_matrix = get_matrix,
	     set_inverse = set_inverse,
	     get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {inverse <- x$get_inverse()			
	if(!is.null(inverse)) {					
		message("getting cached data")			
		return(inverse)
	}
	data <- x$get_matrix()					
	inverse <- solve(data, ...)			
	x$set_inverse(inverse)					
	inverse 
        ## Return a matrix that is the inverse of 'x'
}
