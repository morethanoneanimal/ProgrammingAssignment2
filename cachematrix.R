## Creating and operating on cached matrix

# This function is pretty straightforward once you studied makeVector function, it does pretty much the same
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	get <- function() x
	setInverse <- function(inv) inverse <<- inv
	getInverse <- function() inverse 
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Again, it's almost copy of cachemean function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	x$setInverse( solve(data) )
	x$getInverse()
}
