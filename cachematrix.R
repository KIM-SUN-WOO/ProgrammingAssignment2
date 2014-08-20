## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## variables in the closure of the 4 functions.
## x : matrix
## i : inversion of the matrix. if it is null, it is not computed yet.

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL

	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() x

	setinv <- function(ii) i <<- ii

	getinv <- function() i

	list(set=set, get=get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	i <- x$getinv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()

	i <- solve(data, ... )
	x$setinv(i)

	i
}
