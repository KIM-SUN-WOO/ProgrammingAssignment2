## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Variables for use in the closure of the 4 functions.
##   ( the functions are like methods in object oriented sense )

## x : matrix
## i : inversion of the matrix. if it is null, it is not computed yet.
## When we call makeCacheMatrix with a parameter which is a matrix,
##  set(), get(), setinv() and getinv() functions are defined with the
##  variables in their closure environment.

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


## Making use of the cache to compute the inverse of the matrix.
##
## Check first whether the inversion was already computed or not.
## If there is in the cache, use it.
## Otherwise, compute the inverse of the matrix.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

	i <- x$getinv()

	## Check whether the inversion of the matrix exists or not
	## If it is there, return it.
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	## The inversion of the matrix was not computed yet.
	##  So, get the matrix
	data <- x$get()

	##  Then compute the inverse of the matrix
	i <- solve(data, ... )

	##  Finally, save it for later use.
	x$setinv(i)

	i
}
