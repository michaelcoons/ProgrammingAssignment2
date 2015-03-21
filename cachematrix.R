###################################################################
## Programming assignment #2 
## Michael Coons
###################################################################
## Function: makeCacheMatrix
## Description: Creates a vector of functions used store/retrieve a
##              matrix and cache it's inverse
## Return:      List of functions in a vector
#############################################################
makeCacheMatrix <- function(x = matrix()) {
	## Set the inverse cache to NULL
	inv <- NULL

	## Create the set function
	set <- function(y) {
		## Using teh <<- operator, allows for setting x and inv via lexical scoping
		x <<- y
		inv <<- NULL
	} ## End if set function

	## Get function just returns the matrix
	get <- function() x

	## setinverse function gets the inverse of the matrix and stores it
	setinverse <- function(inverse) inv << inverse

	## getinverse is used to return the inverse matrix of the value
	getinverse <- function() inv

} ## End of function makeCacheMatrix


###################################################################
## Function: cacheSolve
## Description: This function calculates the inverse of a matrix
##              created with the makeCacheMatrix functions but it 
##              first checks to see if the inverse has already been 
##              calculated.
## Return:      Inverse of a matrix
#############################################################
cacheSolve <- function(x, ...) {
	## First see if there is a cached inverse matrix
	inv <- x$getinverse()
	if (is.null(inv)) { ## if it is already cached
		message("Already cached, returning value")
		return(inv)
	}

	## If it is not available in cache, go and calculate the inverse
	data_matrix <- x$get()
	inv <- solve(data_matrix, ...)

	## Set the inverse matrix in cache before returning
	x$setinverse(inv)

        ## Return the calculated inverse value
	inv

} ## End of function cacheSolve
