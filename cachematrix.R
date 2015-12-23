## These functions allow to cache a result possibly
## long to compute about a matrix : its inverse.
## Then it would be easier to get it when needed, the
## inverse is simply got from cache when already
## computed, and calculated when it's not

## Function creating a 4-functions list to :
## - Get the matrix x
## - Set the matrix x
## - Get the value of the element inv from this environment
## - Set the value of the element inv from this environment

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(newmat) {
		x <<- newmat
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse ,
		getinverse = getinverse )
}


## The argument need to be the result of the function
## makeCacheMatrix.
## If the inverse of the matrix x has already been
## computed, then it gives it from the cache, else the
## function computes, caches, and returns it

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setinverse(inv)
	inv
}
