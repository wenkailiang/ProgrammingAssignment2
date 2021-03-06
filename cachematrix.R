## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.  
## This is a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	invx <- NULL
	set <- function(y) {
		x <<- y
		invx <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) invx <<- inverse
	getinverse <- function() invx
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	invx <- x$getinverse()
	if(!is.null(invx)) {
		message("getting cached inverse")
		return (invx)
	}
	data <- x$get()
	invx <- solve(data)
	x$setinverse(invx)
	invx
}
