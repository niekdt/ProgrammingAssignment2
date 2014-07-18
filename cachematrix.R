## The functions below provide a way to compute the inverse of a matrix and caching its result

## makeCacheMatrix(x = matrix()) constructs a special matrix object which is used
## to cache the result of computating the inverse of the given matrix x.
## A new matrix can be assigned to the object, in which case the cache will be cleared. 

makeCacheMatrix <- function(x = matrix()) {
	inverseMat <- NULL
	set <- function(y) { #update the matrix
		x <<- y
		message('matrix changed: clear cache')
		inverseMat <<- NULL
	}
	
	get <- function() x
	
	setsolve <- function(inverse) {
		inverseMat <<- inverse
	}
	
	getsolve <- function() inverseMat
	
	return(list(set=set, get=get, setsolve=setsolve, getsolve=getsolve))
}


## cacheSolve(x, ...) computes the inverse (as a numerical matrix) of the given special matrix object
## The result is cached, meaning that on successive calls do not require recomputation, unless the matrix is changed 

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inverseMat <- x$getsolve()
	
	if(is.null(inverseMat)) { #is the cache empty?
		inverseMat = solve(x$get())
		x$setsolve(inverseMat)
		return(inverseMat)
	}
	else {
		message('Return from cache')
		return(inverseMat)
	}
}
