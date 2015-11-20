## Put comments here that give an overall description of what your
## functions do


## set matrix
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverseM <- function(inverseM) m <<- inverseM
	getInverseM <- function() m
	list(set = set, get = get,
	     setInverseM = setInverseM,
           getInverseM = getInverseM)	
}


## get inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverseM()
      if(!is.null(m)) {
              message("getting cached data")
              return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInverseM(m)
      m
}