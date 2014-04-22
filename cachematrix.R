## Put comments here that give an overall description of what your
## functions do

## Function to operate matrix with cached inverse. Usage:
## x <- makeCacheMatrix(matrix()) //creates x as cached matix
## x$get() // gets x value
## x$set(matrix()) // sets new x value
## x$getinverse() // gets inverse of x
## x$setinverse(matrix()) // sets new cached inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(invN) inv <<- invN
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function to calculate inverse of matrix with caching. Usage:
## inv <- cacheSolve(makeCacheMatrix(matrix())) // returnes inverse of matrix using cache if available

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()

	# return cached version if available
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

	# calculate inverse matrix and cache it
        mtx <- x$get()
        inv <- solve(mtx, ...)
        x$setinverse(inv)
        inv
}
