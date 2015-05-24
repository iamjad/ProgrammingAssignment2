## Repeated Inverse of a Matrix Calculation is an expensive operation especially
## when done on large data. This R file introduces caching technique for
## storing inverse of a matrix in-memory.

## This function takes a matrix and returns a special matrix which can
## store matrix and its inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
		inverse <- NULL
		set <- function(y) {
				x <<- y
        		inverse <<- NULL
     	}
		get <- function() x
		setinverse <- function(inversematrix) inverse <<- inversematrix
		getinverse <- function() inverse
		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function returns inverse of a special matrix created by makeCacheMatrix
## If the inverse of matrix exists in cache, it'll not compute the invese.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversematrix <- x$getinverse()
        if(!is.null(inversematrix)) {
                message("getting cached data")
                return(inversematrix)
        }
        matrix <- x$get()
        inversematrix <- solve(matrix, ...)
        x$setinverse(inversematrix)
        inversematrix
}
