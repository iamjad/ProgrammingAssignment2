## Repeated Inverse of a Matrix Calculation is an expensive operation especially
## when done on large data. This R file introduces caching technique for
## storing inverse of a matrix in-memory.

## This function takes a matrix and returns a special matrix which can
## store matrix and its inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
		## local variable inverse initiated with NULL.
		inverse <- NULL
		
		## setter and getter of matrix
		set <- function(y) {
				x <<- y
        		inverse <<- NULL
     	}
		get <- function() x
		

		## setter and getter of inverse
		setinverse <- function(inversematrix) inverse <<- inversematrix
		getinverse <- function() inverse
		
		##return list object
		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function returns inverse of a special matrix created by makeCacheMatrix
## If the inverse of matrix exists in cache, it'll not compute the invese.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversematrix <- x$getinverse()
        
        ## return inverse if exists in cache
        if(!is.null(inversematrix)) { 
                message("getting cached data")
                return(inversematrix)
        }

        ## compute invese of matrix and store it in the cache
        matrix <- x$get()
        inversematrix <- solve(matrix, ...)
        x$setinverse(inversematrix)
        
        ## Return computed inverse
        inversematrix
}
