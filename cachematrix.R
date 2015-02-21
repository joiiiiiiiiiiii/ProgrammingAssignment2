## Solution to Programming Asignment2.
#    These 2 functions are able to cache data of a potentially time consuming 
#    computation: calculate inverse of a matrix
#
## Function: makeMatrix
#     creates a list (ie a new object) from a matrix (variable x
#     passed to the function when called). The list includes functions to
#
#	1. set the value of the matrix
#	2. get the value of the matrix
#	3. set the value of the inverse matrix
#	4. get the value of the inverse matrix

makeMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y) {
             x <<- y
             inverse <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) inverse <<- solve
     getinverse <- function() inverse
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)

}


## Function: cacheSolve
#    It calculates, or recovers from the cache if previously calculated,
#    the inverse of a matrix the variable x passed to the function must
#     be a list created by the function makeMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        return(inverse)

}
