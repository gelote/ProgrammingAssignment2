# Matrix inversion is usually a costly computation and their may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly (there 
# are also alternatives to matrix inversion that we will not discuss here). 
# The folloing pair of functions can be used to cache the inverse of a matrix.

# Function: makeCacheMatrix
# Creates a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}

# Function: cacheSolve
# Returns the inverse of the matrix 'x' (assumes it is always invertible).
# If it's used for the first time, it computes the inverse and caches it.
# Following calls will use the cached value.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
