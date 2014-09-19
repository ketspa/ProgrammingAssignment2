## makeCacheMatrix function creates a special matrix objiect that can cache its inverse,
## and then cacheSolve function computers the inverse of the matrix.
##If the inverse has already been calculated, 
##the cachesolve should retrieve the inverse from the cache, instead of calculate again.



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
         setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve computes the inverse of the matrix returned from the makeCacheMatrix function.
## If the inverse is cached, cacheSolve retrieves it, if not, the function will compute and return it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    } else {
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
    }
}
