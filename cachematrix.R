### The following two functions compute the inverse of a matrix and
### cache the result in a special matrix object

## This function creates a special "matrix" object x that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y # assign x to a another environment
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" x
## created by makeCacheMatrix if its inverse is not cached or
## returns it without calculation if its already cached  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    # checks if the inverse its already calculated 
    if(!is.null(m)) {
        # if its already calculated then the cached inverse is printed
        message("getting cached data")
        return(m)
    }
    # else the inverse is computed
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}