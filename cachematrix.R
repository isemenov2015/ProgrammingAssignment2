## Describes a 'Constructor' in OOP terms for matrix inversing, i.e. a 'class' that
## incapsulates all methods and data for matrix inversing and storing the results in cache

## Describes set, get, getinverse & setinverse methods for matrix inversing and access results from cache
## if available

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Interface function for cached matrix inversing. Returns inversed matrix for input matrix passed in x.
## Uses stored matrix inverse results from cache if available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m    
}
