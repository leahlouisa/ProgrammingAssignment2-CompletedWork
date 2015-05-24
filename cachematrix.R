## R Programming Week 3 Homework: 

## Creates a special matrix that can cache its inverse, per the assignment

makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
    set <- function(y) {
        x <<- y
        c <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) c <<- inverse
    getInverse <- function() c
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Works with makeCacheMatrix to hopefully return a cached value of the inv. matrix
## Otherwise, calculates the inverse and returns it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    c <- x$getInverse()
    if(!is.null(c)) {
        message("getting cached inverse")
        return(c)
    }
    data <- x$get()
    c <- solve(data, ...)
    x$setInverse(c)
    c
}
