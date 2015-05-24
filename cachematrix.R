## Functions that cache the inverse of a matrix

## Saves the value of the inverse of a matrix in the environment so as to no 
## overwhelm the processing capabilities when confronted with a rather
## complex matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## Recover the inverse of a matrix from the cache if there is one to begin
## with

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setinverse(i)
        i
}
