## Create a special matrix with the ability to cache its inverse
## Once the inverse is neeeded, we check if this is already cached,
## If not, then we calculate it and store it

## Create the special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## Checks for the inverse in the cache
## If not ready, calculate it and store it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if (!is.null(i)) {
        message("Cached Data!")
        return(i)
    }
    data <- x$get()
    i <- MASS::ginv(data)
    x$setinv(i)
    i
}
