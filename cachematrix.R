## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(), nR, nC) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() matrix(x, nrow=nR, ncol=nC, byrow=TRUE)
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i[1,1])) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setInverse(i)
    i
}
