## This function will output a special vector. This vector is a list of functions.
## The functions are (1) set the value of the matrix, (2) get the value of the matrix
## (3) set the inverse of the matrix and (4) get the inverse of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(), nR, nC) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    
    get <- function() matrix(x, nrow=nR, ncol=nC, byrow=TRUE)
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function checks if the inverse of the matrix is already present in the
## cache. If the inverse of the vector is present in the cache then the function
## will not calculate the cache again.

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
