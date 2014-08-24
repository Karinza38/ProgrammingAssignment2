## This function will output a special vector. This vector is a list of functions.
## The functions are (1) set the value of the matrix, (2) get the value of the matrix
## (3) set the inverse of the matrix and (4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix(), nR=0, nC=0) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        if(nR==0 || nC==0)
            return(NULL)
        else
            matrix(x, nrow=nR, ncol=nC, byrow=TRUE)
    }
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function checks if the inverse of the matrix is already present in the
## cache. If the inverse of the vector is present in the cache then the function
## will not calculate the cache again

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i[1,1])) {
        message("getting cached data")
        return(i)
    }
    
    if(is.null(x$get()))
    {
        message("input matrix has a problem")
        return()
    }
    data <- x$get()
    i <- solve(data,...)
    x$setInverse(i) 
        
    i
}
