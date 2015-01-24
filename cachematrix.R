## Caching the Inverse of a Matrix

## Function : makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(nrow=2, ncol=2)) {

        ## Checking if 'x' is a square invertible matrix. 
        if (dim(x)[1] != 2) {
            stop ("Bad matrix value. (", dim(x)[1], " x ", dim(x)[2], ") is not a square invertible matix")
        } else if (dim(x)[2] != 2) {
            stop ("Bad matrix value. (", dim(x)[1], " x ", dim(x)[2], ") is not a square invertible matix")
        }
    
        s <- NULL
        set <- function(y = matrix(nrow=2, ncol=2)) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Function : cacheSolve 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s

}
