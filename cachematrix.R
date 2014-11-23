## First, makeCacheMatrix initialize matrices to store the cached inverse matrix.
## Then cacheSolve will compute the inverse only if the inverse matrix has 
## not been cached. Otherwise, cacheSolve will compute the inverse if the inverse
## is null.

## Function initialize matrices to compute the inverse of theinputed matrix.
## makeCacheMatrix does not calculate the inverse of the matrix, only matrices
## to store the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinv <- function(invmatrix) inv <<- invmatrix
        getinv <- function()inv
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
        }
        
}


## cacheSolve verifies that the inverse of the input has been computed.
## If an inverse has been cached, it will return the cached value with a
## message. If an inverse is not cached, cacheSolve will compute it.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        B <- x$get()
        inv <- solve(B, ...)
        x$setinv(inv)
        inv ## Return a matrix that is the inverse of 'x'
}
        