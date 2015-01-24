## Two functions that cache a matrix and calculate and cache, or 
## retrieve, its inverse.

## makeCacheMatrix: creates and caches a matrix and returns a list of 
## functions that either cache or retrieve values of the matrix, and
## cache or retrieve values of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
#Sets value of vinverse value to NULL
    inv <- NULL
#Caches matrix and NULL inverse value
    setMat <- function(y) {
        x <<- y
        inv <<- NULL
    }
#Returns matrix
    getMat <- function() x
#Caches value for inverse
    setInv <- function(inverse) inv <<- inverse
#Retrieves value cached in inverse
    getInv <- function() inv
#Creates and returns list of functions to object
    list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
}


## cacheSolve: retrieves the value stored for the inverse of a matrix 
## by makeCacheMatrix() function. If the value of the inverse is NULL, the
## inverse is calculated and then cached via the functions listed for
## the matrix by makeCacheMatrix(). The inverse matrix is returned.

cacheSolve <- function(x, ...) {
#Retrieves value cached in inverse
    inv <- x$getInv()
#Returns inverse value if not NULL
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
#Retrieves matrix, calculates inverse values, and caches inverse
    mat <- x$getMat()
    inv <- solve(mat, ...)
    x$setInv(inv)
    inv
}
