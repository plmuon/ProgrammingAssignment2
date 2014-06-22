## Put comments here that give an overall description of what your
## functions do

## Creates matrix object in the form of a list continaing 4 functions:
## 1. set value of the matrix
## 2. get value of the matrix
## 3. set value of the inverse matrix
## 4. get value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <-NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return inverse matrix.
## This function takes a "matrix" as created by makeCacheMatrix
## and will cache the result of the matrix inversion in the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
    inv  <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv  <- solve(data, ...)
    x$setinv(inv)
    inv
}
