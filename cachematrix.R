## The set of functions below create a matrix object that stores the values
## of a square matrix and its inverse. This code is useful in situations
## where the values several matrices & their inverses are reused as it prevents
## the recalculation of the inverse and provides an intuitive way of accessing
## this information.

## makeCacheMatrix allows the storage and retrieval of a square matrix and its
## inverse. The function takes a matrix as an argument.
##
## An instance is created thusly: a <- makeCacheMatrix(b)
## The values b are retrieved by calling a$get() and changed by calling
## a$set(c), where the values of c will replace those of b.
##
## The xinv properties stores the inverse of the matrix.
## The values of xinv can be retrieved or modified using a$getinv and a$setinv.

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y){
                x <<- y
                xinv <<- NULL
        }
        get <- function() return(x)
        setinv <- function(inverse) xinv <<- inverse
        getinv <- function() return(xinv)
        
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)
}

## cacheSolve takes a makeCacheMatrix object as input and determined whether
## the inverse of the matrix contained within the object has been calculated.
## If it has been, it returns the cached inverse, otherwise it calculates
## the inverse using the solve() function, caches the results, and returns
## the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        if(!is.null(xinv)){
                message("Getting cached inverse")
                return(xinv)
        }
        xinv <- solve(x$get())
        x$setinv(xinv)
        return(xinv)
}