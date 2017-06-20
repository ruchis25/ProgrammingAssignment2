## This file contains two functions definitions one is makeCacheMatrix()
## and cacheSolve(). These function are used to calculate the inverse
## of the matrix, if it was not already calculated else it will read the
## value of inverse from the cache.

## makeCacheMatrix() sets the matrix for which the inverse has to be calculated
## it also stores the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    mat_inv <- NULL
    set <- function(x){
        m <<- x
        mat_inv <<- NULL
    }
    get <- function() m
    setinverse <- function(m_inverse) mat_inv <<- m_inverse
    getinverse <- function() mat_inv
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## cacheSolve() verifies whether the inverse of matirx is already caluclated
## if not it calculates the inverse else reads from the cache. This function
## calls in built solve() to calculate the inverse of a matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m_inverse <- x$getinverse()
    if(!is.null(m_inverse)){
        message("getting cached data")
        return(m_inverse)
    }
    data <- x$get()
    m_inverse <- solve(data,...)
    x$setinverse(m_inverse)
    m_inverse
}



