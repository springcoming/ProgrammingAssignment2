## Matix inversion is usually a costly computation and there may be some benifit to caching the
## inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix, this function creates a special "matrix" object that can cache its inverse.
## the spcial "matrix" object has a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    sol <- NULL
    
    set <- function(y){
        x <<- y
        sol <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) sol <<- inverse
    getinverse <- function() sol
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve, this function computes the inverse of the spcial matrix returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the 
## cacheSolve shoud retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    sol <- x$getinverse()
    
    if(!is.null(sol)){
        message("getting cached data.")
        return(sol)
    }
    
    data <- x$get()
    sol <- solve(data)
    x$setinverse(sol)
    sol
}

## Example:
## X <- cbind(c(0, 1, 2), c(1, 1, -1), c(2, 4, 0))
#
## Creates a "matrix" object.
## M <- makeCacheMatrix(X)
## M$get()
##
## console outputs:
##      [,1] [,2] [,3]
## [1,]    0    1    2
## [2,]    1    1    4
## [3,]    2   -1    0
##
## Computing inverse of the matrix for the first time.
## cacheSolve(M)
##
## console outputs:
##      [,1] [,2] [,3]
## [1,]  2.0   -1  1.0
## [2,]  4.0   -2  1.0
## [3,] -1.5    1 -0.5
##
## Retrieving the inverse from the cache for the second time.
## cacheSolve(M)
##
## console outputs:
## getting cached data.
## [,1] [,2] [,3]
## [1,]  2.0   -1  1.0
## [2,]  4.0   -2  1.0
## [3,] -1.5    1 -0.5