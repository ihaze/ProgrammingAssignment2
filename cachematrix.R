## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

## ----------------------------------------- ##
#   1. set the value of the vector
#   2. get the value of the vector
#   3. set the value of the mean
#   4. get the value of the mean
## ----------------------------------------- ##

makeCacheMatrix <- function(x = matrix()) {
    
    inverseMatrix <- NULL
    
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    get <- function() x
    setinverse <- function( inverse ) inverseMatrix <<- inverse
    getinverse <- function() inverseMatrix
    list( set = set, 
          get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}


## Write a short comment describing this function

## ----------------------------------------- ##
# if it is possible solve inverse matrix
## ----------------------------------------- ##

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    xmatrix <- x$getinverse()
    
    if ( !is.null( xmatrix )) {
        message("getting cached data.")
        return( xmatrix )
    }
    
    data <- x$get()
    xmatrix <- solve(data)
    
    x$setinverse(xmatrix)
    xmatrix
}

