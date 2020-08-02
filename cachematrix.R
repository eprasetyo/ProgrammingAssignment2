## Lexical Scoping: caching the inverse of a matrix
## This function creates a special "matrix" object that can cache its inverse

## makeCacheMatrix creates matrix a special "vector"

## set the value of matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    ## get the value of matrix
    get <- function() x
    
    ## set the value of inverse
    setinverse <- function(inverse) i <<- inverse
    
    ## get the value of matrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
