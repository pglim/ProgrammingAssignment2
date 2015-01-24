## This function creates a "matrix" object that can cache its inverse

## Provide a list containing functions to:
##   a. Set and get Matrix
##   b. set and get inverse

makeCacheMatrix <- function(x = matrix()) {
      invmtx  <- NULL 
      set  <- function(y){ 
           x <<- y 
           invmtx <<- NULL  
           } 
      get  <- function() x 
      setinverse  <- function(inverse) invmtx  <<- inverse 
      getinverse  <- function() invmtx 
      list(set= set, get = get, setinverse = setinverse, getinverse = getinverse) 
      }


## Take output form makeCacheMatrix() and return inverse of the matrix

cacheSolve <- function(x, ...) {
      invmtx  <- x$getinverse() 
      if (!is.null(invmtx)){ 
            message("getting cached data...") 
            return(invmtx) 
            }
      invmtx <- solve(x$get())
      x$setinverse(invmtx)
      return(invmtx)
      }

