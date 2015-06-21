## these functions cache the inverse of a matrix avoiding having to compute it repeatedly

## this function creates a list containing a functions to 
## 1) set the matrix
## 2) get the matrix
## 3) set the matrix inverse
## 4) get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) { ## sets the matrix and inverse
    x <<- y
    i <<- NULL
  }
  get <- function() x ##returns the matrix
  setinverse <- function(solve) i <<- mean ##calculates the matrix inverse using solve 
  getinverse <- function() i ## returns the matrix inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## this function checks if inverse has been cached
## if yes, then it returns the cached inverse and doesn't do any computation
## if no, then it computes the inverse and returns the computed inverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse() ##get the matrix inverse from object x from makeCacheMatrix function
  if(!is.null(i)) { ## check if matrix inverse is cached
    message("getting cached data")
    return(i) ##return cached matrix inverse
  }
  data <- x$get()
  i <- solve(data, ...) ##caclulating matrix inverse if not cached
  x$setinverse(i) ## setting caclulated matrix inverse for future calculations
  i  ## return caclulated matrix inverse
  }

