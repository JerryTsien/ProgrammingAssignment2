## Below are two functions that are used to create a special object
## that stores a matrix and caches its inverse.

## This function creates a special "matrix", which is really a list containing 4 functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the cache
  inv <- NULL
  ## function 1. set the value of the matrix
  set <- function (y) {
    x <<- y
    ## The matrix is changed, therefore the cache has to be initialized again
    inv <<- NULL
  }
  ## function 2. get the value of the matrix
  get <- function() x
  ## function 3. set the value of the inverse
  setinverse <- function(inverse) inv <<- inverse
  ## function 4. get the value of the inverse
  getinverse <- function() inv
  
  ## Return a list of the 4 functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" created with the above function.

cacheSolve <- function(x, ...) {

  inv <- x$getinverse()  ## It'll be NULL if the inverse hasn't been computed
  if(!is.null(inv)) {
    ## The inverse has already been computed, so it can be retrieved directly from the cache
    return(inv)
  }

  ## The cache is empty, so the inverse has to be computed from scratch
  data <- x$get()
  inv <- solve(data, ...)  ## Assuming the matrix is always invertible, otherwise an error will be thrown

  ## The newly computed inverse is stored in the cache for future retrieval
  x$setinverse(inv)

  ## Return the inverse
  inv
}
