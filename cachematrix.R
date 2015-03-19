## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## inizialize m and y for cacheSolve() function
  ## m is a matrix object used to cache the inverse of matrix x
  m <- NULL
  ## assign the value of the matrix to check if y is already checked
  set <- function(y) {
    ## the <<- operator is used to assign a value to x from a different environment
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solve) m <<- solve
  
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  ## if m is not empty returns m skipping computation
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## else computes the inverse of x and returns m
  mat<-x$get()
  m<-solve(mat, ...)
  x$setinverse(m)
  return(m)
}
