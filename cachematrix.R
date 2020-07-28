## Put comments here that give an overall description of what your
## functions do: 
###My function creates a special “matrix” object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invertir <- NULL
  set <- function(y){
    x <<- y
    invertir <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) invertir <<- inverse
  getInverse <- function() invertir
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

##This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
###If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 invertir <- x$getInverse()
  if(!is.null(invertir)){
    message("getting cached data")
    return(invertir)
  }
  mat <- x$get()
  invertir <- solve(mat,...)
  x$setInverse(invertir)
  invertir
}
