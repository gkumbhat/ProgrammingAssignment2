## Put comments here that give an overall description of what your
## These functions make a special matrix and then cache the inverse of that special matrix

## Write a short comment describing this function

## makeCacheMatrix creates a special matrix which can be cached later on for the efficient usage

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL 
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a inverse matrix of x
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached inverse")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
