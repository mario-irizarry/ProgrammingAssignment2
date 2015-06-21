## the following two functions:
## 1. creates a matrix that can take its inverse
## 2. calculates and cache's its inverse

## 1.
## this function creates and caches a matrix that can take/cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }  ##  end set function
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}  ##  end make cache matrix

## 2.
## this function calculates and caches the inverse of the provided matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse of matrix")
    return(m)
  }  ##  end if not null
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}  ##  end cache solve function
