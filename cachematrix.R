## The purposes of these function is to find the inverse of a matrix and use lexical scoping to cache
## the inverse so it can be reused without being recomputed

## makeCacheMatrix creates a list of functions to:
## set the values of a matrix, return the values of a matrix, set the value of the matrix's inverse
## and return the value of the maatrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  ## return vector of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve inverts a matrix after checking the cache to see if i exists. 
## If i is not null it returns the value of i from cache.
## if i is null it calculates the inverse of the matrix, sets i to the calculated value and then returns i

cacheSolve <- function(x, ...) {
        
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    ## Return a matrix that is the inverse of 'x if i is not null'
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}
