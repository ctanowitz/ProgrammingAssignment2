## these functions follow the direction of the course. The first caches a matrix.
## The second returns a matrix that is the inverse of x.

## This makes a cache of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function()m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## This solves for x, but checks first if the answer is cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
