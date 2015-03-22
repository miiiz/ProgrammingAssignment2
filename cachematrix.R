## Two functions that cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL     ## define the cache m
      set <- function(y) {
            x <<- y ## assign y to x in the arent environment
            m <<- NULL ## re-initialize m in the parent environment to null
      }
      get <- function() x ## return x
      setinverse <- function(inverse) m <<- inverse 
      getinverse <- function() m 
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
      ## Return the inverse of 'x'
      
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