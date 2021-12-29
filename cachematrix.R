## The following function creates a special vector i.e. a list containing a function to
## set and get the value of the vector
## set and get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The 2nd function calculates the inverse of the special vector created with the makeCacheMatrix. 
## It first checks to see if the inverse has already been calculated. 
## If yes, it gets the inverse from the cache skipping the calculation. 
## If not, it computes the inverse, additionally setting the inverse value in the cache with the setinverse function.

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
