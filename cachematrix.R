## Allow caching the computation of the inverse of matrices thus saving time.
## To use do:
## cm <- makeCacheMatrix(m)    #Stores matrix m, m must be invertible
## cacheSolve(cm) #Returns the inverse of m. This only gets calculated the first time, returned quickly from cache after that

## Creates a special matrix object that can cache its inverse
## Use with the cacheSolve function below
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
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


## Calculates the inverse of the special "matrix" created with the makeCacheMatrix function
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value of the inverse in the cache
## via the setinverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## put the inverse in the cache as it does not exist
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
