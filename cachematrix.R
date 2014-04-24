## makeCacheMatrix() creates a special matrix object from which cacheSolve() can either produce
## an inversed version or load the inversed version from cache

## calculates the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  ## Initializing m
  m <- NULL
  ## Creates getter method that can be accessed from cacheSolve()
  set <- function(y) {
       x <<- y
       m <<- NULL
  }
  ## Creates getter method that can be accessed from cacheSolve()
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The function cacheSolve tries to access an already invered (and stored) version of the 
## matrix m from makeCacheMatrix().
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  ## If an inversed version of m already exists, it caches and returns it with a message 
  ## indicating that the object comes from the cache.   
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  ## If such an inversed version cannot be accessed, it is created, returned (without message),
  ## and stored in the cache
  else {
    m <- solve(x$get())
    x$setsolve(m)
    return(m)
  }
}
