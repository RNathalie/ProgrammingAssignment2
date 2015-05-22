# creating  a function with matrix argument
makeCacheMatrix <- function(x = matrix()) {
  ##assigning the value of inv. matrix to null
  m <- NULL
  ## creating function set for caching
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## assigning the value of inv. 
  get <- function() x
  ##usage of solve function for the inverse
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#getting cache
cacheSolve <- function(x) {
  m <- x$getsolve()
  ##condition (if there existing inv., taking it)
  if(!is.null(m)) {
    message("getting cached data - Matrix inverse")
    return(m)
  }
  ##if no inv. - computes inv. matrix
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}

