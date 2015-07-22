## Two functions to create a matrix and a value for its verse, and then to check to see
## if the inverse exists, and create it if not

## A function contain functions to set a matrix, get the matrix, set the matrix
## inverse, and get the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inv)  m <<- inv
  
  getinv <- function()  m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns the set inverse of the matrix, or if null, calculutes, stores and returns
## the matrix inverse

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
