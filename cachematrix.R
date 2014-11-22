## Put comments here that give an overall description of what your
## functions do

# The first function below creates a special "matrix" object that can cache its inverse.
# The second function below computes the inverse of the special "matrix" returned by "makeCacheMatrix". 
# However, if the inverse has already been calculated and no changes where made to the matrix
# then this function retrieves the inverse from the cache saving computational time.

## Write a short comment describing this function:

# x will be a matrix that can be inversed
# m will be the inverse matrix and it is reset to NULL when makeCacheMatrix is recalled. 
# get returns the original matrix
# setinv will get the inverse matrix and store it in cache
# getinv will returthe cached inverse matrix
# list creates a list of these 4 objects that are returned everytime that makeCachematrix is called 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function:

# the input x is created by makeCacheMatrix
# if the inverse matrix was already cached then it returns it and sends the message to the console. 
# However, if getinv returned NULL, the matrix will be called by x$get()
# and calculated by solve(data,...) here and will be returned. 

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

## Return a matrix that is the inverse of 'x'

