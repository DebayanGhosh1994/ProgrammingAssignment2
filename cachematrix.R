## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
  {
  inv <- NULL   ## initializing inv to hold inverse matrix
  set <- function(y) 
    {
    x <<- y
    inv <<- NULL
  }
  get <- function() x   ## get value of matrix
  setinv <- function(inv) inv <<- inverse  ## set value of inverse matrix
  getinv <- function() inv    ## get value of inverse matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) ## checks whether inverse matrix is null
    {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()   ## get original matrix
  inv <- solve(data, ...)    ## solve function used to inverse the matrix 
  x$setinv(inv)    ## set inverse matrix
  inv   ## return inverse matrix
}

