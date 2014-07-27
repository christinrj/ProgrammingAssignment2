## The makeCacheMatrix function creates a matrix object that can cache 
## its inverse
## The cacheSolve function retreives the inverse of the matrix if it has already 
## been computed by makeCacheMatrix. If it has not already been computed, the
## cacheSolve function will compute it.

## The makeCacheMatrix creates a matrix from the given vector, computes
## its inverse, and stores it in a private variable x

makeCacheMatrix <- function(x = matrix()) {
  ## Initialise inv to NULL as the inverse has not been computed yet
  inv <- NULL
  
  ##Set function to a new value for underlying vector
  set <- function(y)
  { 
  ## Set value of x to y within the enclosing environment of the 
  ## makeCacheMatrix function
    x<<-y
    
  ## Reset inv to NULL in order to modify it in the enclosing environment 
  ## of the makeCacheMatrix function 
    inv<<-NULL
  }
  
  ## Get function for underlying vector
  get <-function()
  {
    
  ## Return value of function x
    x
  }
  
  ## Set inverse of x using the solve function
  setinverse <- function(solve)
  {
    
  ## Set inv as solve in the enclosing environment of the makeCacheMatrix function
    inv <<- solve
  }
  
  ## Return the inverse of the matrix 
  getinverse <- function()
  {
    inv
  }
  
  ## Return a public list of functions that are inside the makeCacheMatrix
  ## making them accessible with the $ operator
  ## necessary as they are used in the cacheSolve function
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function retreives the inverse of the matrix if it has already 
## been computed by makeCacheMatrix, and computes it if has not been computed yet

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ## Returns inverse of matrix x if already cached (if it is not null)
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  ## Get underlying matrix if not available yet and store in getmatrix
  getmatrix <- x$get()
  
  ## Compute inverse of the matrix
  inv <- solve(getmatrix, ...)
  
  ## Cache inverse in x
  x$setinverse(inv)
  
  ## Return cached inverse
  inv
}
