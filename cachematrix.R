makeCacheMatrix <- function(x = matrix()) {
  ## Resets the matrix
  m <- NULL ##Place holder for future value (matrix )
  invm <-NULL ##Place holder for inverse of m
  
  set <- function(y) {
    ##Set the matrix (x) to a new matrix (y)
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  ## Function that will set the inverse, only if the matrix has changed
  setinverse <- function(matrix) {
    m <<- matrix
  }
  ##Function that will get the inverse
  getinverse <- function() {
    m
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
  
}


## Computes the inverse of a Matrix (from makeCacheMatric)
## Check if the matrix has changed and if an inverse has been calculated
## If changed, then calculate the inverse and cache the result
## If the matrix is still the same, retrieve inverse from Cache
cacheSolve <- function(x, ...) {
  #Get the matrix
  #If matrix is same, then get the cached inverse
  #else recalculte the Inverse and then also cache both the new matrix
  # and the newly calculated inverse
  m <- x$get()
  if(!is.null(m)) {
    message("Getting cached data")
    #matequal <- function(a,b) is.matrix(a) && is.matrix(b) && dim(a) == dim(b) && all(a==b)
    return(m)
  }
  
  print("Do expensive calculation an add cache")
  m <- x
  
  x$set(m)
  
  
  m
}

