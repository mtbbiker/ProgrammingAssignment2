makeCacheMatrix <- function(x = matrix()) {
  ## Sets the matrix, required to have an initial matrix for calculation
  mat <- x
  invm <-NULL ##Place holder for inverse of m
  
  set <- function(y) {
    ##Set the matrix (x) to a new matrix (y)
    #Compare existing x with new y, if the same then inverse will be already set and used as such
    #If not same then we have to recalculate the inverse
    if(!matequal(mat,y))
    {
      invm <<- NULL 
    }
    mat <<- y
  }
  
  #Function that will test if 2 matrices are equal
  matequal <- function(a,b) {is.matrix(a) && is.matrix(b) && dim(a) == dim(b) && all(a==b)}
  
  get <- function() {
    mat
  }
  
  
  ## Function that will set the inverse, only if the matrix has changed
  setinverse <- function(inverse) {
    
    invm <<- inverse
  }
  ##Function that will get the inverse
  getinverse <- function() {
    
    invm
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}

cacheSolve <- function(x, ...) {
  #Calculate the Inverse.
  #Example to use Function.
  # 1. Setup the first matrix (Only NON singular) to calculate: mymatrix <- matrix(c(1:16),2,2)
  # 2. Setup the Function to do the Matrix caching: cm <- makeCacheMatrix(mymatrix)
  # 3. Solve the Inverse (and cache if not exist): cacheSolve(cm)
  # 4. If other matrix are to be calculated, use set() function: cm$(newmatrix)
  
  ivm <- x$getinverse()
  if(!is.null(ivm)) {
    message("Getting cached data")
    
    return(ivm)
  }
  
  ma <- (x$get())
  mat <-ma
  ivm <- solve(ma)
  x$setinverse(ivm)
  #return the calculted Inverse
  ivm
}