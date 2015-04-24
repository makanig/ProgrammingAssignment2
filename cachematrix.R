## cacheMatrix
## Function to cache matrix Inverse computation
##

## makeCacheMatrix: Creates a modified matrix object to cache it's inverse
## It returns a list containing 4 functiond to
##         set the value of the matrix
##         get the value of the matrix
##         set the value of the inverse
##         get the value of the inverse
makeCacheMatrix <- function(x = matrix(...)) {

  mInv <- NULL  ## stored inverse
  set <- function(myMatrix) {
    x <<- myMatrix
    mInv <<- NULL
  }
  get <- function() x
  setInverse <- function(myInverse) mInv <<- myInverse
  getInverse <- function() mInv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve:  Calculates the inverse of the special matrix created with makeCacheMatrix
##    First it checks to see if the inverse is already calculated and if so, returns the cache  value
##    Else it calculates the inverse of the special matrix, sets the cache value and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  myInv <- x$getInverse()
  if(!is.null(myInv)) {
    ##message("getting cached data")
    return(myInv)
  }
  myMat <- x$get()
  myInv <- solve(myMat)
  x$setInverse(myInv)
  myInv
  
}


## Functionality and time test for the cacheMatrix function
matrixTest <- function() {
  
  ## Test functionality
  origMat <- matrix(c(-1, -2, 1, 1), 2,2)
  origInv <- solve(origMat)
  
  x <- makeCacheMatrix(origMat)
  cacheInv <- cacheSolve(x)
  
  if (!identical(origInv, cacheInv)) {
    message("cacheMatrix error - inverse not equal")
  } else {
    message("cacheMatrix solve functionality test pass")
  }
  
  cacheInv2 <- cacheSolve (x)
  if (!identical(cacheInv2, cacheInv)) {
    message("cacheMatrix error - caching not correct")
  } else {
    message ("cacheMatrix cacheTest OK")
  }
  
  
  # Time test
  
  n <- 256
  mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
  matCached <- makeCacheMatrix(mat)
  time1 <- system.time(matSolved1 <- cacheSolve(matCached))
  time2 <- system.time(matSolved2 <- cacheSolve(matCached))
  if (time1["user.self"] < time2["user.self"]) {
    message("Solve time is less than cache time")
  }
  print(sprintf("solveTime : %f  cacheTime: %f", time1["user.self"],time2["user.self"]))
}