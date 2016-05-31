## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## These functions are similar to the makeVector and cachemean. 
## The major difference is in the function used to calculate the mean
## I replace it with solve to calculate the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ##This function create a matrix (square matrix) and perform the following
  ##1.set the value of the matrix
  ##2.get the value of the matrix
  ##3.set the value of the inverse
  ##4.get the value of the inverse
  
  INVS <- NULL ## INVS is inverse of the matrix
  
  set <- function(y) {
    x <<- y        # Assign a value using <<- to x and INVS different from the current environment. 
    INVS <<- NULL
  }
  
  get <- function() x ## get x the matrix
  setINVS <- function(solve) INVS <<- solve  ## set the inverse value using solve function
  getINVS <- function() INVS                 ## get the inverse value
  list(set = set, get = get, setINVS = setINVS, getINVS = getINVS)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## get the inverse of the matrix
  INVS <- x$getINVS()
  
  ## if the inverse is already calculated then return the value
  if(!is.null(INVS)) {
    message("getting cached data")
    return(INVS)
  }
  
  ##if the inverse (INVS) is null then calculate the inverse
  data <- x$get()            ## get the matrix
  INVS <- solve(data, ...)   ## calculate the inverse of the matrix using solve function
  x$setINVS(INVS)            ## sets the value of the inverse in the cache using setinv
  return(INVS)               ##return th inverse
  
}
