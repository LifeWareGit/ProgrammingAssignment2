## SUMMARY:
## --------
## These fuctions provides a capability for storing/"caching" a matrix, as well as
## the capability to store the inverse of that matrix, so that the matrix and it's 
## corresponding inverse, can be retrieved cross subsequent invocations.  Since
## calculating the inverse of a matrix can be computatinally intensive, calculating it once
## and storing it for later reuse, is for more computationally efficient than
## recalculating the inverse of a given matrix every time it's required.  

## FUNCTION SUMMARY: makeCacheMatrix
## ---------------------------------
## The function "makeCacheMatrix" creates a list object wrapper around a matrix object
## and provides the ability to cache/store the matrix object itself as well as it's inverse.
## The "makeCacheMatrix" object exposes several utility methods for using this
## capability:
##
## ARGUMENTS:
## x: The new matrix to be "wrapped" within the cached matrix Object
##
## function set: Allows the user to store a new matrix in the parent context
## function get: Retrieves the stored matrix from the parent parent context
## function setInverse: stores the inverse of a matrix in the parent context
## function getInverse: Retrieves the stored inverse from the parent context
##
## USAGE/TEST:
## 
## Note: The last 2 calls will produce the same inverse.  The first will calculate
## a new inverse while the 2nd call will retrieve it from cache.
##
## > mValues <- runif(1:100,0,1)
## > cachedMatrix <- makeCacheMatrix(mValues,10,10))
## > cacheSolve(cachedMatrix)
## > cacheSolve(cacheMatrix)
##
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the Inverse of the matrix "x" to NULL when creating a new Cached Matrix
  invX <- NULL
  
  ## Method to store a new matrix
  set <- function(new_X) {
    
    ## The "<<-" operator causes the "x" to be set to "newX" 
    ## within the parent context (i.e. makeCacheMatrix).
    x <<- newX
    
    ## Since this is a new matrix, initiialize the inverse back to NULL
    invX <<- NULL
  }
  
  ## Method to retrieve the current matrix
  get <- function() {
    x
  } 
  
  ## Method to set the new invserse of the cached Matrix
  setInverse <- function(new_InvX) {
    
    ## Using the "<<-" operator forces the new Inverse to be stored
    ## within the parent context
    invX <<- new_InvX
  }
  
  ## Method to retrieve the current matrix inverse
  getInverse <- function() {
    invX
  }
  
  ## Return the cached matrix object "wrapped" within a list object
  ## and provide the various functional operations for storing/retrieving 
  ## the matrix and its cooresponding inverse.
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## FUNCTION SUMMARY: cacheSolve
## ----------------------------
## The function "cacheSolve" operates on a special cached matrix object which is created using
## "makeCacheMatrix" function.  It either retrieves a previously stored inverse value, or
## calculates a new inverse if a stored value does not exist, and returns the inverse. 
## 
## ARGUMENTS:
## x: The matrix to calculate/retrieve the inverse for
## ...: The parameters to pass to the "solve" function for calculating th inverse of a matrix
##
## ALGORITHM:
## 1. Retrieve the stored matrix inverse
## 2. If the inverse does not exist (i.e. is NULL), calculate a new inverse and store it
## 3. Return the resulting inverse
##
cacheSolve <- function(x, ...) {
        
  ## Retrieve the current inverse of the passed in matrix
  invX <- x$getInverse()
  
  ## Check to see if the inverse is empty/NULL
  if(is.null(invX)) {
    ## The inverse is NULL so calculate a new one and store it
    message("The retrieved inverse is null so calculating and storing a new one.")
    
    ## Retrieve the currently stored/cached matrix from the parent context
    curMatrix <- x$get()
    
    ## Calculate the inverse of the current matrix
    invX <- solve(curMatrix, ...)
    
    ## Store the inverse for future use
    x$setInverse(invX)
  } else {
      message("A previously stored inverse has been retrieved for this matrix.")
  }
  
  ## Return the inverse
  invX
}
