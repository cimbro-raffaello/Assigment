## The cacheSolve function calculates the inverse of a matrix
## If the inverse has already been calculated, the cacheSolve function loads
## the value directly from the saved value stored in the makeCacheMatrix function 


## the makeCacheMatrix function stores the inverse of a matrix that was calculated 
## by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
      
      matinv <- NULL # this variable stores the result of inversion
      
      # if the matrix changes, this function sets to NULL the inverse of the matrix  
      set <- function(y) {
            x <<- y
            matinv <<- NULL 
      }
      
      get <- function() x # return the input matrix
      
      setInv <- function(inv) matinv <<- inv # set the inversed matrix
      
      getInv <- function() matinv # return the inverse of the matrix
      
      # return a list that contains these functions that are used by the
      # cacheSolve function
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## The cacheSolve function calculates the inverse of a matrix
## If the inverse was calculated before, the value is retrieved from the cache
## if the matrix changes, the function reverts to NULL the inverse value in the cache and recalculates the new value

cacheSolve <- function(x = matrix(), l, ...) {
      matinv <- l$getInv() # gets the inversed matrix from object x
     
      data <- l$get() # x$get gets the matrix object
      
      #this function evaluates if there is a change in the matrix
      if (identical(data, x) == FALSE) {
            l$set(x) # if the matrix changes set the inverse value to NULL
      }     else {
            if(!is.null(matinv)) { # if the inversion result is there
                  message("getting cached data")
                  return(matinv) # return the calculated inversion
            }
      }
      data <- l$get() # if the matrix does not change, x$get to get the matrix object
      matinv <- solve(data) # the solve function calculates the inverse of the matrix
      l$setInv(matinv) # load the inverse value in the cache of the makeCacheMatrix function
      
      matinv # return the solved result
}

