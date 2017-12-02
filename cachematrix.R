## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function will create a "special" matrix and do the following 
#setting value of matrix > getting value of matrix > 
#setting the value of inverse matrix > getting the value of inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) { #setting value of matrix
      x <<- y
      invMatrix <<- NULL
  }
  getMatrix <- function() x #getting matrix
  setInverse <- function(inverse) invMatrix <<- inverse #setting value of matrix inverse
  getInverse <- function() invMatrix #getting value of matrix inverse
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

#This function will compute for the inverse of the matrix from above
#If not yet computed, it will compute the inverse, then will return the results
#If computed, it will already return the computations

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)){
      message("getting cache")
      return(invMatrix)
  }
  data <- x$getMatrix()
  invMatrix <- solve(data, ...)
  x$setInverse(invMatrix)
  return(invMatrix)
}

##Try example:
x = rbind(c(4,4,4), c(0,6,9), c(5,5,1))
m = makeCacheMatrix(x)
m$getMatrix()
cacheSolve(m)
