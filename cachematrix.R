## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function returns a closure over a matrix and its inverse.
## This function is used in conjunction with the 'cacheSolve' function
## The inverse is only computed and stored by this 'cacheSolve' function and is initially NULL.
## The result of this function should be stored in a variable for later usage.
##   Usage: mycachematrix <- makeCacheMatrix(matrix)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL                                       # initialise inverse variable

  ## set matrix function
  set <- function(y) {                               
      x <<- y                                           # store the matrix in x
      inverse <<- null                                  # assure an existing inverse is cleared when a new matrix is set
  }
  get <- function() x                                   # get matrix function
  setinverse <- function(solved) inverse <<- solved     # set inverse of matrix function
  getinverse <- function() inverse                      # get inverse of matrix function
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

## This function returns the inverse of a matrix stored in a cachematrix closure 
## generated bij the 'makeCacheMatrix' function and caches the inverse in this closure.
## When the inverse matrix doesn't exist, it is computed and cached, otherwise it retrieved 
## directly from the cache.
##   Usage: cacheSolve(mycachematrix)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()                                 # request te inverse matrix from x
  if (!is.null(inverse)) {                                  # when the result is not null it is
    message("[Inverse of matrix returned from cache]")      #   the inverted matrix and it can
    return(inverse)                                         #   be returned. 
  }
                                                            # otherwise continue with the function
  matrix <- x$get()                                         # retrieve the original matrix
  inverse <- solve(matrix, ...)                             # compute the inverse of the matrix
  x$setinverse(inverse)                                     # cache the inverse to x
  inverse                                                   # return the inverse matrix
}