# Coursera - R Programming Assignment 2
# 
# Based off examples from the assignment
# https://github.com/chuckleb/ProgrammingAssignment2
# 

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. There are
# other options to do this but we will focus on this to teach the <<- operator. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed and that the matrix has not changed. If so,
# it gets the result and skips the computation. If not, it computes the inverse, 
# sets the value in the cache via setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {

  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Inverse value cached, getting from cache.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
