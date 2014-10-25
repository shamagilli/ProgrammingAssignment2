## Second programming assignment
##
## R function that is able to cache the inverse of a square matrix
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.
##
## To use these pair of functions on a given matrix m:
##  >  cachm <- makeCachematrix(m) # Store the result of the first function
##  >  cacheSolve (m) # Calculates the inverse on first time called
##  >  cacheSolve (m) # Gets cached inverse if already calculated
##
## Assumption: The matrix supplied is always invertible.
##

################ makeCachematrix ########################
## The first function, creates a special matrix object, #
## that can cache its inverse.                          #
## The function argument is a matrix. The function      #
## returns a list cantaining 4 functions:               #
## 1.  set the value of the matrix                      #
## 2.  get the value of the matrix                      #
## 3.  set the value of the inverse matrix (solve)      #
## 4.  get the value of the inverse matrix (cached)     #
#########################################################

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}

################ cacheSolve #############################
## The second function, Return a matrix that is the     #
## inverse of the input vector matrix (that was created #
## on the first function.) The function first checks    #
## if there is already a solved matrix calculated, and  #
## return it. If there is no inverse yet, then it is    #
## calculated and saved (set) in the cache vector.      #
#########################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}


