##########################################
##                                      ##
## Coursera Data Science: R Programming ##
##                                      ##
##    Assignement 2: Lexical Scoping    ##
##                                      ##
##########################################
##                                      ##
##          Created By Ellume           ##
##            July 21, 2015             ##
##                                      ##
##########################################


##########################################
##        Assignment Objective          ##
##########################################
## Write the following functions:
## 1. makeCacheMatrix: This function creates a special
## "matrix" object that can cache its inverse.
##
## 2. cacheSolve: This function computes the inverse of
## the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve should retrieve the
## inverse from the cache.


##########################################
##       Assignment Description         ##
##########################################
##
## I based the two required functions off the provided
## examples which did the same thing except with a vector.
##
## 1. Instead of using vector I used a matrix.
## 2. Instead of caching a mean I cached a solved/inverse matrix


##########################################
##       makeCacheMatrix Function       ##
##########################################
##
## Creates a special "matrix", which is really a list containing a function to:
##
## 1. Set the values of a matrix
## 2. Get the values of a matrix
## 3. Set the values of a solve/inverse of the matrix
## 4. Get the values of a solve/inverse of the matrix
##

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


##########################################
##         cacheSolve Function          ##
##########################################
##
## The following function calculates the solve/inverse of the special "matrix"
## created with the above function. However, it first checks to see
## if the solve/inverse has already been calculated. If so, it gets the solve/inverse
## from the cache and skips the computation. Otherwise, it calculates
## the solve/inverse of the data and sets the values of the solve/inverse in the cache
## via the setsolve function.
##

cacheSolve <- function(x, ...) {
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

##########################################
##  If you are also taking this class   ##
##      then you are AWESOME! :D        ##
##                                      ##
##         Thanks for Grading!          ##
##########################################

