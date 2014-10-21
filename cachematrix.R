## This second programming assignment will require you to write an R function that is able 
## to cache potentially time-consuming computations. For example, taking the mean of a numeric 
## vector is typically a fast operation. However, for a very long vector, it may take too long 
## to compute the mean, especially if it has to be computed repeatedly (e.g. in a loop). If the 
## contents of a vector are not changing, it may make sense to cache the value of the mean so 
## that when we need it again, it can be looked up in the cache rather than recomputed. In this 
## Programming Assignment you will take advantage of the scoping rules of the R language and how 
## they can be manipulated to preserve state inside of an R object.

########################
# FUNCTION DEFINITIONS #
########################

##
##  makeVector 
##
## Function makeVector creates a special "vector", which is really a list containing a function 
## to:
##
##    - set the value of the vector
##    - get the value of the vector
##    - set the value of the mean
##    - get the value of the mea
##

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

##
## cachemean
##
## Function cachemean calculates the mean of the special "vector" created with the 
## above function. However, it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates 
## the mean of the data and sets the value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

##
## makeCacheMatrix
## 
## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  # get, set, getinverse
  get <- function() x
  getinverse <- function() {
    data <- x
    h <- solve(data)
    h
  }
  list(set = set, get = get, getinverse = getinverse)
}

##
## cacheSolve
##
## Function cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse from the cache.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    #message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  s
}
