#This file contains two functions 
# 1) makeCacheMatrix
# 2) cacheSolve
#
# makeCacheMatrix - It takes a  matrix as parameter and 
#                   defines four functions to set and get
#                   a matrix and set and get an inverse of 
#                   a matrixc
#
# cacheSolve - it checks whether an inverse of a matrix 
#              has already been calculated and cached.If
#              cached inverse matrix exists, it fetches from
#              the cache, otherwise it calculates the inverse
#              and caches it
            


# makeCacheMatrix - it takes a matrix as parameter and defines 4 functions
#   4 functions
#   1) set - sets a matrix
#   2) get - gets(returns) a matrix
#   3) setInverse - sets inverse of a matrix
#   4) getInverse - gets inverse of a matrix

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(mInput) m <<- mInput
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# cacheSolve - it checks whether an inverse of a matrix has 
#              already been calculated
#              If it already exists, it returns the cached value
#              If it does not exist, it calculates the inverse and
#             cache the inverse matrix  

cacheSolve <- function(x) {
  m <- x$getInverse() # gets cached data
  if(!is.null(m)) { # checks whether cached data exists
    message("getting cached data")
    return(m) # returns cached data
  }
  data <- x$get()
  m <- solve(data) # it calculates the inverse of a matrix x
  x$setInverse(m) # caches the inverse of a matrix
  m
}


