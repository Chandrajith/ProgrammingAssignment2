## Assignment 2
## makeCacheMatrix: This function creates a matrix that can cache its inverse.
## set        : initializes the matrix
## get        : returns the matrix created.
## setInverse : computes the inverse of a matrix and caches it.
##              uses solve function in R. If x is a square invertible matrix
##              then solve(x) returns its inverse.
## getInverse : simply return the cached inverse matrix.
## verifyInverse: Helper function to test whether the inverse of the matrix is right
##                i.e X %*% X` = Identity matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function(y) x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  
  verifyInverse <- function()  {
    if (is.null(m)) {
      message("before verification, run CacheSolve to compute Inverse")
    }
    else {
      print("original matrix")
      print(x)
      
      print("inverse matrix")
      print(getInverse())
      
      message("Should get identity matrix")
      # identity matrix
      id <- x %*% getInverse()  # Should get identity matrix
      r <- round(id, 0)         # round to whole numbers with zero decimal
      r                         
    }
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse, 
       verifyInverse = verifyInverse)
}


## The following function calculates the mean of the matrix created with the above function. 
## However, it first checks to see if the inverse  has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse  of the data and sets the value of the inverse
## in the cache via the setInverse function

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return (m)            # gets from the cache, and return
  }
  data <- x$get()
  m <- solve(data, ...)   # compute the inverse of the matrix
  x$setInverse(m)         # put in the cache
  m                       # return the inverted matrix
}

### ##Sample Output:
### 
### >  makeCacheMatrix(matrix(2:5, 2,2))
### > a$get()
###      [,1] [,2]
### [1,]    2    4
### [2,]    3    5
### > a$set(matrix(1:4, 2, 2))
### > a$get()
###      [,1] [,2]
### [1,]    1    3
### [2,]    2    4
### > a$getInverse()
### NULL
### > cacheSolve(a)
###      [,1] [,2]
### [1,]   -2  1.5
### [2,]    1 -0.5
### > cacheSolve(a)
### getting cached data
###      [,1] [,2]
### [1,]   -2  1.5
### [2,]    1 -0.5
### > a$verifyInverse()
### [1] "original matrix"
###      [,1] [,2]
### [1,]    1    3
### [2,]    2    4
### [1] "inverse matrix"
###      [,1] [,2]
### [1,]   -2  1.5
### [2,]    1 -0.5
### Should get identity matrix
###      [,1] [,2]
### [1,]    1    0
### [2,]    0    1
### > 
### 
