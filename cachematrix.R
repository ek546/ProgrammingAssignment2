## These functions calculate the inverse of a matrix, storing the value so
## it does not have to be computed repeatedly. 

## Got some details that helped me complete this assignment from
## "https://asitarrives.wordpress.com/2014/10/18/understanding-lexical-scoping
## -in-r-great-guidance-for-community-ta-in-coursera/comment-page-1/#comment-12"

## This function takes an invertible matrix as input and makes a list that
## 1. sets the matrix
## 2. returns the matrix
## 3. sets the inverse
## 4. returns the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve(x)
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the inverse of x has been calculated. If 
## it has, it returns the stored inverse; if not, it calculates the
## inverse and stores this value for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
