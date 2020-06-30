## Below functions cache the inverse of a matrix, to avoid repetitive 
## computation when the inverse is needed multiple times

## makeCacheMatrix creates a special matrix, which is a list that contains 
## functions to (1) set the value of the matrix, (2) get the value of the matrix,
## (3) set the value of the inverse matrix and (4) get the value of the inverse
## matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  set.inverse <- function(solve) inv <<- solve
  
  get.inverse <- function() inv
  
  list( set= set,
        get = get,
        set.inverse = set.inverse,
        get.inverse = get.inverse)

}


## cacheSolve calculates the inverse of the special matrix created with the 
## makeCacheMatrix function. If the inverse of the matrix has been calculated 
## previously, it gets the inverse from the cache and does not execute the 
## computation. If not, it calculates the inverse of the matrix and sets the value
## of the inverse in the cache via the set.inverse function

cacheSolve <- function(x, ...) {
  inv <- x$get.inverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data,...)
  x$set.inverse(inv)
        ## Return a matrix that is the inverse of 'x'
  inv
}
