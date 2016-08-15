## The functions cache the inverse of a special "matrix"
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
## Initializing the inverse property
 m<-NULL

 ## Method to set the matrix
 set<-function(y){
   x<<-y
   m<<-NULL
 }
 ## Method to get the matrix
 get <- function() {
   ## Returning the matrix
   x
 }
 ## Method to set the inverse of the matrix
 setInverse <- function(inverse) {
   i <<- inverse
 }
 ## Method to get the inverse of the matrix
 getInverse <- function() {
   ## Return the inverse property
   m
 }
 ## Returning a list of the methods
 list(set = set, get = get,
      setInverse = setInverse,
      getInverse = getInverse)



}


## cacheSolve computes the inverse of the special "matrix" returned by the above function

cacheSolve <- function(x, ...) {
        ## Returning a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Returning the inverse if its already set
        if( !is.null(m) ) {
          message("getting cached data")
          return(m)
        }
        ## Getting the matrix from the object
        data <- x$get()
        ## Calculating the inverse using matrix multiplication
        m <- solve(data) %*% data
        ## Setting the inverse to the object
        x$setInverse(m)
        ## Returning the matrix
        m
}
