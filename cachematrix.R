## "makeCacheMatrix" creates a special matrix and caches it's inverse. 
## "cacheSolve" computes the inverse of the special matrix.

## "makeCacheMatrix" is a function that returns a list of following functions
## set: to set the value of a matrix  
## get: to get the value of a matrix  
## setinv: to set the cached value (inverse of the matrix)  
## getinv: to get the cached value (inverse of the matrix 
## Its purpose is to store a martix and a cached value of the inverse of the matrix.
  
makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <-function(mat){
                    x <<- mat
                    m <<- NULL
              }
              get <- function() x
              setinv <- function(inv) m <<- inv
              getinv <- function() m
              list(set = set, get = get,
                   setinv = setinv,
                   getinv = getinv)
}          


## "cacheSolve" is a function that computes the inverse of the matrix returned by  "makeCacheMatrix".
## If the inverse has already been calculated and the matrix has not been changed,"cacheSolve" retrieves the 
## inverse from the cache so that it doesn't need to compute it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinv()
          if(!is.null(m)){
                  message("getting cached data")
                  return(m)
          }
          data <- x$get()
          m <- solve(data,...)
          x$setinv(m)
          m
}
