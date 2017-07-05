## In this assignment, write a pair of functions that cache the inverse of a matrix.
## makeCacheMatrix: makes a particular matrix object which caches its inverse matrix
## cacheSolve: If the inverse has been computed earlier, then this function returns the inverse from the cache.
## Otherwise, it gets this matrix from the above function and caculates the inverse.

## This makeCacheMatrix function makes a "matrix", which contains below functions
## set the value of the matrix/ get this value/ set the inverse value/ get this inverse value

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function() i
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve function calculates the inverse of the "matrix" using makeCacheMatrix above.
## If the inverse already exists, then chacheSolve returns the inverse from the cache.

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
    message("receiving cached data")
    return(i)
  }
  dat<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
}
