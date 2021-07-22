## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix = create and calculate inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL ## sets inverse to Null
  set <- function(y){
    x<<- y
    inv<<- NULL
  }
  get <- function() {x} ##gets the matrix that was created
  setInverse<- function(inverse) {inv<<- inverse}
  getInverse<- function() {inv}
  list(set=set, get=get, setInverse = setInverse, getInverse=getInverse)
}

## Write a short comment describing this function
## cachesolve = Used to get the cached data
cacheSolve <- function(x, ...) {
  inv<- x$getInverse()
  if(!is.null(inv)){ ##is inverse null? if yes = solve, if not = get cached inverse.
    message("getting cached data")
    return(inv)  ## Returns a matrix that is the inverse of 'x'
  }
  mat<-x$get()
  inv<- solve(mat, ...)
  x$setInverse(inverse)
  inv
}
