## These functions try to compute the inverse of a matrix if the inverse has not been computed yet
## Else it will just retrieve the value from the cache withour recomputing it

##The following function defines an object makeCaheMatrix, which has several methods we can all
##Thy are set, get, setinverse and getinverse respectively

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y){
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(i){
    inverse_matrix <<- i
  }
  getinverse <- function() inverse_matrix
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The following function gets an argument, which is the object makeCaheMatrix. Then it first checks if
## the inverse has already been computed. If so, it will return the inverse without recomputing. 
## Else it will compute and return the inverse.

cacheSolve <- function(x, ...) {
  if (is.null(x$getinverse())){
    matrix <- x$get()
    inverse_matrix <- solve(matrix)
    x$setinverse(inverse_matrix)
    return(inverse_matrix)
  }
  message("getting cached data")
  inverse_matrix <- x$getinverse()
  inverse_matrix
}

