## A two functions that cache inverse of matrix 


## this function used to create matrix that can cache inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix){
    inv <<- inverseMatrix
  }
  getInverse <- function() inv
  
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)

}


## this function used to compute invesre for matrix that is returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv 
        
}
