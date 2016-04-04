## matrix inversions are computationally expensive, so if a matrix remains unchanged but we require it's
## inverse many times we could store it in a separate variable and always remember to update that variable when the
## matrix is changed in our code. However it is more elegant to create an object hat stores both the matrix and it's
## inverse (if it has ever been calculated) and automatically recalculate whenever the value is changed.
## 

## this function appears to return a matrix but it actually reurns is a special list of functions similar to a c# class
## or more precisely the c# get; set; accessors on a class variable. as get and set are reserved words when we call 
## ma <- makecacheMatrix and type m a matrix is returned (via the get accessor)
## 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <- inverse
  getinverse <- function() inv
  list(set = set, get=get, setinverse=setinverse, getinverse=getinverse)

}
##cacheSolve call the getinverse and setinverse "methods" of the cacheMatrix Object and if the inverse has not yet been calculated
## it updates it.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}



